type context = Utf8.context

type line =
  { is_eof       : bool   (* Has the end of the buffer been reached? *)
  ; lnum         : int    (* Line number (startig at 1)              *)
  ; loff         : int    (* Offset to the line ( utf8 or bytes )    *)
  ; boff         : int    (* Offset to the line ( bytes )            *)
  ; coff         : int    (* Offset to the column ( utf8 or bytes )  *)
  ; llen         : int    (* Length of the buffer                    *)
  ; data         : string (* Contents of the buffer                  *)
  ; mutable next : buffer (* Following line                          *)
  ; name         : string (* The name of the buffer (e.g. file name) *)
  ; utf8         : context(* Uses utf8 for positions                 *)
  ; uid          : int    (* Unique identifier                       *)
  ; mutable ctnr : Container.t option array}
                          (* for map table, initialized if used      *)
and buffer = line Lazy.t

and pos = int

let init_pos = 0

(* Generate a unique identifier. *)
let new_uid =
  let c = ref 0 in
  fun () -> let uid = !c in incr c; uid

(* Emtpy buffer. *)
let empty_buffer name lnum loff boff =
  let rec line = lazy
    { is_eof = true ; name ; lnum ; loff ; boff; llen = 0; coff = 0
    ; data = "" ; next = line ; uid = new_uid (); utf8 = ASCII
    ; ctnr = [||] }
  in line

(* Test if a buffer is empty. *)
let rec is_empty (lazy l) pos =
  if pos < l.llen then false
  else if pos = 0 then l.is_eof
  else is_empty l.next (pos - l.llen)

(* Read the character at the given position in the given buffer. *)
let rec read (lazy l as b) i =
  if l.is_eof then ('\255', b, 0) else
  match compare (i+1) l.llen with
  | -1 -> (l.data.[i], b     , i+1)
  | 0  -> (l.data.[i], l.next, 0  )
  | _  -> read l.next (i - l.llen)

let sub b i len =
  let s = Bytes.create len in
  let rec fn b i j =
    if j = len then Bytes.unsafe_to_string s
    else
      let (c,b,i) = read b i in
      Bytes.set s j c;
      fn b i (j+1)
  in
  fn b i 0

(* Get the character at the given position in the given buffer. *)
let rec get (lazy l) i =
  if l.is_eof then '\255' else
  if i < l.llen then l.data.[i]
  else get l.next (i - l.llen)

(* Get the name of a buffer. *)
let filename (lazy b) = b.name

(* Get the current line number of a buffer. *)
let line_num (lazy b) = b.lnum

(* Get the offset of the current line in the full buffer. *)
let line_offset (lazy b) = b.loff

let byte_pos (lazy b) p = b.boff + p

(* Get the utf8 column number corresponding to the given position. *)
let utf8_col_num context data i =
  let rec find num pos =
    if pos < i then
      let cc = Char.code data.[pos] in
      let code i =
        let n = match i with
          1 -> cc land 0b0111_1111
        | 2 -> (cc land (0b0001_1111) lsl 6) lor
                 (Char.code data.[pos+1] land 0b0011_1111)
        | 3 -> (cc land (0b0000_1111) lsl 12) lor
                 ((Char.code data.[pos+1] land 0b0011_1111) lsl 6)  lor
                   (Char.code data.[pos+2] land 0b0011_1111)
        | 4 -> (cc land (0b0000_0111) lsl 18) lor
                 ((Char.code data.[pos+1] land 0b0011_1111) lsl 12) lor
                   ((Char.code data.[pos+2] land 0b0011_1111) lsl 6)  lor
                     (Char.code data.[pos+3] land 0b0011_1111)
        | _ -> assert false
        in
        Uchar.of_int n
      in
      if cc lsr 7 = 0 then
        find (num+Utf8.width ~context (code 1)) (pos + 1)
      else if (cc lsr 5) land 1 = 0 then
          find (num+Utf8.width ~context (code 2)) (pos + 2)
      else if (cc lsr 4) land 1 = 0 then
        find (num+Utf8.width ~context (code 3)) (pos + 3)
      else if (cc lsr 3) land 1 = 0 then
        find (num+Utf8.width ~context (code 4)) (pos + 4)
      else
      -0 (* Invalid utf8 character. *)
    else num
  in find 0 0

let utf8_len context data = utf8_col_num context data (String.length data)

let col_num (lazy b) p =
  if b.utf8 <> ASCII then b.coff + utf8_col_num b.utf8 b.data p
    else b.coff + p

let char_pos (lazy b) p =
  if b.utf8 <> ASCII then b.loff + utf8_col_num b.utf8 b.data p
    else b.loff + p

(* Ensure that the given position is in the current line. *)
let rec normalize (lazy b as str) pos =
  if pos >= b.llen then
    if b.is_eof then (str, 0)
    else normalize b.next (pos - b.llen)
  else (str, pos)

(* Equality of buffers. *)
let buffer_equal (lazy b1) (lazy b2) = b1.uid = b2.uid

(* Comparison of buffers. *)
let buffer_compare (lazy b1) (lazy b2) = b1.uid - b2.uid

(* Get the unique identifier of the buffer. *)
let buffer_uid (lazy buf) = buf.uid

module type MinimalInput =
  sig
    val from_fun : ('a -> unit) -> context -> string
                   -> ('a -> (string * bool)) -> 'a -> buffer
  end

(* The following code has been borrowed from OCaml's “pervasives.ml” file of
   the standard library. This version preserves the newline in the output
   and may split long lines (> 64Ko). *)
external unsafe_input : in_channel -> bytes -> int -> int -> int =
  "caml_ml_input"

external input_scan_line : in_channel -> int =
  "caml_ml_input_scan_line"

(* returns [(s,nl)] with [nl = true] iff there is a newline at the end of [s] *)
let input_line ch =
  let n = input_scan_line ch in
  if n = 0 then (* n = 0: we are at EOF *)
    raise End_of_file
  else if n > 0 then (* n > 0: newline found in buffer *)
    begin
      let res = Bytes.create n in
      let _ = unsafe_input ch res 0 n in
      (Bytes.unsafe_to_string res, true)
    end
  else (* n < 0: newline not found *)
    begin
      let res = Bytes.create (-n) in
      ignore(unsafe_input ch res 0 (-n));
      (Bytes.unsafe_to_string res, false)
    end

module GenericInput(M : MinimalInput) =
  struct
    include M

    let from_channel
        : ?utf8:context -> ?filename:string -> in_channel -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") ch ->
        from_fun ignore utf8 filename input_line ch

    let from_file : ?utf8:context -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) fname ->
        from_fun close_in utf8 fname input_line (open_in fname)

    let from_string : ?utf8:context -> ?filename:string -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") str ->
          let size = String.length str in
          let get_string_line (str, p) =
            let start = !p in
            if start >= size then raise End_of_file;
            let lim = min size (start + 65536) in
            while (!p < lim && str.[!p] <> '\n') do
              incr p
            done;
            let nl = if !p < lim && str.[!p] = '\n' then
                       (incr p; true) else false
            in
            let pos' = !p - start in
            (String.sub str start pos', nl)
          in
          from_fun ignore utf8 filename get_string_line (str, ref 0)
  end

include GenericInput(
  struct
    let from_fun finalise utf8 name get_line file =
      let rec fn remain name lnum loff boff coff cont =
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let (data0, nl) =
              try get_line file
              with End_of_file when remain <> ""  -> ("", false)
            in
            let data = if remain <> "" then remain ^ data0 else data0 in
            let llen = String.length data in
            let (remain,data,llen) =
              if not nl && data0 <> "" && utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                (String.sub data p (llen - p), String.sub data 0 p, p)
              else
                ("",data, llen)
            in
            let len = if utf8 <> Utf8.ASCII then utf8_len utf8 data else llen in
            let nlnum, ncoff = if nl then (lnum+1, 0) else (lnum, coff + len) in
            fun () ->
              { is_eof = false ; lnum ; loff ; boff; coff; llen ; data ; name
              ; next = lazy (fn remain name nlnum (loff + len)
                                           (boff + llen) ncoff cont)
              ; utf8 ; uid = new_uid () ; ctnr = [||] }
          with End_of_file ->
            finalise file;
            fun () -> cont name (lnum+1) loff boff
        end ()
      in
      lazy
        begin
          let cont name lnum loff boff =
            Lazy.force (empty_buffer name lnum loff boff)
          in
          fn "" name 1 0 0 0 cont
        end
  end)

(* Exception to be raised on errors in custom preprocessors. *)
exception Preprocessor_error of string * string
let pp_error : type a. string -> string -> a =
  fun name msg -> raise (Preprocessor_error (name, msg))

module type Preprocessor =
  sig
    type state
    val initial_state : state
    val update : state -> string -> int -> string -> bool
                 -> state * string * int * string option
    val check_final : state -> string -> unit
  end

module Make(PP : Preprocessor) =
  struct
    let from_fun finalise utf8 name get_line file =
      let rec fn remain name lnum loff boff coff st cont =
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let (data0, nl) =
              try get_line file
              with End_of_file when remain <> ""  -> ("", false)
            in
            let data = if remain <> "" then remain ^ data0 else data0 in
            let llen = String.length data in
            let (remain,data) =
              if not nl && data0 <> "" && utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                (String.sub data p (llen-p), String.sub data 0 p)
              else
                ("",data)
            in
            let (st, name, lnum, res) = PP.update st name lnum data nl in
            match res with
            | Some data ->
               let llen = String.length data in
               let len =
                 if utf8 <> Utf8.ASCII then utf8_len utf8 data else llen
               in
               let (nlnum, ncoff) =
                 if nl then (lnum+1, 0) else (lnum, coff+len)
               in
              fun () ->
                { is_eof = false ; lnum ; loff ; boff; coff; llen ; data ; name
                ; next = lazy (fn remain name nlnum (loff + len) (boff + llen)
                                 ncoff st cont)
                ; utf8 ; uid = new_uid () ; ctnr = [||] }
            | None ->
              fun () -> fn remain name lnum loff boff coff st cont
          with End_of_file ->
            finalise file;
            fun () -> cont name (lnum+1) loff boff st
        end ()
      in
      lazy
        begin
          let cont name lnum loff boff st =
            PP.check_final st name;
            Lazy.force (empty_buffer name lnum loff boff)
          in
          fn "" name 1 0 0 0 PP.initial_state cont
        end
  end

module WithPP(PP : Preprocessor) = GenericInput(Make(PP))

let leq_buf {uid=ident1} i1 {uid=ident2} i2 =
  (ident1 = ident2 && i1 <= i2) || ident1 < ident2

let buffer_before b1 i1 b2 i2 = leq_buf (Lazy.force b1) i1 (Lazy.force b2) i2

(** Table to associate value to positions in input buffers *)
module Tbl = struct
  type 'a t = 'a Container.table

  let create = Container.create_table

  let ctnr buf pos =
    if buf.ctnr = [||] then
      buf.ctnr <- Array.make (buf.llen + 1) None;
    let a = buf.ctnr.(pos) in
    match a with
    | None -> let c = Container.create () in buf.ctnr.(pos) <- Some c; c
    | Some c -> c

  let add tbl buf pos x =
    let buf = Lazy.force buf in
    Container.add tbl (ctnr buf pos) x

  let find tbl buf pos =
    let buf = Lazy.force buf in
    Container.find tbl (ctnr buf pos)

  let clear = Container.clear

  let iter : type a. a t -> (a -> unit) -> unit = fun tbl f ->
    Container.iter f tbl

end
