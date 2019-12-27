type context = Utf8.context

type infos =
  { utf8         : context(* Uses utf8 for positions                 *)
  ; name         : string (* The name of the buffer (e.g. file name) *)
  ; uid          : int    (* Unique identifier                       *)
  }

type line =
  { lnum         : int    (* Line number (startig at 1)              *)
  ; boff         : int    (* Offset to the line ( bytes )            *)
  ; coff         : int    (* Offset to the column ( bytes or utf8 )  *)
  ; data         : string (* Contents of the buffer                  *)
  ; mutable next : buffer (* Following line                          *)
  ; mutable ctnr : Container.t option array
                          (* for map table, initialized if used      *)
  ; infos        : infos  (* infos common to the whole file          *)
  }

and buffer = line Lazy.t

and pos = int

let init_pos = 0

(* Generate a unique identifier. *)
let new_uid =
  let c = ref 0 in
  fun () -> let uid = !c in incr c; uid

(* Emtpy buffer. *)
let empty_buffer infos lnum boff coff =
  let rec line = lazy
    { lnum ; boff; coff; data = "" ; next = line ; infos ; ctnr = [||] }
  in line

let is_eof b = b.data = ""

let llen b = String.length b.data

(* Test if a buffer is empty. *)
let rec is_empty (lazy l) pos =
  if pos < llen l then false
  else if pos = 0 then is_eof l
  else is_empty l.next (pos - llen l)

(* Read the character at the given position in the given buffer. *)
let rec read (lazy l as b) i =
  match compare (i+1) (llen l) with
  | -1 -> (l.data.[i], b     , i+1)
  | 0  -> (l.data.[i], l.next, 0  )
  | _  -> if is_eof l then ('\255', b, 0) else read l.next (i - llen l)

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
  if i < llen l then l.data.[i] else
  if is_eof l then '\255' else
  get l.next (i - llen l)

(* Get the name of a buffer. *)
let filename (lazy b) = b.infos.name

(* Get the current line number of a buffer. *)
let line_num (lazy b) = b.lnum

let byte_pos (lazy b) p = b.boff + p

exception Splitted_end
exception Splitted_begin

(* Get the utf8 column number corresponding to the given position. *)
let utf8_col_num context data i =
  let rec find num pos =
    if pos < i then
      let cc = Char.code data.[pos] in
      let code i =
        if (pos+i) >= String.length data then raise Splitted_end;
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
        | _ -> raise Splitted_begin
        in
        Uchar.of_int n
      in
      let (num, pos) =
        try
          if cc lsr 7 = 0 then
            (num+Utf8.width ~context (code 1), pos + 1)
          else if (cc lsr 5) land 1 = 0 then
            (num+Utf8.width ~context (code 2), pos + 2)
          else if (cc lsr 4) land 1 = 0 then
            (num+Utf8.width ~context (code 3), pos + 3)
          else if (cc lsr 3) land 1 = 0 then
            (num+Utf8.width ~context (code 4), pos + 4)
          else (num, pos+1) (* Invalid utf8 character. *)
        with Splitted_begin -> (num, pos+1)
           | Splitted_end   -> (num+1, String.length data)
      in
      find num pos
    else num
  in find 0 0

let utf8_len context data = utf8_col_num context data (String.length data)

let col_num (lazy b) p =
  if b.infos.utf8 <> ASCII then b.coff + utf8_col_num b.infos.utf8 b.data p
  else b.coff + p

module Pos = struct
  type ipos = pos
  (** Type to represent position *)
  type pos = { name : string  (** file's name *)
             ; line  : int    (** line number *)
             ; col   : int    (** column number *)
             ; phantom : bool (** is the postion a "phantom", i.e. not really
                                  in the file *) }

  (** build a position from an input buffer and a column number *)

  let get_pos : buffer -> ipos -> pos Lazy.t  = fun (lazy b) p ->
    lazy (let infos = b.infos in
          let line = b.lnum in
          let coff = b.coff in
          let data = b.data in
          let col =
            if infos.utf8 <> ASCII then coff + utf8_col_num infos.utf8 data p
            else coff + p
          in
          let name = infos.name in
          { name ; line ; col = col ; phantom = false })

end

(* Ensure that the given position is in the current line. *)
let rec normalize (lazy b as str) pos =
  if pos >= llen b then
    if is_eof b then (str, 0)
    else normalize b.next (pos - llen b)
  else (str, pos)

(* Equality of buffers. *)
let buffer_equal (lazy b1) (lazy b2) =
  b1.infos.uid = b2.infos.uid && b1.lnum = b2.lnum && b1.coff = b2.coff

(* Comparison of buffers. *)
let buffer_compare (lazy b1) (lazy b2) =
  match b1.lnum - b2.lnum with
  | 0 -> (match b1.coff - b2.coff with
          | 0 -> b1.infos.uid - b2.infos.uid
          | c -> c)
  | c -> c

(* Get the unique identifier of the buffer. *)
let buffer_uid (lazy buf) = buf.infos.uid

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
      let infos = { utf8; name; uid = new_uid () } in
      let rec fn remain lnum boff coff cont =
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
              if not nl && data0 <> "" && infos.utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                if p > 0 then
                  (String.sub data p (llen - p), String.sub data 0 p, p)
                else ("",data, llen)
              else ("",data, llen)
            in
            let nlnum, ncoff =
              if nl then (lnum+1, 0)
              else
                let len =
                  if infos.utf8 <> Utf8.ASCII then utf8_len infos.utf8 data
                  else llen
                in (lnum, coff + len)
            in
            fun () ->
              { lnum ; boff; coff; data ; infos
              ; next = lazy (fn remain nlnum (boff + llen) ncoff cont)
              ; ctnr = [||] }
          with End_of_file ->
            finalise file;
            fun () -> cont lnum boff coff
        end ()
      in
      lazy
        begin
          let cont lnum boff coff =
            Lazy.force (empty_buffer infos lnum boff coff)
          in
          fn "" 1 0 0 cont
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
      let infos = { utf8; name; uid = new_uid () } in
      let rec fn remain infos lnum boff coff st cont =
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
              if not nl && data0 <> "" && infos.utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                if p > 0 then
                  (String.sub data p (llen-p), String.sub data 0 p)
                else ("",data)
              else ("",data)
            in
            let (st, name, lnum, res) = PP.update st infos.name lnum data nl in
            let infos =
              if name <> infos.name then { infos with name } else infos
            in
            match res with
            | Some data ->
               let llen = String.length data in
               let nlnum, ncoff =
                 if nl then (lnum+1, 0)
                 else
                   let len =
                     if infos.utf8 <> Utf8.ASCII then utf8_len infos.utf8 data
                     else llen
                   in
                   (lnum, coff + len)
               in
              fun () ->
                { lnum ; boff; coff; data ; infos
                ; next = lazy (fn remain infos nlnum (boff + llen)
                                 ncoff st cont)
                ; ctnr = [||] }
            | None ->
              fun () -> fn remain infos lnum boff coff st cont
          with End_of_file ->
            finalise file;
            fun () -> cont infos (lnum+1) boff coff st
        end ()
      in
      lazy
        begin
          let cont infos lnum boff coff st =
            PP.check_final st infos.name;
            Lazy.force (empty_buffer infos lnum boff coff)
          in
          fn "" infos 1 0 0 PP.initial_state cont
        end
  end

module WithPP(PP : Preprocessor) = GenericInput(Make(PP))

let leq_buf {lnum = l1; coff = c1} i1 {lnum =l2; coff = c2} i2 =
  l1 < l2 ||
    (l1 = l2 && (c1 < c2 || (c1 = c2 && i1 <= i2)))

let buffer_before b1 i1 b2 i2 = leq_buf (Lazy.force b1) i1 (Lazy.force b2) i2

(** Table to associate value to positions in input buffers *)
module Tbl = struct
  type 'a t = 'a Container.table

  let create = Container.create_table

  let ctnr buf pos =
    if buf.ctnr = [||] then
      buf.ctnr <- Array.make (llen buf + 1) None;
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
