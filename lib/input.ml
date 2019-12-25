type context = Utf8.context

type infos =
  { utf8         : context(** Uses utf8 for positions                 *)
  ; name         : string (** The name of the buffer (e.g. file name) *)
  ; uid          : int    (** Unique identifier                       *)
  ; rescan       : 'a. (int -> char -> 'a -> 'a) -> int -> 'a -> 'a
  ; lnum_skip    : (int * int) list
  }

type buffer_aux =
  { boff         : int    (* Offset to the line ( bytes )            *)
  ; data         : string (* Contents of the buffer                  *)
  ; mutable next : buffer (* Following line                          *)
  ; mutable ctnr : Container.t option array
                          (* for map table, initialized if used      *)
  ; infos        : infos  (* infos common to the whole file          *)
  }

and buffer = buffer_aux Lazy.t

type idx = int

type byte_pos = int

type spos = infos * byte_pos

let int_of_byte_pos x = x

let init_idx = 0

let infos (lazy b) = b.infos

(* Generate a unique identifier. *)
let new_uid =
  let c = ref 0 in
  fun () -> let uid = !c in incr c; uid

(* Emtpy buffer. *)
let empty_buffer infos boff =
  let rec line = lazy
    { boff; data = "" ; next = line ; infos ; ctnr = [||] }
  in line

let is_eof b = b.data = ""

let llen b = String.length b.data

(* Test if a buffer is empty. *)
let rec is_empty (lazy l) idx =
  if idx < llen l then false
  else if idx = 0 then is_eof l
  else is_empty l.next (idx - llen l)

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
let filename infos = infos.name

(* byte position *)
let byte_pos (lazy b) p = b.boff + p

let spos (lazy b) p = (b.infos, b.boff + p)

let phantom_infos =
  { utf8 = Utf8.ASCII
  ; name = ""
  ; uid = new_uid ()
  ; rescan = (fun _ -> assert false)
  ; lnum_skip = [] }
let phantom_byte_pos = -1
let phantom_spos = (phantom_infos, phantom_byte_pos)

(* Get the current line number of a buffer. *)
let line_num infos i0 =
  let fn i c (l,ls) =
    match ls with
    | (j,l)::ls when i = j -> (l, ls)
    | _ -> if c = '\n' then (l+1, ls) else (l,ls)
  in
  let lnum_skip = List.rev infos.lnum_skip in
  fst (infos.rescan fn i0 (1,lnum_skip))

(* Get the current line number of a buffer. *)
let ascii_col_num infos i0 =
  let fn _ c p = if c = '\n' then 0 else p+1 in
  infos.rescan fn i0 0

(* Get the utf8 column number corresponding to the given position. *)
let utf8_len context data =
  Printf.printf "%S\n%!" data;
  let len = String.length data in
  let rec find num pos =
    if pos < len then
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

let utf8_col_num infos i0 =
  let fn _ c cs = if c = '\n' then [] else c::cs in
  let cs = infos.rescan fn i0 [] in
  let len = List.length cs in
  let str = Bytes.create len in
  Printf.printf "coucou 1\n%!";
  let rec fn cs p =
    match cs with
    | [] -> assert (p=0); ()
    | c::cs -> let p = p - 1 in
               Bytes.set str p c;
               fn cs p
  in
  fn cs len;
  Printf.printf "coucou 2\n%!";
  utf8_len infos.utf8 (Bytes.unsafe_to_string str)

let col_num infos i0 =
  if infos.utf8 = ASCII then ascii_col_num infos i0 else utf8_col_num infos i0

(* Ensure that the given position is in the current line. *)
let rec normalize (lazy b as str) idx =
  if idx >= llen b then
    if is_eof b then (str, 0)
    else normalize b.next (idx - llen b)
  else (str, idx)

(* Equality of buffers. *)
let buffer_equal (lazy b1) (lazy b2) =
  b1.infos.uid = b2.infos.uid && b1.boff = b2.boff

(* Comparison of buffers. *)
let buffer_compare (lazy b1) (lazy b2) =
  match b1.boff - b2.boff with
  | 0 -> b1.infos.uid - b2.infos.uid
  | c -> c

(* Get the unique identifier of the buffer. *)
let buffer_uid (lazy buf) = buf.infos.uid

type scanner = { f : 'a. (byte_pos -> char -> 'a -> 'a) -> idx -> 'a -> 'a }

module type MinimalInput =
  sig
    val from_fun : ('a -> unit) -> context -> string
                   -> ('a -> string) -> ('a -> scanner) -> 'a -> buffer
  end

let buf_size = 0x10000

(* returns [(s,nl)] with [nl = true] iff there is a newline at the end of [s] *)
let input_buffer ch =
  let res = Bytes.create buf_size in
  let n = input ch res 0 buf_size in
  if n = 0 then (* n = 0: we are at EOF *)
    raise End_of_file
  else if n = buf_size then
    Bytes.unsafe_to_string res
  else
    Bytes.sub_string res 0 n

let input_rescan ch =
  { f = fun fn i0 acc ->
          let pos = pos_in ch in
          seek_in ch 0;
          let acc = ref acc in
          for i = 0 to i0 - 1 do
            acc := fn i (input_char ch) !acc
          done;
          seek_in ch pos;
          !acc }

let fd_buffer fd =
  let res = Bytes.create buf_size in
  let n = Unix.read fd res 0 buf_size in
  if n = 0 then (* n = 0: we are at EOF *)
    raise End_of_file
  else if n = buf_size then
    Bytes.unsafe_to_string res
  else
    Bytes.sub_string res 0 n

let fd_rescan fd =
  let ch = Unix.in_channel_of_descr fd in
  input_rescan ch

module GenericInput(M : MinimalInput) =
  struct
    include M

    let from_channel
        : ?utf8:context -> ?filename:string -> in_channel -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") ch ->
        from_fun ignore utf8 filename input_buffer input_rescan ch

    let from_fd
        : ?utf8:context -> ?filename:string -> Unix.file_descr -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") fd ->
        from_fun ignore utf8 filename fd_buffer fd_rescan fd

    let from_file : ?utf8:context -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) fname ->
        let fd = Unix.(openfile fname [O_RDONLY] 0) in
        from_fun Unix.close utf8 fname fd_buffer fd_rescan fd

    let from_string : ?utf8:context -> ?filename:string -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") str ->
        let string_buffer =
          let b = ref true in
          fun () -> if !b then (b := false; str) else raise End_of_file
        in
        let rescan () = { f = fun fn i0 acc ->
          let acc = ref acc in
          for i = 0 to i0 - 1 do
            acc := fn i str.[i] !acc
          done;
          !acc}
        in
        from_fun ignore utf8 filename string_buffer rescan ()
  end

include GenericInput(
  struct
    let from_fun finalise utf8 name get_line rescan file =
      let rescan fn i0 acc = (rescan file).f fn i0 acc in
      let infos = { utf8; name; uid = new_uid (); rescan; lnum_skip = [] } in
      let rec fn remain boff cont =
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let data0 =
              try get_line file
              with End_of_file when remain <> ""  -> ""
            in
            let data = if remain <> "" then remain ^ data0 else data0 in
            let llen = String.length data in
            let (remain,data,llen) =
              if data0 <> "" && infos.utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                if p > 0 then
                  (String.sub data p (llen - p), String.sub data 0 p, p)
                else ("",data, llen)
              else ("",data, llen)
            in
            fun () ->
              { boff; data ; infos
              ; next = lazy (fn remain (boff + llen) cont)
              ; ctnr = [||] }
          with End_of_file ->
            finalise file;
            fun () -> cont boff
        end ()
      in
      lazy
        begin
          let cont boff =
            Lazy.force (empty_buffer infos boff)
          in
          fn "" 0 cont
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
    val update : state -> string -> string
                 -> state * (string option * int option * string) list
    val check_final : state -> string -> unit
  end

module Make(PP : Preprocessor) =
  struct
    let from_fun finalise utf8 name get_line rescan file =
      let rescan fn i0 acc = (rescan file).f fn i0 acc in
      let infos = { utf8; name; uid = new_uid (); rescan = rescan
                    ; lnum_skip = [] }
      in
      let rec fn remain infos boff st cont =
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let data0 =
              try get_line file
              with End_of_file when remain <> ""  -> ""
            in
            let data = if remain <> "" then remain ^ data0 else data0 in
            let llen = String.length data in
            let (remain,data) =
              if data0 <> "" && infos.utf8 <> Utf8.ASCII then
                let p = Utf8.prev_grapheme data llen in
                if p > 0 then
                  (String.sub data p (llen-p), String.sub data 0 p)
                else ("",data)
              else ("",data)
            in
            let (st, ls) = PP.update st infos.name data in
            let rec gn infos boff = function
              | [] -> fn remain infos boff st cont
              | (name, lnum, data) :: ls ->
                 let infos = match name with
                   | None      -> infos
                   | Some name -> { infos with name }
                 in
                 let infos = match lnum with
                   | None   -> infos
                   | Some l ->
                      { infos with lnum_skip = (boff, l) :: infos.lnum_skip }
                 in
                 if data = "" then gn infos boff ls
                 else
                   let llen = String.length data in
                   { boff; data ; infos
                     ; next = lazy (gn infos (boff + llen) ls)
                     ; ctnr = [||] }
            in
            fun () -> gn infos boff ls
          with End_of_file ->
            finalise file;
            fun () -> cont infos boff st
        end ()
      in
      lazy
        begin
          let cont infos boff st =
            PP.check_final st infos.name;
            Lazy.force (empty_buffer infos boff)
          in
          fn "" infos 0 PP.initial_state cont
        end
  end

module WithPP(PP : Preprocessor) = GenericInput(Make(PP))

let leq_buf {boff = b1} i1 {boff = b2} i2 =
  b1 < b2 || (b1 = b2 && (i1 <= i2))

let buffer_before b1 i1 b2 i2 = leq_buf (Lazy.force b1) i1 (Lazy.force b2) i2

(** Table to associate value to positions in input buffers *)
module Tbl = struct
  type 'a t = 'a Container.table

  let create = Container.create_table

  let ctnr buf idx =
    if buf.ctnr = [||] then
      buf.ctnr <- Array.make (llen buf + 1) None;
    let a = buf.ctnr.(idx) in
    match a with
    | None -> let c = Container.create () in buf.ctnr.(idx) <- Some c; c
    | Some c -> c

  let add tbl buf idx x =
    let buf = Lazy.force buf in
    Container.add tbl (ctnr buf idx) x

  let find tbl buf idx =
    let buf = Lazy.force buf in
    Container.find tbl (ctnr buf idx)

  let clear = Container.clear

  let iter : type a. a t -> (a -> unit) -> unit = fun tbl f ->
    Container.iter f tbl

end
