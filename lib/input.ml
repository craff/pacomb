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
  ; data         : Rope.t (* Contents of the buffer                  *)
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
let empty_buffer infos lnum boff =
  let rec line =
    lazy
      { lnum ; boff; coff = 0; data = Rope.Empty
      ; next = line ; infos ; ctnr = [||] }
  in line

let is_eof b = b == Lazy.force b.next

let llen b = Rope.length b.data

(* Test if a buffer is empty. *)
let rec is_empty (lazy l) pos =
  if pos < llen l then false
  else if pos = 0 then is_eof l
  else is_empty l.next (pos - llen l)

(* Read the character at the given position in the given buffer. *)
let rec read (lazy l as b) i =
  if is_eof l then ('\255', b, 0) else
  match compare (i+1) (llen l) with
  | -1 -> (Rope.get_char l.data i, b     , i+1)
  | 0  -> (Rope.get_char l.data i, l.next, 0  )
  | _  -> read l.next (i - llen l)

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
  if is_eof l then '\255' else
  if i < llen l then Rope.get_char l.data i
  else get l.next (i - llen l)

(* Get the name of a buffer. *)
let filename (lazy b) = b.infos.name

(* Get the current line number of a buffer. *)
let line_num (lazy b) = b.lnum

let byte_pos (lazy b) p = b.boff + p

(* Get the utf8 column number corresponding to the given position. *)
let utf8_col_num context (data:Rope.t) i =
  let rec find num pos =
    if pos < i then
      let cc = Char.code (Rope.get_char data pos) in
      let code i =
        let n = match i with
          1 -> cc land 0b0111_1111
        | 2 -> (cc land (0b0001_1111) lsl 6) lor
                 (Char.code (Rope.get_char data (pos+1)) land 0b0011_1111)
        | 3 -> (cc land (0b0000_1111) lsl 12) lor
                 ((Char.code (Rope.get_char data (pos+1)) land 0b0011_1111) lsl 6)  lor
                   (Char.code (Rope.get_char data (pos+2)) land 0b0011_1111)
        | 4 -> (cc land (0b0000_0111) lsl 18) lor
                 ((Char.code (Rope.get_char data (pos+1)) land 0b0011_1111) lsl 12) lor
                   ((Char.code (Rope.get_char data (pos+2)) land 0b0011_1111) lsl 6)  lor
                     (Char.code (Rope.get_char data (pos+3)) land 0b0011_1111)
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

let utf8_len context data = utf8_col_num context data (Rope.length data)

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

  let get_pos_aux infos line coff data p =
    lazy (let col =
      if infos.utf8 <> ASCII then coff + utf8_col_num infos.utf8 data p
      else coff + p
    in
    let name = infos.name in
    { name ; line ; col = col ; phantom = false })

  let get_pos : buffer -> ipos -> pos Lazy.t  = fun (lazy b) p ->
    let infos = b.infos in
    let line = b.lnum in
    let coff = b.coff in
    let data = b.data in
    get_pos_aux infos line coff data p

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
    val from_rope : context -> string -> Rope.t -> buffer
  end

module GenericInput(M : MinimalInput) =
  struct
    include M

    let from_channel
        : ?utf8:context -> ?filename:string -> in_channel -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") ch ->
        from_rope utf8 filename (Rope.from_channel ch)

    let from_fd
        : ?utf8:context -> ?filename:string -> Unix.file_descr -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") fd ->
      from_rope utf8 filename (Rope.from_fd fd)

    let from_file : ?utf8:context -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) fname ->
        let fd = Unix.(openfile fname [O_RDONLY] 0) in
        from_rope utf8 fname (Rope.from_fd ~close:true fd)

    let from_string : ?utf8:context -> ?filename:string -> string -> buffer =
      fun ?(utf8=Utf8.ASCII) ?(filename="") str ->
        from_rope utf8 filename (Rope.from_string str)
  end

include GenericInput(
  struct
    let from_rope utf8 name data =
      let infos = { utf8; name; uid = new_uid () } in
      let rec fn data lnum boff coff cont =
        (* Tail rec exception trick to avoid stack overflow. *)
        match data with
        | Rope.Empty -> cont (lnum+1) boff
        | _ ->
           let llen = Rope.length data in
           let nl = Rope.newline data in
           let nlnum, ncoff =
             if nl then (lnum+1, 0)
             else
                 let len =
                   if infos.utf8 <> Utf8.ASCII then utf8_len infos.utf8 data
                   else llen
                 in (lnum, coff + len)
           in
           { lnum ; boff; coff; data ; infos
             ; next = lazy (fn (Rope.next data) nlnum (boff + llen) ncoff cont)
             ; ctnr = [||] }
      in
      lazy
        begin
          let cont lnum boff =
            Lazy.force (empty_buffer infos lnum boff)
          in
          fn data 1 0 0 cont
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
    val update : state -> string -> int -> Rope.t
                 -> state * string * int * Rope.t option
    val check_final : state -> string -> unit
  end

module Make(PP : Preprocessor) =
  struct
    let from_rope utf8 name data =
      let infos = { utf8; name; uid = new_uid () } in
      let rec fn rope infos lnum boff coff st cont =
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let nl = Rope.newline data in
            let (st, name, lnum, res) = PP.update st infos.name lnum rope in
            let infos =
              if name <> infos.name then { infos with name } else infos
            in
            match res with
            | Some data ->
               let llen = Rope.length data in
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
                ; next = lazy (fn (Rope.next data) infos nlnum (boff + llen)
                                 ncoff st cont)
                ; ctnr = [||] }
            | None ->
              fun () -> fn rope infos lnum boff coff st cont
          with End_of_file ->
            fun () -> cont infos (lnum+1) boff st
        end ()
      in
      lazy
        begin
          let cont infos lnum boff st =
            PP.check_final st infos.name;
            Lazy.force (empty_buffer infos lnum boff)
          in
          fn Empty infos 1 0 0 PP.initial_state cont
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
