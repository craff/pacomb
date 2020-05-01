type context = Utf8.context

type stream_infos =
  | File of { name               : string
            ; length             : int
            ; date               : float
            }
  | String of string
  | Stream

let stream_infos_of_fd file_name fd =
  let open Unix in
  let s = fstat fd in
  File { name       = file_name
       ; length     = s.st_size
       ; date       = s.st_mtime }

let stream_infos_of_ch file_name ch =
  stream_infos_of_fd file_name (Unix.descr_of_in_channel ch)

let stream_infos_of_str str =
  String str

type infos =
  { utf8         : Utf8.context (** Uses utf8 for positions                 *)
  ; stream_infos : stream_infos (** The name of the buffer (e.g. file name) *)
  ; uid          : int          (** Unique identifier                       *)
  }

let stream_infos infos = infos.stream_infos

let filename infos = match infos.stream_infos with
  | File { name } -> name
  | _             -> ""

let utf8 infos = infos.utf8

type buffer =
  { boff         : int    (* Offset to the line ( bytes )            *)
  ; data         : string (* Contents of the buffer                  *)
  ; mutable next : buffer Lazy.t (* Following line                   *)
  ; mutable ctnr : Container.t option array
                          (* for map table, initialized if used      *)
  ; infos        : infos  (* infos common to the whole file          *)
  }

(* Generate a unique identifier. *)
let new_uid =
  let c = ref 0 in
  fun () -> let uid = !c in incr c; uid

(** infos function *)
let infos b = b.infos

let phantom_infos =
  { utf8         = Utf8.ASCII
  ; stream_infos = Stream
  ; uid          = new_uid ()
  }


(** idx type and constant *)
type idx = int

let init_idx = 0

(** byte_pos type and constant *)
type byte_pos = int

let int_of_byte_pos x = x

let init_byte_pos = 0

let phantom_byte_pos = -1

(** spos type  and constant *)
type spos = infos * byte_pos

let phantom_spos = (phantom_infos, phantom_byte_pos)

(* Emtpy buffer. *)
let empty_buffer infos boff =
  let rec line = lazy
    { boff; data = "" ; next = line ; infos ; ctnr = [||] }
  in Lazy.force line

let is_eof b = b.data = ""

let llen b = String.length b.data

(* Test if a buffer is empty. *)
let rec is_empty l idx =
  if idx < llen l then false
  else if idx = 0 then is_eof l
  else is_empty (Lazy.force l.next) (idx - llen l)

(* Read the character at the given position in the given buffer. *)
let [@inline] rec read l i =
  if i < llen l then (l.data.[i], l     , i+1)
  else if is_eof l then ('\255', l, 0)
  else read (Lazy.force l.next) (i - llen l)

(* Get the character at the given position in the given buffer. *)
let [@nline] rec get l i =
  if i < llen l then l.data.[i] else
  if is_eof l then '\255' else
  get (Lazy.force l.next) (i - llen l)

(* substring of a buffer *)
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

(* byte position *)
let [@inline] byte_pos b p = b.boff + p

(* short position *)
let [@inline] spos b p = (b.infos, b.boff + p)

(* Equality of buffers. *)
let buffer_equal b1 b2 =
  b1.infos.uid = b2.infos.uid && b1.boff = b2.boff

(* Comparison of buffers. *)
let buffer_compare b1 b2 =
  match b1.boff - b2.boff with
  | 0 -> b1.infos.uid - b2.infos.uid
  | c -> c

(* Get the unique identifier of the buffer. *)
let buffer_uid b = b.infos.uid


exception NoLineNorColumnNumber


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

let fd_buffer fd =
  let res = Bytes.create buf_size in
  let n = Unix.read fd res 0 buf_size in
  if n = 0 then (* n = 0: we are at EOF *)
    raise End_of_file
  else if n = buf_size then
    Bytes.unsafe_to_string res
  else
    Bytes.sub_string res 0 n

let from_fun finalise utf8 stream_infos get_line file =
  let infos = { utf8; stream_infos; uid = new_uid () } in
  let cont boff =
    finalise file;
    empty_buffer infos boff
  in
  let rec  fn boff =
    begin
      (* Tail rec exception trick to avoid stack overflow. *)
      try
        let data = get_line file in
        let llen = String.length data in
        fun () ->
        { boff; data ; infos
          ; next = lazy (fn (boff + llen))
          ; ctnr = [||] }
      with End_of_file ->
        fun () -> cont boff
    end ()
  in
  fn 0

let from_channel
    : ?utf8:context -> ?filename:string -> in_channel -> buffer =
  fun ?(utf8=Utf8.ASCII) ?(filename="") ch ->
  let filename = stream_infos_of_ch filename ch in
  from_fun ignore utf8 filename input_buffer ch

let from_fd
    : ?utf8:context -> ?filename:string -> Unix.file_descr -> buffer =
  fun ?(utf8=Utf8.ASCII) ?(filename="") fd ->
  let filename = stream_infos_of_fd filename fd in
  from_fun ignore utf8 filename fd_buffer fd

let from_file : ?utf8:context -> string -> buffer =
  fun ?(utf8=Utf8.ASCII) filename ->
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let filename = stream_infos_of_fd filename fd in
  from_fun Unix.close utf8 filename fd_buffer fd

let from_string : ?utf8:context -> string -> buffer =
  fun ?(utf8=Utf8.ASCII) str ->
  let stream_infos = stream_infos_of_str str in
  let b = ref true in
  let string_buffer () =
    if !b then (b := false; str) else raise End_of_file
  in
  from_fun ignore utf8 stream_infos string_buffer ()

let leq_buf {boff = b1} i1 {boff = b2} i2 =
  b1 < b2 || (b1 = b2 && (i1 <= i2))

let buffer_before b1 i1 b2 i2 = leq_buf b1 i1 b2 i2

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
    Container.add tbl (ctnr buf idx) x

  let find tbl buf idx =
    Container.find tbl (ctnr buf idx)

  let clear = Container.clear

  let iter : type a. a t -> (a -> unit) -> unit = fun tbl f ->
    Container.iter f tbl

end
