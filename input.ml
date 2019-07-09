(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

type line =
  { is_eof       : bool   (* Has the end of the buffer been reached? *)
  ; lnum         : int    (* Line number (startig at 1)              *)
  ; loff         : int    (* Offset to the line                      *)
  ; llen         : int    (* Length of the line                      *)
  ; data         : string (* Contents of the line                    *)
  ; mutable next : buffer (* Following line                          *)
  ; name         : string (* The name of the buffer (e.g. file name) *)
  ; uid          : int    (* Unique identifier                       *)
  ; ctnr         : Container.t} (* for map table                     *)
and buffer = line Lazy.t

(* Generate a unique identifier. *)
let new_uid =
  let c = ref 0 in
  fun () -> let uid = !c in incr c; uid

(* Emtpy buffer. *)
let empty_buffer name lnum loff =
  let rec line = lazy
    { is_eof = true ; name ; lnum ; loff ; llen = 0
    ; data = "" ; next = line ; uid = new_uid ()
    ; ctnr = Container.create () }
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

(* Get the current line as a string. *)
let line (lazy b) = b.data

(* Get the length of the current line. *)
let line_length (lazy b) = b.llen

(* Get the utf8 column number corresponding to the given position. *)
let utf8_col_num (lazy {data; _}) i =
  let rec find num pos =
    if pos < i then
      let cc = Char.code data.[pos] in
      if cc lsr 7 = 0 then find (num+1) (pos+1) else
      if (cc lsr 6) land 1 = 0 then -1 else (* Invalid utf8 character *)
      if (cc lsr 5) land 1 = 0 then find (num+1) (pos+2) else
      if (cc lsr 4) land 1 = 0 then find (num+1) (pos+3) else
      if (cc lsr 3) land 1 = 0 then find (num+1) (pos+4) else
      -0 (* Invalid utf8 character. *)
    else num
  in find 0 0

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
    val from_fun : ('a -> unit) -> string -> ('a -> string)
                     -> 'a -> buffer
  end

(* The following code has been borrowed from OCaml's “pervasives.ml” file of
   the standard library. This version preserves the newline in the output. *)
external unsafe_input : in_channel -> bytes -> int -> int -> int =
  "caml_ml_input"

external input_scan_line : in_channel -> int =
  "caml_ml_input_scan_line"

let input_line ch =
  let rec build_result buf pos = function
    | []       -> buf
    | hd :: tl -> let len = Bytes.length hd in
                  Bytes.blit hd 0 buf (pos - len) len;
                  build_result buf (pos - len) tl
  in
  let rec scan accu len =
    let n = input_scan_line ch in
    if n = 0 then (* n = 0: we are at EOF *)
      match accu with
      | [] -> raise End_of_file
      | _  -> build_result (Bytes.create len) len accu
    else if n > 0 then (* n > 0: newline found in buffer *)
      let res = Bytes.create n in
      ignore (unsafe_input ch res 0 n);
      match accu with
      | [] -> res
      |  _ -> let len = len + n in
              build_result (Bytes.create len) len (res :: accu)
    else (* n < 0: newline not found *)
      let beg = Bytes.create (-n) in
      ignore(unsafe_input ch beg 0 (-n));
      scan (beg :: accu) (len - n)
  in Bytes.to_string (scan [] 0)

module GenericInput(M : MinimalInput) =
  struct
    include M

    let from_channel : ?filename:string -> in_channel -> buffer =
      fun ?(filename="") ch -> from_fun ignore filename input_line ch

    let from_file : string -> buffer =
      fun fname -> from_fun close_in fname input_line (open_in fname)

    let from_string : ?filename:string -> string -> buffer =
      fun ?(filename="") str ->
        let get_string_line (str, p) =
          let len = String.length str in
          let start = !p in
            if start >= len then raise End_of_file;
            while (!p < len && str.[!p] <> '\n') do
              incr p
            done;
            if !p < len then incr p;
            let len' = !p - start in
            String.sub str start len'
        in
        from_fun ignore filename get_string_line (str, ref 0)
  end

include GenericInput(
  struct
    let from_fun finalise name get_line file =
      let rec fn name lnum loff cont =
        let lnum = lnum + 1 in
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let data = get_line file in
            let llen = String.length data in
            fun () ->
              { is_eof = false ; lnum ; loff ; llen ; data ; name
              ; next = lazy (fn name lnum (loff + llen) cont)
              ; uid = new_uid () ; ctnr = Container.create ()}
          with End_of_file ->
            finalise file;
            fun () -> cont name lnum loff
        end ()
      in
      lazy
        begin
          let cont name lnum loff =
            Lazy.force (empty_buffer name lnum loff)
          in
          fn name 0 0 cont
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
    val update : state -> string -> int -> string
                 -> state * string * int * bool
    val check_final : state -> string -> unit
  end

module Make(PP : Preprocessor) =
  struct
    let from_fun finalise name get_line file =
      let rec fn name lnum loff st cont =
        let lnum = lnum + 1 in
        begin
          (* Tail rec exception trick to avoid stack overflow. *)
          try
            let data = get_line file in
            let (st, name, lnum, take) = PP.update st name lnum data in
            if take then
              let llen = String.length data in
              fun () ->
                { is_eof = false ; lnum ; loff ; llen ; data ; name
                ; next = lazy (fn name lnum (loff + llen) st cont)
                ; uid = new_uid () ; ctnr = Container.create () }
            else
              fun () -> fn name lnum loff st cont
          with End_of_file ->
            finalise file;
            fun () -> cont name lnum loff st
        end ()
      in
      lazy
        begin
          let cont name lnum loff st =
            PP.check_final st name;
            Lazy.force (empty_buffer name lnum loff)
          in
          fn name 0 0 PP.initial_state cont
        end
  end

module WithPP(PP : Preprocessor) = GenericInput(Make(PP))

let leq_buf {uid=ident1; _} i1 {uid=ident2; _} i2 =
  (ident1 = ident2 && i1 <= i2) || ident1 < ident2

let buffer_before b1 i1 b2 i2 = leq_buf (Lazy.force b1) i1 (Lazy.force b2) i2

(** First kind of table: association list in file order
    (first position in the beginning *)
module OrdTbl = struct
  type 'a t = (line * int * 'a list) list

  let empty = []

  let add buf pos x tbl =
    let buf = Lazy.force buf in
    let rec fn acc = function
      | [] -> List.rev_append acc [(buf, pos, [x])]
      | ((buf',pos', y as c) :: rest) as tbl ->
         if pos = pos' && buf.uid = buf'.uid then
           List.rev_append acc ((buf', pos', (x::y)) :: rest)
         else if leq_buf buf pos buf' pos' then
           List.rev_append acc ((buf, pos, [x]) :: tbl)
         else fn (c::acc) rest
    in
    fn [] tbl

  let pop = function
    | [] -> raise Not_found
    | (buf,pos,l)::rest -> Lazy.from_val buf,pos,l,rest

  let is_empty tbl = tbl = []

  let iter buf fn =
    List.iter (fun (_,_,l) -> List.iter fn l) buf



end

(** Second kind of table: unordered, but imperative and more efficient *)
module Tbl = struct
  type 'a t = 'a option array Container.table

  let create = Container.create_table

  let add tbl buf pos x =
    let buf = Lazy.force buf in
    try
      let a = Container.find tbl buf.ctnr in
      a.(pos) <- Some x
    with Not_found ->
      let a = Array.make (buf.llen+1) None in
      a.(pos) <- Some x;
      Container.add tbl buf.ctnr a

  let find tbl buf pos =
    let buf = Lazy.force buf in
    let a = Container.find tbl buf.ctnr in
    match a.(pos) with
    | None -> raise Not_found
    | Some x -> x

  let clear = Container.clear

  let iter : type a. a t -> (a -> unit) -> unit = fun tbl f ->
    let open Container in
    let fn : a option array -> unit = fun a ->
      Array.iter (function None -> () | Some x -> f x) a
    in
    (* FIXME: https://caml.inria.fr/mantis/view.php?id=7636 *)
    iter { Container.f = Obj.magic fn } tbl

  (* Tests for the above FIXME: the type is not abstract ! *)
  (*
  let test1 : type a b. (a, b) Container.elt -> a = fun x -> x
  let test2 : type a b. a -> (a, b) Container.elt = fun x -> x
  *)
end
