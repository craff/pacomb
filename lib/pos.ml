(** Functions managing positions *)

(** byte position, short abbreviation *)
type t = Input.spos

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

(** Type to represent position *)

let phantom = { name = ""; line = 0; col  = 0; phantom = true }

(** build a position from an input buffer and a column number *)
let pos_of_spos : t -> pos = fun (infos, i0 as spos) ->
  if spos == Input.phantom_spos then phantom else
  let name = Input.filename infos in
  let line = Input.line_num infos i0 in
  let col = Input.col_num infos i0 in
  { name; line; col; phantom=false }

let get_pos : Input.buffer -> Input.idx -> pos = fun b p ->
  let i0 = Input.byte_pos b p in
  pos_of_spos (Input.infos b, i0)

type interval =
  { name       : string
  ; start_line : int
  ; start_col  : int
  ; end_line   : int
  ; end_col    : int
  ; phantom    : bool }

let interval (p1 : pos) (p2 : pos) =
  { name       = p1.name
  ; start_line = p1.line
  ; start_col  = p1.col
  ; end_line   = p1.line
  ; end_col    = p2.col
  ; phantom    = p1.phantom || p2.phantom }

let interval_of_spos : (t * t) -> interval
  = function (b1, b2) ->
              let p1 = pos_of_spos b1 in
              let p2 = pos_of_spos b2 in
              interval p1 p2

let max_pos p1 p2 =
  if p1.line > p2.line then p1
  else if p1.line < p2.line then p2
  else if p1.col < p2.col then p2
  else p1

type style = OCaml | Short

let print_pos ?(style=OCaml) () ch (pos:pos) =
  let open Printf in
  if pos.name = "" then
    let format : (_,_,_) format = match style with
      | OCaml -> "Line %d, character %d"
      | Short -> "%d:%d"
    in
    fprintf ch format pos.line pos.col
  else
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, line %d, character %d"
      | Short -> "%S:%d:%d"
    in
    fprintf ch format pos.name pos.line pos.col

let print_interval ?(style=OCaml) () ch pos =
  let open Printf in
  if pos.name = "" then
    if pos.start_line = pos.end_line then
      let format : (_,_,_) format = match style with
        | OCaml -> "line %d, characters %d-%d"
        | Short -> "%d:%d-%d"
      in
      fprintf ch format pos.start_line pos.start_col pos.end_col
    else
      let format : (_,_,_) format = match style with
        | OCaml -> "line %d, character %d - line %d, character %d"
        | Short -> "%d:%d-%d:%d"
      in
      fprintf ch format pos.start_line
        pos.start_col pos.end_line pos.end_col
  else
    if pos.start_line = pos.end_line then
      let format : (_,_,_) format = match style with
        | OCaml -> "File %S, line %d, characters %d-%d"
        | Short -> "%S:%d:%d-%d"
      in
      fprintf ch format pos.name pos.start_line pos.start_col pos.end_col
    else
      let format : (_,_,_) format = match style with
        | OCaml -> "File %S, line %d, character %d - line %d, character %d"
        | Short -> "%S:%d:%d-%d:%d"
      in
      fprintf ch format pos.name pos.start_line
        pos.start_col pos.end_line pos.end_col

let print_spos ?(style=OCaml) () ch p =
  print_pos ~style () ch (pos_of_spos p)

let print_spos2 ?(style=OCaml) () ch p =
  print_interval ~style () ch (interval_of_spos p)

let print_buf_pos ?(style=OCaml) () ch (buf,idx) =
  print_pos ~style () ch (get_pos buf idx)

(** exception returned by the parser *)
exception Parse_error of Input.buffer * Input.idx * string list

let fail_no_parse (_:exn) = exit 1

(** A helper to handle exceptions *)
let handle_exception ?(error=fail_no_parse) ?(style=OCaml) f a =
  try f a with Parse_error(buf, pos, msgs) as e ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: %a.\n%!")
      (print_buf_pos ~style ()) (buf, pos);
    if msgs <> [] then
      begin
        Printf.eprintf "expecting:\n%!";
        List.iter (Printf.eprintf "\t%s\n%!") msgs;
      end;
    error e
