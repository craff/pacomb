(** Functions managing positions *)

(** Type to represent position *)
include Input.Pos

type interval_aux =
  { name : string
  ; start_line : int
  ; start_col  : int
  ; end_line   : int
  ; end_col    : int
  ; phantom    : bool }

type interval = interval_aux Lazy.t

let interval (lazy p1) (lazy p2) =
  { name = p1.Input.Pos.name
  ; start_line = p1.line
  ; start_col  = p1.col
  ; end_line   = p1.line
  ; end_col    = p2.col
  ; phantom    = p1.phantom || p2.phantom }

type t = pos Lazy.t

let max_pos p1 p2 =
  if p1.line > p2.line then p1
  else if p1.line < p2.line then p2
  else if p1.col < p2.col then p2
  else p1

let phantom = lazy { name = ""; line = 0; col  = 0; phantom = true }

type style = OCaml | Short

let print_pos ?(style=OCaml) () ch (lazy pos : t) =
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

let print_interval ?(style=OCaml) () ch (lazy pos) =
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

let print_buf_pos ?(style=OCaml) () ch (buf,col) =
  print_pos ~style () ch (get_pos buf col)

(** exception returned by the parser *)
exception Parse_error of Input.buffer * Input.pos * string list

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
