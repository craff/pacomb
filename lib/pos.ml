(** Functions managing positions *)

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; utf8_col : int (** column number with unicode *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

type interval = { start : pos; end_ : pos }

type t = pos

let max_pos p1 p2 =
  if p1.line > p2.line then p1
  else if p1.line < p2.line then p2
  else if p1.col < p2.col then p2
  else p1

let phantom = { name = ""; line = 0; col  = 0; utf8_col = 0; phantom = true }

(** if false (the default) [utf8_col] field is set to [-1] by [get_pos] *)
let compute_utf8_col = ref false

(** build a position from an input buffer and a column number *)
let get_pos : Input.buffer -> int -> pos = fun b n ->
  let open Input in
  { name = filename b;
    line = line_num b;
    col = n;
    utf8_col = if !compute_utf8_col then utf8_col_num b n else (-1);
    phantom = false
  }

type style = OCaml | Short

let print_pos ?(utf8_col=false) ?(style=OCaml) () ch pos =
  let open Printf in
  let col = if utf8_col then pos.utf8_col else pos.col in
  if pos.name = "" then
    let format : (_,_,_) format = match style with
      | OCaml -> "Line %d, character %d"
      | Short -> "%d:%d"
    in
    fprintf ch format pos.line col
  else
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, line %d, character %d"
      | Short -> "%S:%d:%d"
    in
    fprintf ch format pos.name pos.line col

let print_interval ?(style=OCaml) () ch { start; end_ } =
  let open Printf in
  if start.line = end_.line then
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, line %d, characters %d-%d"
      | Short -> "%S:%d:%d-%d"
    in
    fprintf ch format start.name start.line start.col end_.col
  else
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, line %d, character %d - line %d, character %d"
      | Short -> "%S:%d:%d-%d:%d"
    in
    fprintf ch format start.name start.line start.col end_.line end_.col

let print_buf_pos ?(utf8_col=false) ?(style=OCaml) () ch (buf,col) =
  compute_utf8_col := true;
  print_pos ~utf8_col ~style () ch (get_pos buf col)

(** exception returned by the parser *)
exception Parse_error of Input.buffer * int * string list

let fail_no_parse (_:exn) = exit 1

(** A helper to handle exceptions *)
let handle_exception
      ?(utf8_col=false) ?(error=fail_no_parse) ?(style=OCaml) f a =
  try f a with Parse_error(buf, pos, msgs) as e ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: %a.\n%!")
      (print_buf_pos ~utf8_col ~style ()) (buf, pos);
    if msgs <> [] then
      begin
        Printf.eprintf "expecting:\n%!";
        List.iter (Printf.eprintf "\t%s\n%!") msgs;
      end;
    error e
