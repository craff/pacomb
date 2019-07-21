(** Functions managing positions *)

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; utf8_col : int (** column number with unicode *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

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
