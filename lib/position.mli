(** {1 Functions managing positions} *)

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; utf8_col : int (** column number with unicode *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

(** Abbreviation *)
type t = pos

(** a phantom position, used for grammar accepting the empty input *)
val phantom : pos

(** the max of to position (further in the file *)
val max_pos : pos -> pos -> pos

(** if false (the default) [utf8_col] field is set to [-1] by [get_pos] *)
val compute_utf8_col : bool ref

(** Get a position from an input buffer and a column number *)
val get_pos : Input.buffer -> int -> pos
