(** {1 Functions managing positions} *)

(** short position *)
type spos = Input.spos

(** Type to represent an interval between two position *)
type pos =
  { offset_start : int
  ; offset_end   : int
  ; infos        : Input.infos }

val mk_pos : Input.byte_pos -> Input.byte_pos -> Input.infos -> pos

(** a phantom position, used for grammar accepting the empty input, and other
    reference initialisation *)
val phantom_pos : pos
val no_pos : pos

(** return false on [phantom_pos/no_pos] *)
val has_pos : pos -> bool

(** build the smallest position containing both position *)
val merge : pos -> pos -> pos

(** Fully informed positions *)
type pos_info =
  { start_line        : int
  ; start_col         : int
  ; start_line_offset : int
  ; start_byte        : int
  ; end_line          : int
  ; end_col           : int
  ; end_line_offset   : int
  ; end_byte          : int
  ; file_name         : string
  ; text              : string }

(** Function to recover full position from the information stored in located
   data.

    The optional paramater ~relocate, which is identity by default allows to
   deal with situation like a moved file since position was produced, or a file
   name stored with a relative position. The file_name in the pos_info is not
   affected by [relocate] which is only used to reopen the file.

    The optionnal parameter [~text] (false by default) will fill the [text]
   field within the position if it is true. Otherwise the [text] field is set to
   the empty string.

    This function may raise [No_detailled_position] is the file can not
   be reopen or [File_changed] if the file is present but changed its last modificatio
   time of size. *)
exception No_detailed_position
exception File_changed

val pos_info
    : ?relocate:(string -> string) -> ?text:bool -> pos -> pos_info

(** configuration for quoting file *)
type quote =
  { numbers  : bool    (** includes line number *)
  ; prefix   : string  (** prefix added after each newline but not added for the
                          first printed line *)
  ; header   : string  (** header, added as first line if non empty *)
  ; footer   : string  (** footer, added as last line if non empty *)
  ; enlight  : string -> string
                       (** used to transform the quoted part of the printed
                           lines *)
  }

(** default quote:
```
  let ulined : string -> string =
    fun s -> "\027[0m\027[4m" ^ s ^ "\027[0m"

  let default_quote =
    { numbers  = true
    ; prefix   = ""
    ; header   = ""
    ; footer   = ""
    ; enlight  = ulined }
'''
*)
val default_quote : quote

(** Style for printing positions: *)
type style = OCaml (** like OCaml *)
           | Short (** like gcc *)

(** printing for the three kind of positions, and the current position
    of a buffer.  *)
val print_pos_info   : ?style:style -> ?quote:quote -> unit -> out_channel -> pos_info -> unit
(** The three functions below will print only the byte_pos if the file
    cannot be reopenned. The exception File_changed is raised if the file
    changed since last openning.

    If quote is given, the file is quoted.
*)
val print_pos  : ?style:style -> ?quote:quote -> unit -> out_channel -> pos -> unit
val print_spos  : ?style:style -> ?quote:quote -> unit -> out_channel -> spos -> unit
val print_buf_pos : ?style:style -> ?quote:quote -> unit -> out_channel
                    -> (Input.buffer * Input.idx) -> unit

(** Exception raised when parsing fails *)
exception Parse_error of Input.buffer * Input.idx * string list

(** [handle_exception  fn v] applies  the function [fn]  to [v] and  handles the
    [Parse_error] exception. In  particular, a parse error  message is presented
    to the user  in case of a failure,  then [error e] is called where  e is the
    raised exception.   The default  [error] is  [fun _ ->  exit 1].  [raise] is
    another possibility. *)
val handle_exception : ?error:(exn -> 'b) -> ?style:style
                       -> ('a -> 'b) -> 'a -> 'b
