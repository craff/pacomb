(* {1 Blank: ignoring insignificant characters}

It provides functions to eliminate "blank" characteres. *)


(**  Position  in   a  buffer  is  a  [Input.buffer]  together   with  an  index
    [Input.pos]. *)
type buf = Input.buffer
type pos = Input.pos

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> pos -> buf * pos
type t     = blank

(** {2 Functions managing blanks} *)

(** Use when you have no blank chars *)
val none : blank

(** Blank from a charset *)
val from_charset : Charset.t -> blank

(** Blank from a terminal *)
val from_terminal : 'a Lex.t -> blank

(** [line_comments s] Blank with standard spaces and line starting with [s].
    [cs] defaults to [Charset.from_string " \t\n\r"] *)
val line_comments : ?cs:Charset.t -> string -> blank

(* {2 records for layout (i.e. blank function) change} *)

type layout_config =
  { old_blanks_before : bool
  (** Ignoring blanks with the old blank function before parsing? *)
  ; new_blanks_before : bool
  (** Then ignore blanks with the new blank function (before parsing)? *)
  ; new_blanks_after  : bool
  (** Use the new blank function one last time before resuming old layout? *)
  ; old_blanks_after  : bool
  (** Use then the old blank function one last time as well? *) }

(** Default configuration,  parsing with the old blanks before  (i.e., the field
    [old_blanks_before] is  [true]), and the  new blanks after (i.e.,  the field
    [old_blanks_after] is also [true]). The other two fields are [false]. *)
val default_layout_config : layout_config
