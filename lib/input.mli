(** A module providing efficient input buffers with preprocessing. *)

(** {2 Type} *)

(** The abstract type for an input buffer. *)
type buffer

(** {2 Reading from a buffer} *)

(** [read buf pos] returns the character at position [pos] in the buffer
    [buf], together with the new buffer and position. *)
val read : buffer -> int -> char * buffer * int

(** [sub b i len] returns [len] characters from position [pos]. If the
    end of buffer is reached, the string is filed with eof '\255' *)
val sub : buffer -> int -> int -> string

(** [get buf pos] returns the character at position [pos] in the  buffer
    [buf]. *)
val get : buffer -> int -> char

(** {2 Creating a buffer} *)

(** [from_file fn] returns a buffer constructed using the file [fn]. *)
val from_file : string -> buffer

(** [from_channel ~filename ch] returns a buffer constructed  using  the
    channel [ch]. The optional [filename] is only used as a reference to
    the channel in error messages. *)
val from_channel : ?filename:string -> in_channel -> buffer

(** [from_string ~filename str] returns a buffer constructed  using  the
    string [str]. The optional [filename] is only used as a reference to
    the channel in error messages. *)
val from_string : ?filename:string -> string -> buffer

(** [from_fun finalise name get data] returns a buffer constructed  from
    the object [data] using the [get] function. The get function is used
    to obtain one line of input from [data]. The [finalise] function  is
    applied to [data] when the end of file is reached. The [name] string
    is used to reference the origin of the data in error messages. *)
val from_fun : ('a -> unit) -> string -> ('a -> string) -> 'a -> buffer

(** Exception that can be raised by a preprocessor in case of error. The
    first string references the name of the buffer (e.g. the name of the
    corresponding file) and the second string contains the message. *)
exception Preprocessor_error of string * string

(** [pp_error name msg] raises [Preprocessor_error(name,msg)]. *)
val pp_error : string -> string -> 'a

(** Specification of a preprocessor. *)
module type Preprocessor =
  sig
    (** Type for the internal state of the preprocessor. *)
    type state

    (** Initial state of the preprocessor. *)
    val initial_state : state

    (** [update st name lnum line] takes as input the state [st] of  the
        preprocessor, the file name [name], the number of the next input
        line [lnum] and the next input line [line] itself. It returns  a
        tuple of the new state, the new file name, and [None] if the line
        must be ignored or [Some(lnum,line)] which is the new line number
        and ne new line. The new file name and line number can be used to
        implement  line  number  directives.   The  function  may   aise
        [Preprocessor_error] in case of error. *)
    val update : state -> string -> int -> string
                   -> state * string * (int * string) option

    (** [check_final st name] check that [st] indeed is a correct  state
        of the preprocessor for the end of input of file [name].  If  it
        is not the  case, then  the  exception  [Preprocessor_error]  is
        raised. *)
    val check_final : state -> string -> unit
  end

(** Functor for building buffers with a preprocessor. *)
module WithPP : functor (PP : Preprocessor) ->
  sig
    (** Same as [Input.from_fun] but uses the preprocessor. *)
    val from_fun : ('a -> unit) -> string -> ('a -> string)
                     -> 'a -> buffer

    (** Same as [Input.from_channel] but uses the preprocessor. *)
    val from_channel : ?filename:string -> in_channel -> buffer

    (** Same as [Input.from_file] but uses the preprocessor. *)
    val from_file : string -> buffer

    (** Same as [Input.from_string] but uses the preprocessor. *)
    val from_string : ?filename:string -> string -> buffer
  end

(** {2 Buffer manipulation functions} *)

(** [is_empty buf] test whether the buffer [buf] is empty. *)
val is_empty : buffer -> int -> bool

(** [line_num buf] returns the current line number of [buf]. *)
val line_num : buffer -> int

(** [line_beginning buf] returns the offset of the current line  in  the
    buffer [buf]. *)
val line_offset : buffer -> int

(** [line buf] returns the current line in the buffer [buf]. *)
val line : buffer -> string

(** [line_length buf] returns the length of  the  current  line  in  the
    buffer [buf]. *)
val line_length : buffer -> int

(** [utf8_col_num buf pos] returns the utf8 column number  corresponding
    to the position [pos] in [buf]. *)
val utf8_col_num : buffer -> int -> int

(** [normalize buf pos] ensures that [pos] is less than  the  length  of
    the current line in [str]. *)
val normalize : buffer -> int -> buffer * int

(** [filename buf] returns the file name associated to the [buf]. *)
val filename : buffer -> string

(** [buffer_uid buf] returns a unique identifier for [buf]. *)
val buffer_uid : buffer -> int

(** [buffer_eq b1 b2] tests the equality of [b1] and [b2]. *)
val buffer_equal : buffer -> buffer -> bool

(** [buffer_compare b1 b2] compares [b1] and [b2]. *)
val buffer_compare : buffer -> buffer -> int

(** [leq_bug b1 i1 b2 i2] returns true if the position [b1, i1] is before
    [b2, i2]. Gives meaningless result if [b1] and [b2] do not refer to the
    same file. *)
val buffer_before : buffer -> int -> buffer -> int -> bool

(** Table to associate value to positions in input buffers *)
module Tbl : sig
  type 'a t

  val create : unit -> 'a t

  val add : 'a t -> buffer -> int -> 'a -> unit

  val find : 'a t -> buffer -> int -> 'a

  val clear : 'a t -> unit

  val iter : 'a t -> ('a -> unit) -> unit
end
