(** A module providing efficient input buffers with preprocessing. *)

(** {2 Type} *)

(** The abstract type for an input buffer. *)
type buffer

type infos

val infos : buffer -> infos

(** The abstract type position relative to the current buffer *)
type idx

(** The abstract position relative to the beginning of file *)
type byte_pos

(** Short (and quick) type for positions *)
type spos = infos * byte_pos

(** convert byte_pos to natural number *)
val phantom_byte_pos : byte_pos
val int_of_byte_pos : byte_pos -> int

(** phantom spos*)
val phantom_spos : spos

(** position at the beginning of a buffer *)
val init_idx : idx

(** {2 Reading from a buffer} *)

(** [read buf idx] returns the character at posvalition [idx] in the buffer
    [buf], together with the new buffer and position. *)
val read : buffer -> idx -> char * buffer * idx

(** [sub b i len] returns [len] characters from position [idx]. If the
    end of buffer is reached, the string is filed with eof '\255' *)
val sub : buffer -> idx -> int -> string

(** [get buf idx] returns the character at position [idx] in the  buffer
    [buf]. *)
val get : buffer -> idx -> char

(** {2 Creating a buffer} *)

type context = Utf8.context

(** [from_file fn] returns a buffer  constructed using the file [fn].  if [utf8]
    is  [Utf8.UTF8]  ([Utf8.ASCII]  is  the  default),  positions  are  reported
    according to [utf8]. [read] is still reading bytes.  *)
val from_file : ?utf8:context -> string -> buffer

(** [from_channel ~filename  ch] returns a buffer constructed  using the channel
    [ch]. The optional [filename] is only used  as a reference to the channel in
    error messages. *)
val from_channel : ?utf8:context -> ?filename:string -> in_channel -> buffer

(** Same as above for file descriptor *)
val from_fd : ?utf8:context -> ?filename:string -> Unix.file_descr -> buffer

(** [from_string  ~filename str] returns  a buffer constructed using  the string
    [str]. The optional [filename] is only used as a reference to the channel in
    error messages. *)
val from_string : ?utf8:context -> ?filename:string -> string -> buffer

(** Exception that can  be raised by a preprocessor in case  of error. The first
    string references the name of the buffer (e.g. the name of the corresponding
    file) and the second string contains the message. *)
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

    (** [update  st name  lnum data  nl] takes as  input the  state [st]  of the
        preprocessor, the  file name [name], the  number of the next  input data
        [lnum] and  the next input  [data] itself. [nl] is  true iff there  is a
        newline at the end  of [data]. It returns a tuple of  the new state, the
        new  file name,  the new  line number  and [None]  if the  data must  be
        ignored or  [Some(data)] which is  the new data.  The new file  name and
        line  number can  be  used  to implement  line  number directives.   The
        function may raise [Preprocessor_error] in case of error. *)
    val update : state -> string -> string
                 -> state * (string option * int option * string) list

    (** [check_final st name]  check that [st] indeed is a  correct state of the
        preprocessor for  the end  of input of  file [name].  If  it is  not the
        case, then the exception [Preprocessor_error] is raised. *)
    val check_final : state -> string -> unit
  end

(** Functor for building buffers with a preprocessor. *)
module WithPP : functor (PP : Preprocessor) ->
  sig
    (** Same as [Input.from_channel] but uses the preprocessor. *)
    val from_channel : ?utf8:context -> ?filename:string -> in_channel -> buffer

    (** Same as [Input.from_file] but uses the preprocessor. *)
    val from_file : ?utf8:context -> string -> buffer

    (** Same as [Input.from_string] but uses the preprocessor. *)
    val from_string : ?utf8:context -> ?filename:string -> string -> buffer
  end

(** {2 Buffer manipulation functions} *)

(** [is_empty buf] test whether the buffer [buf] is empty. *)
val is_empty : buffer -> int -> bool

(** [line_num buf] returns the current line number of [buf]. *)
val line_num : infos -> byte_pos -> int

(** [col_num buf n] returns the current line number of [(buf,n)]. *)
val col_num : infos -> byte_pos -> int

(** position in bytes, regardless to utf8 *)
val byte_pos : buffer -> idx -> byte_pos

(** get spos from butter and idx *)
val spos : buffer -> idx -> spos

(** [normalize buf idx] ensures that [idx] is less than  the  length  of
    the current line in [str]. *)
val normalize : buffer -> idx -> buffer * idx

(** [filename buf] returns the file name associated to the [buf]. *)
val filename : infos -> string

(** [buffer_uid buf]  returns a unique identifier. [Input.read]  does not change
   the uid. The uid is created when creating the initial buffer. *)
val buffer_uid : buffer -> int

(** [buffer_eq b1 b2] tests the equality of [b1] and [b2]. *)
val buffer_equal : buffer -> buffer -> bool

(** [buffer_compare b1 b2] compares [b1] and [b2]. *)
val buffer_compare : buffer -> buffer -> int

(** [buffer_before b1 i1 b2 i2] returns  true if the position [b1, i1] is before
    [b2, i2]. Gives meaningless result if [b1] and [b2] do not refer to the same
    file, i.e. do not have the same uid. *)
val buffer_before : buffer -> int -> buffer -> int -> bool

(** Table  to associate value to  positions in input buffers.  The complexity of
    access in the table is O(ln(N)) where N is the number of tables. *)
module Tbl : sig
  type 'a t

  val create : unit -> 'a t

  val add : 'a t -> buffer -> idx -> 'a -> unit

  val find : 'a t -> buffer -> idx -> 'a

  val clear : 'a t -> unit

  val iter : 'a t -> ('a -> unit) -> unit
end
