(** A module providing efficient input buffers with preprocessing. *)

(** {2 Type} *)

(** Information for a file for reopening *)
type stream_infos =
  (* regular file, that can be reopened *)
  | File of { name               : string
            ; length             : int
            ; date               : float
            }
  (* string *)
  | String of string
  (* stream: no detailed position available *)
  | Stream

(** The abstract type for an input buffer. *)
type buffer

(** Type of fixed data attached to the buffer (like file name) *)
type infos

(** return the [infos] associated to a buffer *)
val infos : buffer -> infos

(** dummy infos *)
val phantom_infos : infos

(** returns the [stream_infos] stored in [infos] *)
val stream_infos : infos -> stream_infos

(** returns the filename if it exists, the empty string otherwise *)
val filename : infos -> string

(** [utf8 infos] return the unicode context in use for this file *)
val utf8 : infos -> Utf8.context

(** [find_directives infos offset line name] returns [line', name']
    according to a line directive as byte offset n, of the given
    [line] and [name] is no line directive exists at this position *)
val find_directives : infos -> int -> int -> string -> int * string

(** The abstract type position relative to the current buffer *)
type idx

(** position at the beginning of a buffer *)
val init_idx : idx

(** The abstract position relative to the beginning of buffer *)
type byte_pos

(** convert byte_pos to natural number *)
val int_of_byte_pos : byte_pos -> int

(** zero *)
val init_byte_pos : byte_pos

(** dummy value, to initiaize references for instance *)
val phantom_byte_pos : byte_pos

(** Short (and quick) type for positions *)
type spos = infos * byte_pos

(** dummy value, to initiaize references for instance *)
val phantom_spos : spos

(** {2 Reading from a buffer} *)

(** [read buf idx] returns the character  at position [idx] in the buffer [buf],
   together with the new buffer and position. Read infinitely many '\255' at end
   of buffer *)
val read : buffer -> idx -> char * buffer * idx

(** [sub b  i len] returns [len]  characters from position [idx]. If  the end of
   buffer is reached, the string is filed with eof '\255' *)
val sub : buffer -> idx -> int -> string

(**  [get  buf idx]  returns  the  character at  position  [idx]  in the  buffer
   [buf]. *)
val get : buffer -> idx -> char

(** {2 Creating a buffer} *)

type context = Utf8.context

(** [from_file fn] returns a buffer constructed using the file [fn].

    If [utf8] is  [Utf8.UTF8] or [Utf8.CJK_UTF8] ([Utf8.ASCII]  is the default),
   positions are reported according to [utf8]. [read] is still reading bytes.

    Getting line  number and column number  requires rescanning the file  and if
   the file  is not a regular  file, it is  kept in memory. Setting  [rescan] to
   false avoid this, but only byte position and file name will be available.  *)
val from_file : ?utf8:context -> string -> buffer

(** [from_channel ~filename  ch] returns a buffer constructed  using the channel
    [ch]. The optional [filename] is only used  as a reference to the channel in
    error messages.

    [uft8] and [rescan] as in [from_file]. *)
val from_channel : ?utf8:context -> ?filename:string
                   -> in_channel -> buffer

(** Same as above for file descriptor *)
val from_fd : ?utf8:context -> ?filename:string
              -> Unix.file_descr -> buffer

(** [from_string  ~filename str] returns  a buffer constructed using  the string
    [str]. The optional [filename] is only used as a reference to the channel in
    error messages. *)
val from_string : ?utf8:context -> string -> buffer

(** Specification of a preprocessor. *)
module type Preprocessor =
  sig
    (** Type for the internal state of the preprocessor. *)
    type state

    (** Initial state of the preprocessor. *)
    val initial_state : state

    (** [update st name data] takes as input the state [st] of the preprocessor,
       the file name [name] and the next input [data] itself. It returns the new
       state,  a list  tuples  [(new_filename, new_linenum,  new_data)] with  an
       optional new file name  and line number and the data  which can be empty.
       This allows ofor include directive or line number directives.

       Beware that the preprocessor is rerun  when getting position so it should
       be reentrant.
    *)
    val update : state -> string
                 -> state * (string option * int option * string) list

    (** [check_final st name]  check that [st] indeed is a  correct state of the
        preprocessor for  the end  of input of  file [name].  If  it is  not the
        case, then this function shoud raise an exception of your choice. *)
    val check_final : state -> unit
  end

(** Functor for building buffers with a preprocessor. *)
module WithPP : functor (PP : Preprocessor) ->
  sig
    (** Same as [Input.from_channel] but uses the preprocessor. *)
    val from_channel : ?utf8:context -> ?filename:string
                       -> in_channel -> buffer

    (** Same as [Input.fd] but uses the preprocessor. *)
    val from_fd : ?utf8:context -> ?filename:string
                  -> Unix.file_descr -> buffer

    (** Same as [Input.from_file] but uses the preprocessor. *)
    val from_file : ?utf8:context
                    -> string -> buffer

    (** Same as [Input.from_string] but uses the preprocessor. *)
    val from_string : ?utf8:context -> string -> buffer
  end

(** {2 Buffer manipulation functions} *)

(** [is_empty buf] test whether the buffer [buf] is empty. *)
val is_empty : buffer -> int -> bool

exception NoLineNorColumnNumber

(** position in bytes, regardless to utf8 *)
val byte_pos : buffer -> idx -> byte_pos

(**  get spos  from  buffer and  idx,  to  get line_num  and  col_num if  needed
   later. *)
val spos : buffer -> idx -> spos

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
