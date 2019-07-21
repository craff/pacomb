(** A small module for efficient regular expressions. *)

open Input

(** Type of a regular expression. *)
type regexp =
  | Chr of char        (* Single character.                *)
  | Set of Charset.t   (* Any character in a charset.      *)
  | Seq of regexp list (* Sequence of regular expressions. *)
  | Alt of regexp list (* Alternative between regexps.     *)
  | Opt of regexp      (* Optional regexp.                 *)
  | Str of regexp      (* Zero or more times the regexp.   *)
  | Pls of regexp      (* One  or more times the regexp.   *)
  | Sav of regexp      (* save what is read    *)

type t = regexp

(** Exception that is raised when a regexp cannot be read. *)
exception Regexp_error of buffer * int

val print : out_channel -> regexp -> unit

val accept_empty : regexp -> bool

val accepted_first_chars : regexp -> Charset.t

val from_string : string -> regexp

(** [read re buf pos] attempts to parse using the buffer [buf] at
    position [pos] using the regular expression [re]. The return value is
    a triple of the parsed string, the buffer after parsing and the
    position after parsing. The exception [Regexp_error(err_buf, err_pos]
    is raised in case of failure at the given position. *)
val read : regexp -> buffer -> int -> string list * buffer * int
