(** A small module for efficient regular expressions. *)

(** Type of a regular expression. *)
type regexp =
  | Chr of char        (* Single character.                *)
  | Set of Charset.t   (* Any character in a charset.      *)
  | Seq of regexp list (* Sequence of regular expressions. *)
  | Alt of regexp list (* Alternative between regexps.     *)
  | Opt of regexp      (* Optional regexp.                 *)
  | Str of regexp      (* Zero or more times the regexp.   *)
  | Pls of regexp      (* One  or more times the regexp.   *)
  | Sav of regexp      (* Save the matching string.        *)

(** Short synonym of {!type:regexp}. *)
type t = regexp

(** [pp ff re] outputs the regexp [re] to the formatter [ff]. *)
val pp : Format.formatter -> regexp -> unit

(** [accepts_empty re] tells whether the empty input is valid for [re]. *)
val accepts_empty : regexp -> bool

(** [accepted_first_chars re] returns the set of characters that are possible,
    valid first characters for matching [re]. *)
val accepted_first_chars : regexp -> Charset.t

(** Exception raised when a regexp does not match.  Note that the given buffer
    and position correspond to the first character that cannot be matched. *)
exception Regexp_error of Input.buffer * int

(** [read re buf pos] attempts to match the regular expression [re] in  buffer
    [buf], at position [pos]. The returned value [(matched, buf', pos')] gives
    the new buffer [buf'] and position [pos'] after the longest possible match
    using [re]. It also contains a list of matched strings, all corresponding
    to a {!constructor:Sav} constructor. If the regexp [re] cannot be matched
    then exception {!exception:Regexp_error} is raised. *)
val read : regexp -> Input.buffer -> int -> string list * Input.buffer * int

(** [from_string s] ... TODO *)
val from_string : string -> regexp
