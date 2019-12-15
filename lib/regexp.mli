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
exception Regexp_error of Input.buffer * Input.pos

(** [from_string s] convert a string into a regexp following [Str] syntax. *)
val from_string : string -> regexp

(** create a terminal from a regexp. Returns the whole matched string *)
val regexp : ?name:string -> regexp -> string Lex.t

(** create a terminal from a regexp.  Returns the groups list, last to finish to
    be  parsed  is first  in  the  result.  The  optional argument  grps  allows
    selection of the produced groups. As usual, 0 means the whole regexp and n >
    0 the sub string corresponding to the nth opening parenthesis.  *)
val regexp_grps : ?name:string -> ?grps:int list -> regexp -> string list Lex.t

(** create a blank function from a string representing a regexp *)
val blank_regexp : string -> Blank.t
