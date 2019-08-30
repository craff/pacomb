(** {1 Module to build and parse list of words} *)

(** Type of a word list with
    'a : the type of characters (typically, char or Uchar.t)
    'b : a value associated to each word *)
type ('a,'b) t

(** Create a new empty table *)
val create : unit -> ('a,'b) t

(** Returns the number of bindings in the table *)
val size : ('a,'b) t -> int

(** [add_ascii  tbl s  v] adds  a binding  from [s]  to [v]  in [tbl],  keep all
    previous bindings.

    [map] is a function transforming character before addition (typically a case
    transformer). (defaults to identity). *)
val add_ascii : ?map:(char -> char) -> (char,'b) t -> string -> 'b -> unit

(** [replace_ascii tbl s v] adds a binding  from [s] to [v] in [tbl], remove all
    previous bindings *)
val replace_ascii : ?map:(char -> char) -> (char,'b) t -> string -> 'b -> unit

(** [mem_ascii tbl s] tells if [s] if present in [tbl]. Typically used to reject
    identifiers that are keywords *)
val mem_ascii : ?map:(char -> char) -> (char,'b) t -> string -> bool

(** Same as above for an unicode string, which are splitted in graphemes *)
val add_utf8
  : ?map:(string -> string) -> (string, 'b) t -> string -> 'b -> unit
val replace_utf8
  : ?map:(string -> string) -> (string,'b) t -> string -> 'b -> unit
val mem_utf8
  : ?map:(string -> string) -> (string,'b) t -> string -> bool

(**  Parses word  from a  dictionnary returning  as action  all the  assiociated
    values (it is an ambiguous grammar if there is more than one value).

    [final_test]  will be  called after  parsing. It  may be  used typically  to
    ensure that the  next character is not alphanumeric.  Defaults  to an always
    passing test.

    [map] is called on each character before searching in the table, typically a
    case conversion. Defaults to identity.  *)
val word : ?name:string -> ?final_test:(Input.buffer -> Input.pos -> bool)
           -> ?map:(char -> char) -> (char, 'a) t -> 'a Grammar.t

val utf8_word : ?name:string -> ?final_test:(Input.buffer -> Input.pos -> bool)
           -> ?map:(string -> string) -> (string, 'a) t -> 'a Grammar.t
