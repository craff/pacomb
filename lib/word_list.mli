(** {1 Module to build and parse list of words} *)

(** Type of a word list with
    'a : the type of characters (typically, char or Uchar.t)
    'b : a value associated to each word *)
type ('a,'b) t

(** exception raise when multiple binding are added and not allowed *)
exception Already_bound

(** Create  a new  empty table. The  optional parameter [  unique ]  defaults to
   true. Setting  it to false  with allow multiple identical  bindings, creating
   ambiguous grammars. If [ unique ] is true, then adding multiple bindings will
   raise the exception [ Already_bound ].

   [map] is a function transforming  character before addition (typically a case
   transformer or a unicode normalisation). (defaults to identity).

   [final_test]  will be  called  after parsing.  It may  be  used typically  to
   ensure that the next character is not alphanumeric.  Defaults to an always
   passing test.

   [cs]  can be  given as  an optimisation.  All words  added should  start with
   characters in this set. *)
val create : ?unique:bool ->
             ?map:('a -> 'a) ->
             ?cs:Charset.t ->
             ?final_test:(Input.buffer -> Input.pos -> bool)
             -> unit -> ('a,'b) t

(** Returns the number of bindings in the table *)
val size : ('a,'b) t -> int

(** empty a table *)
val reset : ('a,'b) t -> unit

(** [add_ascii  tbl s  v] adds  a binding  from [s]  to [v]  in [tbl],  keep all
    previous bindings. *)

val add_ascii : (char,'b) t -> string -> 'b -> unit

(** [mem_ascii tbl s] tells if [s] if present in [tbl]. Typically used to reject
    identifiers that are keywords *)
val mem_ascii : (char,'b) t -> string -> bool

(** Same as above for a unicode string, which are splitted in graphemes *)
val add_utf8 : (string, 'b) t -> string -> 'b -> unit

val mem_utf8 : (string,'b) t -> string -> bool

(**  Parses word  from a  dictionnary returning  as action  all the  assiociated
    values (it is an ambiguous grammar if there is more than one value).*)
val word : ?name:string -> (char, 'a) t -> 'a Grammar.t

val utf8_word : ?name:string -> (string, 'a) t -> 'a Grammar.t
