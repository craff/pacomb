(** Combinator library, using continuation.
    as usual left recursion is not support, but the library is
    intended to be used through the [Grammar] module that
    provides elimination of left recursion *)
type 'a comb

(** exception that can be raised to reject parsing from action code *)
val give_up : unit -> 'a

(** combinator that always fails *)
val cfail : 'a comb

(** combinator accepting the empty input *)
val cempty : 'a -> 'a comb

(** combinator accepting a given terminal *)
val cterm : 'a Lex.fterm -> 'a comb

(** sequence of two combinator, parse with the first and theh parse the
    rest of the string with the second combinator. The last function
    is used to compose the semantics returned by the two combinators.*)
val cseq : 'a comb -> 'b comb -> ('a -> 'b -> 'c) -> 'c comb

(** combinator parsing with the first combinator and in case
    of failure with the second from the same position.
    The optionnal charset corresponds to the charaters accepted
    at the beginning of the input for each combinators.
    The charset must be Charset.full if the corresponding combinator
    accept the empty input *)
val calt : ?cs1:Charset.t -> ?cs2:Charset.t -> 'a comb ->  'a comb -> 'a comb

(** parse with the given combinator and transform the semantics with
    the given function *)
val capp : 'a comb -> ('a -> 'b) -> 'b comb

(** parse as the given combinator and give the position
    to the left of the parsing input as argument to the action *)
val clpos : (Lex.pos -> 'a) comb -> 'a comb

(** same as above with the position to the right *)
val crpos : (Lex.pos -> 'a) comb -> 'a comb

(** [cls c1 c2] is an optimized version of [let rec r = seq c1 (seq r c2)]
    which is illegal as it is left recursive. The optional charset indicates    the characteres accepted by [c2] at the beginning of input. *)
val clr : ?cs2:Charset.t -> 'a comb -> ('a -> 'a) comb -> 'a comb

(** exception raised by the function below when parsing fails *)
exception ParseError of Lex.pos

val parse_buffer : 'a comb -> Lex.blank -> Input.buffer -> 'a

val parse_string : 'a comb -> Lex.blank -> string -> 'a

val parse_channel : 'a comb -> Lex.blank -> in_channel -> 'a

(** access to a reference to a combinator, use by Grammar.compile
    for recursive grammars (not for left recursion *)
val cref : 'a comb ref -> 'a comb
