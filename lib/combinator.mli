(** Combinator library, using continuation.
    as usual left recursion is not support, but the library is
    intended to be used through the [Grammar] module that
    provides elimination of left recursion *)
type 'a t

(** exception that can be raised to reject parsing from action code *)
val give_up : unit -> 'a

(** combinator that always fails *)
val cfail : 'a t

(** combinator accepting the empty input *)
val cempty : 'a -> 'a t

(** combinator accepting a given terminal *)
val cterm : 'a Lex.fterm -> 'a t

(** sequence of two combinators, parses with the first and then parses the
    rest of the input with the second combinator. The last function
    is used to compose the semantics returned by the two combinators.*)
val cseq : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t

(** [sdep_seq c1 c2 f] is a dependant sequence, contrary to [seq c1 c2 f],
    the combinator used to parse after [c1] depends upon the value
    returned by [c1]. It s a good idea to memoize the function c2. *)
val cdep_seq: 'a t -> ('a -> 'b t) -> ('b -> 'c) -> 'c t

(** Combinator parsing with the first combinator and in case
    of failure with the second from the same position.
    The optionnal charset corresponds to the charaters accepted
    at the beginning of the input for each combinators.
    The charset must be Charset.full if the corresponding combinator
    accept the empty input *)
val calt : ?cs1:Charset.t -> ?cs2:Charset.t -> 'a t ->  'a t -> 'a t

(** Parses with the given combinator and transforms the semantics with
    the given function *)
val capp : 'a t -> ('a -> 'b) -> 'b t

(** Parses as the given combinator and give the position
    to the left of the parsing input as argument to the action *)
val clpos : (Lex.pos -> 'a) t -> 'a t

(** to eliminate left recursion, lpos has to be left factored.
    if lpos is one single combinator, this adds a lot of closures
    in action code. To solve this problem, lpos is splitted in
    two combinators, one that pushes the position to a stack and pops
    after parsing and another that reads the position. *)
val cpush : 'a t -> 'a t
val cread : int -> (Lex.pos -> 'a) t -> 'a t

(** same as above with the position to the right *)
val crpos : (Lex.pos -> 'a) t -> 'a t

(** [cls c1 c2] is an optimized version of [let rec r = seq c1 (seq r c2)]
    which is illegal as it is left recursive and loops. The optional charset indicates
    the characteres accepted by [c2] at the beginning of input. *)
val clr : ?cs2:Charset.t -> 'a t -> ('a -> 'a) t -> 'a t

(** access to a reference to a combinator, use by Grammar.compile
    for recursive grammars (not for left recursion *)
val cref : 'a t ref -> 'a t

(** change the blank function used to parse with the given combinator.
    we can choose which blank to use at the boundary with the optional
    parameters. *)
val clayout
    : ?old_before:bool -> ?new_before:bool -> ?new_after:bool -> ?old_after:bool
      -> 'a t -> Lex.blank -> 'a t

(** combinator that caches a grammar to avoid exponential behavior.
    parsing with the grammar from each position is memoized to avoid
    parsing twice the same sequence with the same grammar. *)
val cache : 'a t -> 'a t

(** exception raised by the function below when parsing fails *)
exception Parse_error of Input.buffer * int

(** [handle_exception fn v] applies the function [fn] to [v] and handles
    the [Parse_error] exception. In particular, a parse error message is
    presented to the user in  case  of  a  failure, then [error ()]
    is called. The default [error] is [fun () -> exit 1]. *)
val handle_exception : ?error:(unit -> 'b) -> ('a -> 'b) -> 'a -> 'b

(** parse a whole input buffer. the eof combinator is added at
    the end of the given combinator *)
val parse_buffer : 'a t -> Lex.blank -> Input.buffer -> 'a

(** parse a while string *)
val parse_string : 'a t -> Lex.blank -> string -> 'a

(** parse a whole input channel *)
val parse_channel : 'a t -> Lex.blank -> in_channel -> 'a

(** partial parsing. Beware, the returned position is not the maximum position
    that can be reached by the grammar *)
val partial_parse_buffer : 'a t -> Lex.blank -> ?blank_after:bool
                           -> Input.buffer -> int -> 'a * Input.buffer * int
