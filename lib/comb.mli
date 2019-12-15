(** {1 Combinator library, using continuation}

    As usual left recursion is not supported,  but the library is intended to be
    used  through  the  [Grammar]  module  that  provides  elimination  of  left
    recursion. However, a  cache combinatr is supported to overcome  the cost of
    backtracking. *)

(** {2 function and type usefull to the end-user} *)

(** The type of combinator *)
type 'a t

(** Partial parsing.  Beware, the returned position is not  the maximum position
    that can be reached by the grammar.  *)
val partial_parse_buffer : 'a t -> Blank.t -> ?blank_after:bool ->
        Lex.buf -> Lex.pos -> 'a * Lex.buf * Lex.pos

(** Returns all possible parse trees.  Usefull for natural languages but also to
    debug ambiguity in a supposed non ambiguous grammar. If end of input is not
    parsed in some ways, some value may correspond to only the beginning of the
    input. Except when debugging or testing, you should rather use cache/merge
    anyway. *)
val parse_all_buffer : 'a t -> Blank.t -> Lex.buf -> Lex.pos -> 'a list

(** {2 combinator constructors, normally not needed by the casual user } *)

(** [fail] is a parser rejecting every input (it always fails). *)
val fail : 'a t

(** Fails and report an error *)
val error : string -> 'a t

val assert_false : 'a t

(** [empty v] is  a parser that only accepts the empty input  and returns [v] as
    its semantic value. *)
val empty : 'a -> 'a t

(** [lexeme l] is  a parser accepting the lexeme (or  terminal) [l], and returns
    the corresponding semantic value. *)
val lexeme : 'a Lex.lexeme -> 'a t

(**  [seq g1  g2] sequences  the parsers  [g1] and  [g2].  The  resulting parser
    starts by parsing  using [g1], and then  parses the rest of  the input using
    [g2]. The result of  parsing with [g2] is then apply to  the result of [g1].
 *)
val seq : 'a t -> ('a -> 'b) t -> 'b t

(** [dseq c1 c2] is a dependant sequence, contrary to [seq c1 c2], the
    combinator used to parse after [c1] depends upon the first value returned by
    [c1]. It is a good idea to memoize the function c2.  The separation of ['a]
    and ['b] in the smeantics of [g1] allows to depend on the smallest set of
    possible vaue which is important in case of memoisation. *)
val dseq: ('a * 'b) t -> ('a -> ('b -> 'c) t)  -> 'c t

(** Combinator parsing with the first combinator and in case of failure with the
    second from  the same  position.  The optionnal  charset corresponds  to the
    charaters accepted at the beginning of  the input for each combinators.  The
    charset  must be  Charset.full if  the corresponding  combinator accept  the
    empty input *)
val alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t

(** same as above but with a list of alternative. *)
val alts : (Charset.t * 'a t) list -> 'a t

(** [option a ~cs  c] is an optimisation for [alt (empty a)  ~cs c].  In fact it
    is better to use [alt] with grammar  not accepting empty and use [option] to
    deal with an empty case *)
val option: 'a -> Charset.t -> 'a t -> 'a t

(** Parses with the given combinator and transforms the semantics with the given
    function *)
val app : 'a t -> ('a -> 'b) -> 'b t

(** forces immediate evaluation of the action just after parsing *)
val eval : 'a t -> 'a t

(** Parses  as the given  combinator and  give the position  to the left  of the
    parsing input as argument to the action *)
val left_pos : (Pos.t -> 'a) t -> 'a t

(** Same as above with the position to the right *)
val right_pos : (Pos.t -> 'a) t -> 'a t

(** To eliminate left  recursion, lpos has to be left factored.   if lpos is one
    single combinator, this adds a lot of closures in action code. To solve this
    problem, lpos is  splitted in two combinators, one that  pushes the position
    to a stack and pops after parsing and another that reads the position. *)
val read_pos : Pos.t Assoc.key -> (Pos.t -> 'a) t -> 'a t

(** key used by lr below *)
type 'a key = 'a Lazy.t Assoc.key

(** [lr c1 v c2] is an optimized version  of [let rec r = seq c1 (seq r c2)]
    which is  illegal as it  is left recursive  and loops. The  optional charset
    indicates the characteres accepted by [c2] at the beginning of input. [v] is
    like variable bound in [c2], see [read_tbl] below *)
val lr : 'a t -> 'a key -> 'a t -> 'a t

(** Same as above, but also store the position *)
val lr_pos : 'a t -> 'a key -> Pos.t Assoc.key -> 'a t -> 'a t

(** type to represent the left prefix of a mutually recursive grammar.
    the key represents the produced grammar for each left prefix. *)
type mlr_left =
  LNil : mlr_left
| LCns : 'a key * Charset.t * 'a t * mlr_left -> mlr_left

(** type of the suffix to be repeted in a mutually recursive grammar.
    the first key represents the grammar that parsed the input before
    the second key represents the produced grammar.

    Somehow, mlr_right is a matrix R, the two keys being the index of
    the coefficient and mlr_left is a vector L. Parsing, will somehow use
    L R^n for n large enough;
*)
type mlr_right =
  RNil : mlr_right
| RCns : 'a key * 'b key * Charset.t * 'b t * mlr_right -> mlr_right

(** The combinator itself. The optionnal argument indicated that we need
    the position before parsing *)
val mlr : ?lpos:Pos.t Assoc.key -> mlr_left -> mlr_right -> 'a key -> 'a t

(** combinator to  access the value stored by  lr. It must be uses  as prefix of
    [c2] in [lr c1 c2].  For instance, the coding  of [let rec r = seq c1 (seq r
    c2)] is [let k = Assoc.new_key () in lr c1 k (seq (read_tbl k) c2)]. Here we
    ommited the actions.  This way of  coding left recursion avoids to transform
    the action and  produce closure. The code for elimination  of left recursion
    is also much simpler *)
val read_tbl : 'a key -> 'a t

(** Allow to test the blank characteres before a grammar and more *)
val test_before : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t

(** Allow to test the blank characteres after a grammar and more *)
val test_after : ('a -> Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t

(** Access to a reference to a combinator, used by Grammar.compile for recursive
    grammars (not for left recursion *)
val deref : 'a t ref -> 'a t

(** Change the blank  function used to parse with the  given combinator.  we can
    choose which blank to use at the boundary with the optional parameters. *)
val change_layout : ?config:Blank.layout_config -> Blank.t -> 'a t -> 'a t

(** Combinator  that caches  a grammar to  avoid exponential  behavior.  parsing
    with the grammar  from each position is memoized to  avoid parsing twice the
    same sequence with the same grammar. *)
val cache : ?merge:('a -> 'a -> 'a) -> 'a t -> 'a t
