(** {1 Combinator library, using continuation}

    As usual left recursion is not supported,  but the library is intended to be
    used  through  the  [Grammar]  module  that  provides  elimination  of  left
    recursion. However, a  cache combinatr is supported to overcome  the cost of
    backtracking. *)

(** {2 function and type usefull to the end-user} *)

(** The type of combinator *)
type 'a t

(** Partial parsing.  Beware, the returned position is not  the maximum position
    that can be reached by the grammar. The charset is the character accepted at
    the end of input. Mainly useful with 'eof' when [blank_after] is [true]. *)
val partial_parse_buffer : 'a t -> Lex.blank -> ?blank_after:bool ->
        ?cs:Charset.t -> Input.buffer -> int -> 'a * Input.buffer * int

(** Returns all possible parse trees.  Usefull for natural languages but also to
    debug ambiguity in a supposed non ambiguous grammar. *)
val parse_all_buffer : 'a t -> Lex.blank -> Input.buffer -> int -> 'a list

(** {2 combinator constructors, normally not needed by the casual user } *)

(** [fail] is a parser rejecting every input (it always fails). *)
val fail : 'a t

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
    The charset if  the set of characters  accepted by [g2] at  the beginning of
    input. *)
val seq : 'a t -> ?cs:Charset.t -> ('a -> 'b) t -> 'b t

(**  [dseq  c1 c2]  is  a  dependant sequence,  contrary  to  [seq c1  c2],  the
    combinator used to parse after [c1] depends upon the first value returned by
    [c1]. It is  a good idea to memoize  the function c2.  The charset  is an in
    [seq], but you have to take a set that works for all possible values of type
    ['a].  The separation  of ['a] and ['b]  in the smeantics of  [g1] allows to
    depend on the  smallest set of possible  vaue which is important  in case of
    memoisation. *)
val dseq: ('a * 'b) t -> ?cs:Charset.t -> ('a -> ('b -> 'c) t)  -> 'c t

(** Combinator parsing with the first combinator and in case of failure with the
    second from  the same  position.  The optionnal  charset corresponds  to the
    charaters accepted at the beginning of  the input for each combinators.  The
    charset  must be  Charset.full if  the corresponding  combinator accept  the
    empty input *)
val alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t

(** [option a ~cs  c] is an optimisation for [alt (empty a)  ~cs c].  In fact it
    is better to use [alt] with grammar  not accepting empty and use [option] to
    deal with an empty case *)
val option: 'a -> Charset.t -> 'a t -> 'a t

(** Parses with the given combinator and transforms the semantics with the given
    function *)
val app : 'a t -> ('a -> 'b) -> 'b t

(** Parses  as the given  combinator and  give the position  to the left  of the
    parsing input as argument to the action *)
val left_pos : (Pos.t -> 'a) t -> 'a t

(** To eliminate left  recursion, lpos has to be left factored.   if lpos is one
    single combinator, this adds a lot of closures in action code. To solve this
    problem, lpos is  splitted in two combinators, one that  pushes the position
    to a stack and pops after parsing and another that reads the position. *)
val push : 'a t -> 'a t
val read : int -> (Pos.t -> 'a) t -> 'a t

(** Same as above with the position to the right *)
val right_pos : (Pos.t -> 'a) t -> 'a t

(** [lr ~cs c1 v c2] is an optimized version  of [let rec r = seq c1 (seq r c2)]
    which is  illegal as it  is left recursive  and loops. The  optional charset
    indicates the characteres accepted by [c2] at the beginning of input. [v] is
    like variable bound in [c2], see [read_tbl] below *)
val lr : 'a t -> Charset.t -> 'a Assoc.key -> 'a t -> 'a t

(** combinator to  access the value stored by  lr. It must be uses  as prefix of
    [c2] in [lr c1 c2].  For instance, the coding  of [let rec r = seq c1 (seq r
    c2)] is [let k = Assoc.new_key () in lr c1 k (seq (read_tbl k) c2)]. Here we
    ommited the actions.  This way of  coding left recursion avoids to transform
    the action and  produce closure. The code for elimination  of left recursion
    is also much simpler *)
val read_tbl : 'a Assoc.key -> 'a t

(** Access to a reference to a combinator, used by Grammar.compile for recursive
    grammars (not for left recursion *)
val deref : 'a t ref -> 'a t

type layout_config =
  { old_blanks_before : bool
  (** Ignoring blanks with the old blank function before parsing? *)
  ; new_blanks_before : bool
  (** Then ignore blanks with the new blank function (before parsing)? *)
  ; new_blanks_after  : bool
  (** Use the new blank function one last time before resuming old layout? *)
  ; old_blanks_after  : bool
  (** Use then the old blank function one last time as well? *) }

(** Default configuration,  parsing with the old blanks before  (i.e., the field
    [old_blanks_before] is  [true]), and the  new blanks after (i.e.,  the field
    [old_blanks_after] is also [true]). The other two fields are [false]. *)
val default_layout_config : layout_config

(** Change the blank  function used to parse with the  given combinator.  we can
    choose which blank to use at the boundary with the optional parameters. *)
val change_layout : ?config:layout_config -> Lex.blank -> 'a t -> 'a t

(** Combinator  that caches  a grammar to  avoid exponential  behavior.  parsing
    with the grammar  from each position is memoized to  avoid parsing twice the
    same sequence with the same grammar. *)
val cache : 'a t -> 'a t
