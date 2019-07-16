(** This library implements a representation of grammar with semantical action.
    A  semantical  action is a value returned  as result of parsing. Parsing is
    performed  by  compiling   the grammar  to  combinators implemented  in the
    [Combinator]  module.  This library offers  "scanner less" parsing, but the
    [Lex] module provide a notion of terminals and blanks which allows for easy
    way to write grammars. *)

(** type of a grammar with semantical action of type ['a ].*)
type 'a t

(** [print_grammar ch g] prints the grammar [g] of the given output channel. *)
val print_grammar : ?def:bool -> out_channel -> 'a t -> unit

(** [fail ()] is a grammar that parses nothing (always fails) *)
val fail : unit -> 'a t

(** [empty a] accepts the empty input and returns [a] *)
val empty : 'a -> 'a t

(** [term t] accepts the terminal [t] and returns its semantics.
    See module [Lex] *)
val term : 'a Lex.terminal -> 'a t

(** [appl(g,f)] parses with [g] and apply [f] to the resulting semantics *)
val appl : 'a t * ('a -> 'b) -> 'b t

(** [alt(g1,g2)] parses with [g1] and if it fails then [g2] *)
val alt : 'a t * 'a t -> 'a t

(** [seq(g1,g2,f)] parse with g1 and then with g2 for the rest of the input, uses [f]
    to combine both semantics *)
val seq : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t

(** [dseq(g1,g2,f)] is a dependant sequence, the grammar [g2] used after [g1] may
    depend upon the semantics of [g1]. This is not very efficient as the grammar
    [g2] must be compiled at parsing time. *)
val dseq : 'a t * ('a -> 'b t) * ('b -> 'c) -> 'c t

(** [lr(g1,g2)] corresponds to R::= g1 | R g2 and is used in the elimination of
    left recursion *)
val lr : 'a t * ('a -> 'a) t -> 'a t

(** [lpos g] is identical to [g] but passes the position just before parsing with
    [g] to the semantical action of [g] *)
val lpos : (Lex.pos -> 'a) t -> 'a t

(** [lpos g] is identical to [g] but passes the position just after parsing with
    [g] to the semantical action of [g] *)
val rpos : (Lex.pos -> 'a) t -> 'a t

(** [fixpoint g] compute the fixpoint of [g], that is a grammar [g0] such that
    [g0 = g g0] *)
val fixpoint : ?name:string -> ('a t -> 'a t) -> 'a t

val declare_grammar : ?name:string -> unit -> 'a t

val set_grammar : 'a t -> 'a t -> unit

(** [layout (g,b)] changes the blank function to parse the input with the
    grammar [g]. The optional parameters allow to control which blank is used
    at the bounndary. Both can be used in which case the new blank are used
    second before parseing with [g] and first after. *)
val layout : ?old_before:bool -> ?new_before:bool ->
             ?new_after:bool -> ?old_after:bool ->
             'a t * Lex.blank -> 'a t

(** [compile g] produces a combinator that can be used to actually do the parsing
    see the combinator module *)
val compile : 'a t -> 'a Combinator.t
