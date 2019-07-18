(** This library implements a representation of grammar with semantical action.
    A  semantical  action is a value returned  as result of parsing. Parsing is
    performed  by  compiling   the grammar  to  combinators implemented  in the
    [Combinator]  module.  This library offers  "scanner less" parsing, but the
    [Lex] module provide a notion of terminals and blanks which allows for easy
    way to write grammars. *)

(** type of a grammar with semantical action of type ['a ].*)
type 'a grammar
type 'a t

(** [print_grammar ch g] prints the grammar [g] of the given output channel. *)
val print_grammar : ?def:bool -> out_channel -> 'a grammar -> unit

(** [fail ()] is a grammar that parses nothing (always fails) *)
val fail : unit -> 'a grammar

(** [empty a] accepts the empty input and returns [a] *)
val empty : 'a -> 'a grammar

(** [term t] accepts the terminal [t] and returns its semantics.
    See module [Lex] *)
val term : ?name:string -> 'a Lex.terminal -> 'a grammar

(** [appl(g,f)] parses with [g] and apply [f] to the resulting semantics *)
val appl : ?name:string -> 'a grammar * ('a -> 'b) -> 'b grammar

(** [alt(g1,g2)] parses with [g1] and if it fails then [g2] *)
val alt : 'a grammar * 'a grammar -> 'a grammar

(** [seq(g1,g2,f)] parse with g1 and then with g2 for the rest of the input, uses [f]
    to combine both semantics *)
val seq : 'a grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c grammar
val seq1 : 'a grammar * 'b grammar -> 'a grammar
val seq2 : 'a grammar * 'b grammar -> 'b grammar

(** [dseq(g1,g2,f)] is a dependant sequence, the grammar [g2] used after [g1] may
    depend upon the semantics of [g1]. This is not very efficient as the grammar
    [g2] must be compiled at parsing time. *)
val dseq : 'a grammar * ('a -> 'b grammar) * ('b -> 'c) -> 'c grammar

(** [lr(g1,g2)] corresponds to R::= g1 | R g2 and is used in the elimination of
    left recursion *)
val lr : 'a grammar * ('a -> 'a) grammar -> 'a grammar

(** [lpos g] is identical to [g] but passes the position just before parsing with
    [g] to the semantical action of [g] *)
val lpos : (Lex.pos -> 'a) grammar -> 'a grammar

(** [lpos g] is identical to [g] but passes the position just after parsing with
    [g] to the semantical action of [g] *)
val rpos : (Lex.pos -> 'a) grammar -> 'a grammar

(** [fixpoint g] compute the fixpoint of [g], that is a grammar [g0] such that
    [g0 = g g0] *)
val fixpoint : ?name:string -> ('a grammar -> 'a grammar) -> 'a grammar

val cache : 'a grammar -> 'a grammar

val declare_grammar : string -> 'a grammar

val set_grammar : 'a grammar -> 'a grammar -> unit

(** [layout (g,b)] changes the blank function to parse the input with the
    grammar [g]. The optional parameters allow to control which blank is used
    at the bounndary. Both can be used in which case the new blank are used
    second before parseing with [g] and first after. *)
val layout : ?old_before:bool -> ?new_before:bool ->
             ?new_after:bool -> ?old_after:bool ->
             'a grammar * Lex.blank -> 'a grammar

(** [compile g] produces a combinator that can be used to actually do the parsing
    see the combinator module *)
val compile : 'a grammar -> 'a Combinator.t

val grammar_info : 'a grammar -> bool * Charset.t

(** [grammar_family to_str name] returns a pair [(gs, set_gs)], where [gs]
    is a finite family of grammars parametrized by a value of type ['a]. A name
    [name] is to be provided for the family, and an optional function [to_str]
    can be provided to print the parameter and display better error messages. *)
val grammar_family : ?param_to_string:('a -> string) -> string
  -> ('a -> 'b grammar) * (('a -> 'b grammar) -> unit)

(**
   {[
   (* Declare the grammar family *)
   let (gr, set_gr) = grammar_family to_str name in

   ... code using grammars of gr to define mutually recursive grammars ...
   ... the grammars in gr cannot be used in "left position" ...
   ... (same restriction as for declare_grammar ...

   (* Define the grammar family *)
   let _ = set_gr the_grammars

   ... now the new family can be used ...
   ]}
*)

val give_name : string -> 'a grammar -> 'a grammar
