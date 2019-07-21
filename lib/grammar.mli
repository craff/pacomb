(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(** {1 Main module of Pacomb}

    {2 Overview}

    PaComb  implements a  representation of  grammars with  semantical action
    (i.e. a value  returned as result of parsing).  A  semantical action is a
    value returned as  result of parsing.  Parsing is  performed by compiling
    the grammar to  combinators implemented in the  [Combinator] module. This
    library offers  "scanner less"  parsing, but the  [Lex] module  provide a
    notion  of terminals  and  blanks  which allows  for  easy  way to  write
    grammars in two phases as usual.

    Defining languages using directly the  Grammar module leads to cumbersome
    code. This is  why Pacomb propose a  ppx extension that can  be used with
    the compilation flag [-ppx pacombPpx]. Here is an example:


    {[
    [%%parser
       type p = Atom | Prod | Sum
       let rec
         expr p = Atom < Prod < Sum
                ; (p=Atom) (x::FLOAT)                        => x
                ; (p=Atom) '(' (e::expr Sum) ')'             => e
                ; (p=Prod) (x::expr Prod) '*' (y::expr Atom) => x*.y
                ; (p=Prod) (x::expr Prod) '/' (y::expr Atom) => x/.y
                ; (p=Sum ) (x::expr Sum ) '+' (y::expr Prod) => x+.y
                ; (p=Sum ) (x::expr Sum ) '-' (y::expr Prod) => x-.y
    ]]}

    The extension  [[%%parser ...]] extends  structure with new  let bindings
    defining grammars. This  applies both for [let] and [let  rec] the latter
    being  reserved  to recursive  grammars.  We  also provide  an  extension
    [[%grammar]]  for  expression  that  corresponds to  grammars,  i.e.  the
    right-hand side of binding in the [[%%parser]] extension.

    Here is the BNF for these right-hand-side, with its semantics
    {[
    grammar ::= rule                                                   itself
           | grammar ; rule                                       Grammar.alt
    rule ::= qitems => expr                            A rule with its action
           | expr < ... < expr                       priority order see below
    qitems ::= ()                                               Grammar.empty
           | non_empty_qitems                                          itself
    non_empty_qitems ::= qitem
           | non_empty_qitems qitems                              Grammar.seq
    qitem ::= item | (lid :: item)          give a name if used in the action
    item ::= '...'                                  Grammar.term(Lex.char ())
           | "..."                                Grammar.term(Lex.string ())
           | INT                                     Grammar.term(Lex.int ())
           | FLOAT                                 Grammar.term(Lex.float ())
           | RE(exp)        Grammar.term(Lex.regexp (Regexp.from_string exp))
           | exp                                                       itself
    ]}

    - non recursive let bindings correspond to just a name for the grammar.
    - recursive let bindings correspond either to
      - [declare_grammar + set_grammar] (if no paramater)
      - [grammar_familly + setting the grammar] is a parameter is given.
        In the latter case, a rule [p_1 < p_2 < ... < p_n] will automatically
        add rules to  include the grammar parametrized by p_i  in the grammar
        parametrized by p_(i+1).

    Anything  which  does not  coresponds  to  this  grammar will  we  keeped
    unchanged in the structure as ocaml code (like the type definition in the
    example  above.   A  mutually  recursive   definition  can  also  mix  te
    definition of grammars (parametric of  not) with the definition of normal
    ocaml values.


    Pacomb  must  eliminate  left  recursion  in grammars  in  order  to  use
    combinators that  would loop  otherwise. However,  left recursion  is not
    support it it traverses:

    - A [Grammar.layout]  contructor to  change blanks (probably  possible to
      solve this, but probably not woth it.

    - A [Grammar.desq] constructor that  provides dependent sequence. Solving
      this is an open problem.

    - Grammars are not left factorised automatically: (A B) | (A C) may parse
      A twice.  two solutions

      - left factorise your grammar yourself,
      - Use Grammar.cache, trading memory for speed.

    - The ppx  extension is not  not too bad but  still suffer from  the fact
      that is uses  a sublanguage of OCaml to describe  grammar. For instance
      [%grammar (_::INT) => 0] is not legal because "_" can not be used in an
      Ocaml expression.
*)

(** {2 Type} *)

(** type of a grammar with semantical action of type ['a ].*)
type 'a grammar

(** An abbreviation *)
type 'a t = 'a grammar

(** {2 Grammar contructors} *)

(** [print_grammar ch g] prints the grammar [g] of the given output channel.
    if [def=false] (the default is [true]) it will print the transformed
    grammar prior to compilation. *)
val print_grammar : ?def:bool -> out_channel -> 'a grammar -> unit

(** [fail ()] is a grammar that parses nothing (always fails) *)
val fail : unit -> 'a grammar

(** [empty a] accepts the empty input and returns [a] *)
val empty : 'a -> 'a grammar

(** [test b] is [if b then empty () else fail ()]. Very usefull in
    grammar family at the beginning of a rule *)
val test : bool -> unit grammar

(** [term t] accepts the terminal [t] and returns its semantics.
    See module [Lex] *)
val term : ?name:string -> 'a Lex.terminal -> 'a grammar

(** [appl g f] parses with [g] and apply [f] to the resulting semantics *)
val appl : ?name:string -> 'a grammar -> ('a -> 'b) -> 'b grammar

(** [alt g1 g2] parses with [g1] and if it fails then [g2] *)
val alt : 'a grammar -> 'a grammar -> 'a grammar

(** [seq g1 g2 f] parse with g1 and then with g2 for the rest of the input, uses [f]
    to combine both semantics *)
val seq : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** usefull derivations from [seq] *)
val seq1 : 'a grammar -> 'b grammar -> 'a grammar
val seq2 : 'a grammar -> 'b grammar -> 'b grammar
val seqf : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

(** [dseq g1 g2 f)] is a dependant sequence, the grammar [g2] used after [g1] may
    depend upon the semantics of [g1]. This is not very efficient as the grammar
    [g2] must be compiled at parsing time. It is a good idea to memoize [g2] *)
val dseq : 'a grammar -> ('a -> 'b grammar) -> ('b -> 'c) -> 'c grammar

(** [lpos g] is identical to [g] but passes the position just before parsing with
    [g] to the semantical action of [g] *)
val lpos : (Position.t -> 'a) grammar -> 'a grammar

(** [rpos g] is identical to [g] but passes the position just after parsing with
    [g] to the semantical action of [g] *)
val rpos : (Position.t -> 'a) grammar -> 'a grammar

(** variants of seqf with the position of the first iterm *)
val seqf_pos : 'a grammar -> (Position.t -> 'a -> Position.t -> 'b) grammar -> 'b grammar
val seqf_lpos : 'a grammar -> (Position.t -> 'a -> 'b) grammar -> 'b grammar
val seqf_rpos : 'a grammar -> ('a -> Position.t -> 'b) grammar -> 'b grammar

(** variants of seq2 with the position of the first iterm *)
val seq2_pos : 'a grammar -> (Position.t -> Position.t -> 'b) grammar -> 'b grammar
val seq2_lpos : 'a grammar -> (Position.t -> 'b) grammar -> 'b grammar
val seq2_rpos : 'a grammar -> (Position.t -> 'b) grammar -> 'b grammar

(** [cache g] avoid to parse twice the same input with g by memoizing the result
    of the first parsing. Using [cache] allows to recover a polynomial complexity *)
val cache : 'a grammar -> 'a grammar

(** [layout g b] changes the blank function to parse the input with the
    grammar [g]. The optional parameters allow to control which blanks are used
    at the bounndary. Both can be used in which case the new blanks are used
    second before parsing with [g] and first after. *)
val layout : ?old_before:bool -> ?new_before:bool ->
             ?new_after:bool -> ?old_after:bool ->
             'a grammar -> Lex.blank -> 'a grammar

(** {2 Definition of recursive grammars } *)

(** to define recursive grammars, one may declare the grammar first and then
    gives its value.
    [declare_grammar name] creates an undefined grammar with the given name *)
val declare_grammar : string -> 'a grammar

(** [set_grammar g1 g2] set the value of [g1] declared with [declare_grammar].
    will raise [Invalid_argument] if [g1] was not defined using
    [declare_grammar] or if it was already set.*)
val set_grammar : 'a grammar -> 'a grammar -> unit

(** [fixpoint g] compute the fixpoint of [g], that is a grammar [g0] such that
    [g0 = g g0] *)
val fixpoint : ?name:string -> ('a grammar -> 'a grammar) -> 'a grammar

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

(** {2 Compilation of a grammar and various} *)

(** [compile g] produces a combinator that can be used to actually do the parsing
    see the [Combinator] module *)
val compile : 'a grammar -> 'a Combinator.t

(** [grammar_info g] returns [(b,cs)] where [b] is true is the grammar accepts
    the empty input and where [cs] is the characters set accepted at the beginnning
    of the input. *)
val grammar_info : 'a grammar -> bool * Charset.t

(** gives the grammar name *)
val grammar_name : 'a grammar -> string

(** allows to rename a grammar *)
val give_name : string -> 'a grammar -> 'a grammar
