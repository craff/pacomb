(** {1 Main module of Pacomb} *)

(** {2 Type} *)

(** type of a grammar with semantical action of type ['a ].*)
type 'a grammar

(** An abbreviation *)
type 'a t = 'a grammar

(** {2 Grammar contructors} *)

(** All  construœqctors can be  given an optional  [name] argument that  is used
    when printing the grammar. *)

(** [print_grammar ch g] prints the grammar [g] of the given output channel.  if
    [def=false] (the  default is [true])  it will print the  transformed grammar
    prior to compilation. *)
val print_grammar : ?no_other:bool -> ?def:bool -> out_channel ->
                    'a grammar -> unit

(** [fail ()] is a grammar that parses nothing (always fails) *)
val fail : ?name:string -> unit -> 'a grammar

(** fails reporting an error *)
val error : ?name:string -> string -> 'a grammar

(** [empty a] accepts the empty input and returns [a] *)
val empty : ?name:string -> 'a -> 'a grammar

(** [test  b] is  [if b then  empty ()  else fail ()].  Very usefull  in grammar
    family at the beginning of a rule. The test is done at grammar construction,
    not at parsing time (except if it is used in a dependant grammar). *)
val cond : ?name:string -> bool -> unit grammar

(** [term  t] accepts the  terminal [t] and  returns its semantics.   See module
    [Lex] *)
val term : ?name:string -> 'a Lex.terminal -> 'a grammar

(** [appl g f] parses with [g] and apply [f] to the resulting semantics *)
val appl : ?name:string -> 'a grammar -> ('a -> 'b) -> 'b grammar

(** [eval g] parses with [g] and forces immedaite evaluation of the semantics *)
val eval : ?name:string -> 'a grammar -> 'a grammar

(** [alt [g1;g2;...;gn]] parses with [g1] and if it fails then [g2] and so on *)
val alt : ?name:string -> 'a grammar list -> 'a grammar

(** [seq g1 g2]  parses with [g1] and then with [g2] for  the rest of the input,
    combine both semantics by apply the semantics of [g2] to [g1] *)
val seq : ?name:string -> 'a grammar -> ('a -> 'b) grammar -> 'b grammar


(** variation of the abover when we do not use all semantics *)
val seq1 : ?name:string -> 'a grammar -> 'b grammar -> 'a grammar
val seq2 : ?name:string -> 'a grammar -> 'b grammar -> 'b grammar

(** [dseq  g1 g2)] is  a dependant sequence, the  grammar [g2] used  after  [g1]
    may depend  upon the semantics  of [g1]. This is  not very efficient  as the
    grammar [g2] must be compiled at  parsing time.  [g2] is memoized by default
    to partially overcome this fact. *)
val dseq : ?name:string -> ('a * 'b) grammar
           -> ('a -> ('b -> 'c) grammar) -> 'c grammar

(** [lpos  g] is identical  to [g] but passes  the position just  before parsing
    with [g] to the semantical action of [g] *)
val lpos : ?name:string -> (Pos.t -> 'a) grammar -> 'a grammar

(** [rpos g] is identical to [g] but passes the position just after parsing with
    [g] to the semantical action of [g] *)
val rpos : ?name:string -> (Pos.t -> 'a) grammar -> 'a grammar

(** variants of seq with the position of the first iterm *)
val seq_pos : ?name:string -> 'a grammar -> (Pos.t * 'a * Pos.t -> 'b) grammar
              -> 'b grammar
val seq_lpos : ?name:string -> 'a grammar -> (Pos.t * 'a -> 'b) grammar
               -> 'b grammar
val seq_rpos : ?name:string -> 'a grammar -> ('a * Pos.t -> 'b) grammar
               -> 'b grammar

(** [cache  g] avoids to parse twice  the same input  with [g] by  memoizing the
    result of  the first parsing. The  optional [merge] parameter is  applied to
    group semantics corresponding  to the same part of the  input. Using [cache]
    with [merge] allows to recover a polynomial time complexity (cubic at worst)
    and a quadratic space (in the size of the input) *)
val cache : ?name:string -> ?merge:('a -> 'a -> 'a) -> 'a grammar -> 'a grammar

(** allows to perform a test, the test function receive the position before
    and after the blanks *)
val test_before : ?name:string
                  -> (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a grammar -> 'a grammar

val test_after : ?name:string
                 -> ('a -> Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a grammar -> 'a grammar

(** particular cases of the above testing the absence of blanks. *)
val no_blank_before : 'a grammar -> 'a grammar
val no_blank_after : 'a grammar -> 'a grammar

(** [layout b g] changes the blank  function to parse the input with the grammar
    [g].  The optional parameters allow to  control which blanks are used at the
    boundary.  Both  can be used  in which case the  new blanks are  used second
    before parsing with [g] and first after. *)
val layout : ?name:string -> ?config:Blank.layout_config
             -> Blank.t -> 'a grammar -> 'a grammar

(** usual option/star/plus combinator *)
val option : ?name:string -> 'a grammar -> 'a option grammar
val default_option : ?name:string -> 'a -> 'a grammar -> 'a grammar
val star : ?name:string -> 'a grammar -> 'a list grammar
val plus : ?name:string -> 'a grammar -> 'a list grammar
val star_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar
val plus_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar

(** {2 Definition of recursive grammars } *)

(** to  define recursive grammars,  one may declare  the grammar first  and then
    gives its value.   [declare_grammar name] creates an  undefined grammar with
    the given name *)
val declare_grammar : string -> 'a grammar

(** [set_grammar g1  g2] set the value of [g1]  declared with [declare_grammar].
    will   raise   [Invalid_argument]   if    [g1]   was   not   defined   using
    [declare_grammar] or if it was already set.*)
val set_grammar : 'a grammar -> 'a grammar -> unit

(** [fixpoint g] compute  the fixpoint of [g], that is a  grammar [g0] such that
    [g0 = g g0] *)
val fixpoint : ?name:string -> ('a grammar -> 'a grammar) -> 'a grammar

(** [grammar_family to_str name] returns a  pair [(gs, set_gs)], where [gs] is a
    finite  family of  grammars parametrized  by a  value of  type ['a].  A name
    [name] is to  be provided for the family, and  an optional function [to_str]
    can be provided to print the parameter and display better error messages. *)
val grammar_family : ?param_to_string:('a -> string) -> string
  -> ('a -> 'b grammar) * (('a -> 'b grammar) -> unit)

(**
   {[
   (* Declare the grammar family *)
   let (gr, set_gr) = grammar_family to_str name in

   ... code using grammars of gr to define mutually recursive grammars ...

   (* Define the grammar family *)
   let _ = set_gr the_grammars

   ... now the new family can be used ...
   ]}
*)

(** {2 Compilation of a grammar and various} *)

(**  [compile g]  produces a  combinator that  can be  used to  actually do  the
    parsing see the [Comb] module *)
val compile : 'a grammar -> 'a Comb.t

(** gives the grammar name *)
val grammar_name : 'a grammar -> string

(** allows to rename a grammar *)
val give_name : string -> 'a grammar -> 'a grammar

(** Parse a whole input buffer. the eof combinator is added at
    the end of the given combinator *)
val parse_buffer : 'a grammar -> Blank.t -> Lex.buf -> Lex.pos -> 'a

(** Partial parsing.  Beware, the returned  position is not the maximum position
    that can  be reached  by the grammar  it the grammar  is ambiguous.  In this
    case, a message is printed on  stderr. The charset is the character accepted
    at  the  end of  input.  Mainly  useful  with  'eof' when  [blank_after]  is
    [true]. *)
val partial_parse_buffer : 'a grammar -> Blank.t -> ?blank_after:bool ->
                           Lex.buf -> Lex.pos -> 'a * Lex.buf * Lex.pos

(** Returns all possible parse trees.  Usefull for natural languages but also to
    debug ambiguity in a supposed non ambiguous grammar. *)
val parse_all_buffer : 'a grammar -> Blank.t -> Lex.buf -> Lex.pos -> 'a list

(**  Parse a  whole string,  reporting position  according to  utf8 if  optional
    argument [utf8] is given and [Utf8.UTF8 or Utf8.CJK_UTF8] *)
val parse_string  : ?utf8:Utf8.context -> ?filename:string
                    -> 'a grammar -> Blank.t -> string -> 'a

(**  Parse a  whole  input  channel, reporting  postiion  according  to utf8. *)
val parse_channel : ?utf8:Utf8.context -> ?filename:string
                    -> 'a grammar -> Blank.t -> in_channel -> 'a

(**  Parse a  whole  file, reporting  postiion  according  to utf8. *)
val parse_file : ?utf8:Utf8.context ->
                    'a grammar -> Blank.t -> string -> 'a
