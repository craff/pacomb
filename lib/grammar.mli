(** {1 Main module of Pacomb} *)

(** {2 Type} *)

(** type of a grammar with semantical action of type ['a ].*)
type 'a grammar

(** An abbreviation *)
type 'a t = 'a grammar

(** {2 Grammar contructors} *)

(** [print_grammar ch g] prints the grammar [g] of the given output channel.  if
    [def=false] (the  default is [true])  it will print the  transformed grammar
    prior to compilation. *)
val print_grammar : ?def:bool -> out_channel -> 'a grammar -> unit

(** [fail ()] is a grammar that parses nothing (always fails) *)
val fail : unit -> 'a grammar

(** [string msg] is a grammar that parses nothing (always fails) and store
    a message at the reached position that will be printing if parsing fails
    to go further in the buffer. Multiple message may be reported if the same
    position is reached in several ways *)
val error : string -> 'a grammar

(** [empty a] accepts the empty input and returns [a] *)
val empty : 'a -> 'a grammar

(** [test  b] is  [if b then  empty ()  else fail ()].  Very usefull  in grammar
    family at the beginning of a rule *)
val test : bool -> unit grammar

(** [term  t] accepts the  terminal [t] and  returns its semantics.   See module
    [Lex] *)
val term : ?name:string -> 'a Lex.terminal -> 'a grammar

(** [appl g f] parses with [g] and apply [f] to the resulting semantics *)
val appl : ?name:string -> 'a grammar -> ('a -> 'b) -> 'b grammar

(** [alt [g1;g2;...;gn]] parses with [g1] and if it fails then [g2] and so on *)
val alt : ?name:string -> 'a grammar list -> 'a grammar

(** [seq g1 g2 f] parse with g1 and then with g2 for the rest of the input, uses
    [f] to combine both semantics *)
val seq : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** usefull derivations from [seq] *)
val seq1 : 'a grammar -> 'b grammar -> 'a grammar
val seq2 : 'a grammar -> 'b grammar -> 'b grammar
val seqf : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

(** [dseq  g1 ~cs g2 f)]  is a dependant  sequence, the grammar [g2]  used after
    [g1] may depend  upon the semantics of  [g1]. This is not  very efficient as
    the  grammar [g2]  must be  compiled at  parsing time.  [g2] is  memoized by
    default *)
val dseq : ('a * 'b) grammar
           -> ?cs:Charset.t -> ('a -> 'c grammar)
           -> ('a -> 'b -> 'c -> 'd) -> 'd grammar

(** usefull derivation*)
val dseqf : ('a * 'b) grammar
            -> ?cs:Charset.t -> ('a -> ('a -> 'b -> 'c) grammar)
            -> 'c grammar

(** [lpos  g] is identical  to [g] but passes  the position just  before parsing
    with [g] to the semantical action of [g] *)
val lpos : (Pos.t -> 'a) grammar -> 'a grammar

(** [rpos g] is identical to [g] but passes the position just after parsing with
    [g] to the semantical action of [g] *)
val rpos : (Pos.t -> 'a) grammar -> 'a grammar

(** variants of seqf with the position of the first iterm *)
val seqf_pos : 'a grammar -> (Pos.t -> 'a -> Pos.t -> 'b) grammar -> 'b grammar
val seqf_lpos : 'a grammar -> (Pos.t -> 'a -> 'b) grammar -> 'b grammar
val seqf_rpos : 'a grammar -> ('a -> Pos.t -> 'b) grammar -> 'b grammar

(** variants of seq2 with the position of the first iterm *)
val seq2_pos : 'a grammar -> (Pos.t -> Pos.t -> 'b) grammar -> 'b grammar
val seq2_lpos : 'a grammar -> (Pos.t -> 'b) grammar -> 'b grammar
val seq2_rpos : 'a grammar -> (Pos.t -> 'b) grammar -> 'b grammar

(** [cache  g] avoid to  parse twice  the same input  with [g] by  memoizing the
    result of  the first parsing. Using  [cache] allows to recover  a polynomial
    time complexity (cubic at  worst) and a quadratic space (in  the size of the
    input) *)
val cache : 'a grammar -> 'a grammar

type layout_config = Comb.layout_config

(** [layout b g] changes the blank  function to parse the input with the grammar
    [g].  The optional parameters allow to  control which blanks are used at the
    bounndary. Both  can be used  in which case the  new blanks are  used second
    before parsing with [g] and first after. *)
val layout : ?config:layout_config -> Lex.blank -> 'a grammar -> 'a grammar

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
   ... the grammars in gr cannot be used in "left position" ...
   ... (same restriction as for declare_grammar ...

   (* Define the grammar family *)
   let _ = set_gr the_grammars

   ... now the new family can be used ...
   ]}
*)

(** {2 Compilation of a grammar and various} *)

(**  [compile g]  produces a  combinator that  can be  used to  actually do  the
    parsing see the [Comb] module *)
val compile : 'a grammar -> 'a Comb.t

(** [grammar_info g]  returns [(b,cs)] where [b] is true  is the grammar accepts
    the  empty input  and  where [cs]  is  the characters  set  accepted at  the
    beginnning of the input. *)
val grammar_info : 'a grammar -> bool * Charset.t

(** gives the grammar name *)
val grammar_name : 'a grammar -> string

(** allows to rename a grammar *)
val give_name : string -> 'a grammar -> 'a grammar

(** Parse a whole input buffer. the eof combinator is added at
    the end of the given combinator *)
val parse_buffer : 'a grammar -> Lex.blank -> Input.buffer -> int -> 'a

(** Parse a whole string *)
val parse_string : 'a grammar -> Lex.blank -> string -> 'a

(** Parse a whole input channel *)
val parse_channel : 'a grammar -> Lex.blank -> in_channel -> 'a

(** Partial parsing.  Beware, the returned position is not  the maximum position
    that can be reached by the grammar. The charset is the character accepted at
    the end of input. Mainly useful with 'eof' when [blank_after] is [true]. *)
val partial_parse_buffer : 'a grammar -> Lex.blank -> ?blank_after:bool ->
        ?cs:Charset.t -> Input.buffer -> int -> 'a * Input.buffer * int

(** Returns all possible parse trees.  Usefull for natural languages but also to
    debug ambiguity in a supposed non ambiguous grammar. *)
val parse_all_buffer : 'a grammar -> Lex.blank -> Input.buffer -> int -> 'a list
