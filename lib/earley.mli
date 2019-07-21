(** {1 Earley compatible interface (UNFINISHED) } *)

(** Earley is a parser combinator library implemented using  the  Earley
    algorithm. This modules is an UNFINISHED WORK to provide an Earley
    compatible interface to Pacomb *)

open Charset
open Input

(** {2 Types and exceptions} *)

(** Type of a parser (or grammar) producing a value of type ['a]. *)
type 'a grammar

(** As [Earley] does scannerless parsing, a notion of  [blank]  function
    is used to discard meaningless parts of the input (e.g. comments  or
    spaces). A [blank] function takes as input a [buffer] and a position
    (represented as an [int]) and returns a couple of a [buffer]  and  a
    position corresponding to the next meaningful character.

    WARNING: a blank function must return a normalized pair (b,p),
    which means 0 <= p < Input.line_num b. You can use Input.normalize
    to ensure this. *)
type blank = buffer -> int -> buffer * int

(** The exception [Parse_error(buf,pos,msgs)] is raised whenever parsing
    fails. It contains the position [pos] (and the corresponding  buffer
    [buf]) of the furthest reached position in the input. *)
exception Parse_error of buffer * int

(** [give_up ()] can be called by the user to force the parser to reject
    a possible parsing rule. *)
val give_up : unit -> 'a

(** [handle_exception fn v] applies the function [fn] to [v] and handles
    the [Parse_error] exception. In particular, a parse error message is
    presented to the user in  case  of  a  failure, then [error ()]
    is called. The default [error] is [fun () -> exit 1]. *)
val handle_exception : ?error:(unit -> 'b) -> ('a -> 'b) -> 'a -> 'b

(** {2 Atomic parsers} *)

(** [char ~name c v] is a grammar that accepts only the  character  [c],
    and returns [v] as a semantic value. An optional [name] can be given
    to the grammar for reference in error messages. *)
val char : ?name:string -> char -> 'a -> 'a grammar

(** [string s v] is a grammar that accepts only the  string  [str],  and
    returns [v] as a semantic value. An optional [name] can be given  to
    the grammar for reference in error messages. *)
val string : ?name:string -> string -> 'a -> 'a grammar

(** [keyword s forbidden v] is simalar to string, but the parsing fails
    if [forbidden c] returns [true] when [c] is the next available
    character. *)
val keyword : ?name:string -> string -> (char -> bool) -> 'a -> 'a grammar

(** [eof v] is a grammar that only accepts the end of file  and  returns
    [v] as a semantic value. Note that the end of file can be parsed one
    or more times (i.e. the input ends with infinitely many end of  file
    symbols. *)
val eof : 'a -> 'a grammar

(** [any] is a grammar that accepts a single character (but fails on the
    end of file) and returns its value. *)
val any : char grammar

(** [in_charset cs] is a grammar that parses any character of  the  [cs]
    charset, and returns its value. An optional [name] can be  given  to
    the grammar for reference in error messages. *)
val in_charset : ?name:string -> charset -> char grammar

(** [not_in_charset cs] is similar to  [in_charset cs]  but  it  accepts
    the characters that are not in [cs]. *)
(*val not_in_charset : ?name:string -> charset -> unit grammar*)

(** [blank_not_in_charset cs] is the same as [not_in_charset] but
    testing with blank_test. *)
(*val blank_not_in_charset : ?name:string -> charset -> unit grammar*)

(** [empty v] is a grammar that does not parse anything and returns  [v]
    as a semantic value. Note that this grammar never fails. *)
val empty : 'a -> 'a grammar

(** type for a function waiting for the start and end positions
    (i.e. buffer and index) of an item, in general resulting from parsing *)
type 'a fpos = buffer -> int -> buffer -> int -> 'a

(** [empty_pos v] is similar to the above except that the action
    wait for the position of a complete sequence build using
     [fsequence] of [sequence].

     For instance, [sequence_position g1 g2 f] below can be defined
     as [fsequence g1 (fsequence g2 (empty_pos f'))].
     where [f' = fun b p b' p' a2 a1 = f b p b' p' a1 a2] to give
     the result of g1 and g2 in the expected order.
 *)
(*val empty_pos : 'a fpos -> 'a grammar*)

(** [fail ()] is a grammar that always fail, whatever the input. *)
val fail : unit -> 'a grammar

(** [black_box fn cs accept_empty name] is a grammar that uses the
    function [fn] to parses the input buffer. [fn buf pos] should
    start parsing [buf] at position [pos], and return a couple
    containing the new buffer and position of the first unread
    character. The character set [cs] must contain at least the
    characters that are accepted as first character by [fn], and no
    less. The boolean [accept_empty] must be true if the function
    accept the empty string. The [name] argument is used for reference
    in error messages. Note that the functon [fn] should use [give_up ()]
    in case of a parse error.

    WARNING: fn must return a triple (x,b,p) when (b,p) is normalized,
    which means 0 <= p < Input.line_num b. You can use Input.normalize to
    ensure this. *)
(*val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool
                   -> string -> 'a grammar*)

(** [debug msg] is a dummy grammar that always succeeds and prints [msg]
    on [stderr] when used. It is useful for debugging. *)
(*val debug : string -> unit grammar*)

(** [regexp ?name re] is a grammar that uses the regexp [re] to parse
    the input buffer. The value returnes is the array of the contents
    of the groups. *)
val regexp : ?name:string -> string -> string array grammar

(** {2 Blanks management} *)

(** [no_blank] is a [blank] function that does not discard any character
    of the input buffer. *)
val no_blank : blank

(** [blank_regexp re] builds a blank from the regexp [re]. *)
(*val blank_regexp : string -> blank*)

(** [blank_grammar gr bl] produces a [blank] function using the  grammar
    [gr] and the [blank] function [bl]. It parses as much of  the  input
    as possible using the grammar [gr] with the [blank]  function  [bl],
    and returns the reached position. *)
(*val blank_grammar : unit grammar -> blank -> blank*)

(** [change_layout ~old_blank_before  ~new_blank_after  gr bl]  replaces
    the current  [blank]  function with [bl],  while  parsing  using the
    grammar [gr].  The optional  parameter [old_blank_before] ([true] by
    default)  forces the application of the old blank  function,  before
    starting to parse with  [gr].  Note  that the new blank  function is
    always called before the first terminal of [gr]. Similarly, the opt-
    -ional parameter [new_blank_after] ([true] by default) forces a call
    to the new blank function after the end of the parsing of [gr]. Note
    that the old blank function is always called after the last terminal.
*)
(*val change_layout : ?old_blank_before:bool -> ?new_blank_after:bool
                      -> 'a grammar -> blank -> 'a grammar*)


(** [change_layout ~oba gr bl] same as abobe but with no blank.  It
    keeps the first char prediction and is therefore more efficient *)
(*val no_blank_layout : 'a grammar -> 'a grammar*)

(** {2 Support for recursive grammars} *)

(** [declare_grammar name] returns a new grammar that can be used in the
    definition of other grammars, but that cannot be run on input before
    it has been initialized with [set_grammar]. The [name]  argument  is
    used for reference to the grammar in error messages. *)
val declare_grammar : string -> 'a grammar

(** [set_grammar gr grdef] set the definiton of grammar [gr] (previously
    declared with [declare_grammar]) to be  [grdef].  [Invalid_argument]
    is raised if [set_grammar] is used on a grammar that was not created
    with [declare_grammar]. The behavious is undefined if a  grammar  is
    set twice with [set_grammar]. *)
val set_grammar : 'a grammar -> 'a grammar -> unit

(** {2 Parsing functions} *)

(** [parse_buffer gr bl buf] parses the buffer [buf] using  the  grammar
    [gr] and the blank function [bl]. The exception [Parse_error] may be
    raised in case of error. *)
val parse_buffer : 'a grammar -> blank -> buffer -> 'a

(** [parse_string ~filename gr bl str] parses the string [str] using the
    grammar [gr] and the blank function [bl]. An optional [filename] can
    be provided for reference  to  the  input  in  error  messages.  The
    exception [Parse_error] may be raised in case of error. *)
val parse_string : ?filename:string -> 'a grammar -> blank
                     -> string -> 'a

(** [parse_channel ~filename gr bl ch] parses the contenst of  the  input
    channel [ch] using the grammar [gr] and the blank  function  [bl].  A
    [filename] can be provided for reference to the input in case  of  an
    error. [parse_channel] may raise the [Parse_error] exception. *)
val parse_channel : ?filename:string -> 'a grammar -> blank
                      -> in_channel -> 'a

(** [parse_file gr bl fn] parses the file [fn] using the grammar [gr] and
    the blank function [bl]. The exception [Parse_error] may be raised in
    case of error. *)
val parse_file : 'a grammar -> blank -> string -> 'a

(** [partial_parse_buffer gr bl buf pos] parses  input  from  the  buffer
    [buf] starting a position [pos], using the grammar [gr] and the blank
    function [bl]. A triple is returned containing the  new  buffer,  the
    position that was reached during parsing, and the semantic result  of
    the parsing. The optional argument [blank_after], [true] by default,
    indicates if the returned position if after the final blank or not.
    Note that this function should not be used in the  defi-
    nition of a grammar using the [black_box] function. *)
val partial_parse_buffer : 'a grammar -> blank -> ?blank_after:bool ->
                            buffer -> int -> 'a * buffer * int

(** A functor providing support for using and [Input] preprocessor. *)
(*module WithPP : functor (PP : Preprocessor) ->
  sig
    (** [parse_string] is the same as [Earley.parse_string] but  it  uses
        the preprocessor defined by [PP]. *)
    val parse_string : ?filename:string -> 'a grammar -> blank
                         -> string -> 'a

    (** [parse_channel] is the same as [Earley.parse_channel] but it uses
        the preprocessor defined by [PP]. *)
    val parse_channel : ?filename:string -> 'a grammar -> blank
                          -> in_channel -> 'a

    (** [parse_file] is the same as [Earley.parse_file] but it  uses  the
        preprocessor defined by [PP]. *)
    val parse_file : 'a grammar -> blank -> string -> 'a
  end*)

(** {2 Debuging and flags} *)

(** [debug_lvl] is a flag that can be set for [Earley] to display  debug
    data on [stderr]. The default value is [0], and bigger numbers acti-
    vate more and more debuging informations. *)
val debug_lvl : int ref

(** [warn_merge] is a flag that is used to choose whether  warnings  are
    displayed or not when an ambiguity is encountered while parsing. The
    default value is [true]. *)
val warn_merge : bool ref

(** [keep_all_names] is false by default and allow for inlining  grammar
    with a name to optimise parsing.  When debugging,  it is possible to
    set it to true  (before all grammar constructions) for more accurate
    messages. *)
(*val keep_all_names : bool ref*)

(** {2 Greedy combinator} *)

(** [greedy g] parses g in a greedy way: only the longest match is considered.
    Still ambigous if the longest match is not unique *)
val greedy : 'a grammar -> 'a grammar

(** {2 Sequencing combinators} *)

(** [sequence g1 g2 f] is a grammar that first parses using [g1], and then
    parses using [g2]. The results of the sequence is then obtained by applying
    [f] to the results of [g1] and [g2]. *)
val sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** [sequence_position g1 g2 f] is a grammar that first parses using [g1], and
    then parses using [g2]. The results of the sequence is then obtained by
    applying [f] to the results of [g1] and [g2], and to the positions (i.e.
    buffer and index) of the corresponding parsed input.

    Remark: [sequence g1 g2 f] is equivalent to
    [sequence_position g1 g2 (fun _ _ _ _ -> f)]. *)
(*val sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) fpos
  -> 'c grammar*)

(** [fsequence g1 g2] is a grammar that first parses using [g1], and then
    parses using [g2]. The results of the sequence is then obtained by applying
    the result of [g1] to the result of [g2].

    Remark: [fsequence g1 g2] is equivalent to
    [sequence g1 g2 (fun x f -> f x)]. *)
val fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

(** same as fsequence, but the result of [g2] also receive the position of the
    result of [g1]. *)
(*val fsequence_position : 'a grammar -> ('a -> 'b) fpos grammar -> 'b grammar*)


(** same as fsequence, but the result of [g2] receives nothing, meaning
    we forget the result of [g1]. *)
val fsequence_ignore : 'a grammar -> 'b grammar -> 'b grammar

(** [sequence3] is similar to [sequence], but it composes three grammars into
    a sequence.

    Remark: [sequence3 g1 g2 g3 f] is equivalent to
    [sequence (sequence g1 g2 f) g3 (fun f x -> f x)]. *)
val sequence3 : 'a grammar -> 'b grammar -> 'c grammar
  -> ('a -> 'b -> 'c -> 'd) -> 'd grammar

(** [simple_dependent_sequence g1 g2] is a grammar that first parses using [g1],
    which returns a value [a], and then continues to parse with [g2 a] and
    return its result. *)
val simple_dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar

(** [dependent_sequence g1 g2] is a grammar that first parses using [g1],
    which returns a value [(a,b)], and then continues to parse with [g2 a] and
    return its result applied to [b]. compared to the above function, allow
    memoizing the second grammar *)
(*val dependent_sequence: ('a * 'b) grammar ->
                        ('a -> ('b -> 'c) grammar) -> 'c grammar*)

(*val iter : 'a grammar grammar -> 'a grammar*)
(**  = fun g -> dependent_sequence g (fun x -> x) *)


(** [option v g] tries to parse the input as [g], and returns [v] in case of
    failure. *)
(*val option : 'a -> 'a grammar -> 'a grammar*)

val fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
(** [fixpoint v g] parses a repetition of one or more times the input parsed
    by [g]. The value [v] is used as the initial value (i.e. to finish the
    sequence).

    if parsing X with g returns a function gX, parsing X Y Z with fixpoint a g
    will return gX (gY (gZ a)).

    This consumes stack proportinal to the input length ! use revfixpoint ...
*)

(*val fixpoint' : 'a -> 'b grammar -> ('b -> 'a -> 'a) -> 'a grammar*)

(*val fixpoint1 : 'a -> ('a -> 'a) grammar -> 'a grammar*)
(** as [fixpoint] but parses at leat once with the given grammar *)

(*val fixpoint1' : 'a -> 'b grammar -> ('b -> 'a -> 'a) -> 'a grammar*)

(** [listN g sep] parses sequences of [g] separated by  [sep] of length at
    least [N], for [N=0,1] or [2]. *)
(*val list0 : 'a grammar -> unit grammar -> 'a list grammar
val list1 : 'a grammar -> unit grammar -> 'a list grammar
val list2 : 'a grammar -> unit grammar -> 'a list grammar*)

(** [alternatives [g1;...;gn]] tries to parse using all the grammars
    [[g1;...;gn]] and keeps only the first success. *)
val alternatives : 'a grammar list -> 'a grammar

(** [apply f g] applies function [f] to the value returned by the grammar
    [g]. *)
val apply : ('a -> 'b) -> 'a grammar -> 'b grammar

(** [apply_position f g] applies function [f] to the value returned by the
    grammar [g] and the positions at the beginning and at the end of the
    input parsed input. *)
(*val apply_position : ('a -> 'b) fpos -> 'a grammar -> 'b grammar*)

(** [position g] tranforms the grammar [g] to add information about the
    position of the parsed text. *)
(*val position : 'a grammar -> (string * int * int * int * int * 'a) grammar*)

(** [test c f] perform a test [f] on the input buffer. Do not parse
    anything (position are unchanged). The charset [c] should contains
    all character accepted as at the position given to f *)
(*val test : ?name:string -> Charset.t ->
           (buffer -> int -> ('a * bool)) -> 'a grammar*)

(** [blank_test c f] same as above except that [f] is applied to
    [buf' pos' buf pos] where [(buf', pos')] is the position before the
    blank. The charset c should contains all character accepted as at
    the position (buf,pos). This allow to test the presence of blank
    or even to read the blank and return some information *)
(*val blank_test : ?name:string -> Charset.t -> ('a * bool) fpos -> 'a grammar*)

(** a test that fails if there is no blank *)
(*val with_blank_test : 'a -> 'a grammar*)

(** a test that fails if there are some blank *)
(*val no_blank_test : 'a -> 'a grammar*)

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

(** Similar to the previous one, with an optimization.
    [grammar_prio to_str name] returns a pair [(gs, set_gs)], where
    [gs] is a finite family of grammars parametrized by a value of type ['a].
    [set_gs] requires two lists of grammars to set the value of the grammar:
    - the first list are grammar that can only be activated by the parameter
      (if the given function return true)
    - the second list is used as for grammar family *)
val grammar_prio : ?param_to_string:('b -> string) -> string
     -> ('b -> 'c grammar) *
          ((('b -> bool) * 'c grammar) list * ('b -> 'c grammar list) -> unit)

(** A mixture of the two above *)
val grammar_prio_family
    : ?param_to_string:('a * 'b -> string) -> string
     -> ('a -> 'b -> 'c grammar) *
          (('a -> (('b -> bool) * 'c grammar) list
                  * ('b -> 'c grammar list)) -> unit)

(** [accept_empty g] returns [true] if the grammar [g] accepts the empty input
    and [false] otherwise. *)
val accept_empty : 'a grammar -> bool

val grammar_info : 'a grammar -> bool * Charset.t



(** give a name to the grammar. Usefull for debugging. *)
val give_name : string -> 'a grammar -> 'a grammar
