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

(** {1 Combinator library, using continuation}

    As usual left recursion is not supported, but the library is intended to
    be used through the [Grammar] module that provides elimination of left
    recursion. However, a cache combinatr is supported to overcome the cost
    of backtracking. *)

(** {2 function and type usefull to the end-user} *)

(** The type of combinator *)
type 'a combinator

(** Abbreviation *)
type 'a t = 'a combinator

(** [give_up ()] will reject the current parsing rule from the action code *)
val give_up : unit -> 'a

(** Exception raised by the function below when parsing fails *)
exception Parse_error of Input.buffer * int

(** [handle_exception fn v] applies the function [fn] to [v] and handles
    the [Parse_error] exception. In particular, a parse error message is
    presented to the user in  case  of  a  failure, then [error ()]
    is called. The default [error] is [fun () -> exit 1]. *)
val handle_exception : ?error:(unit -> 'b) -> ('a -> 'b) -> 'a -> 'b

(** Parse a whole input buffer. the eof combinator is added at
    the end of the given combinator *)
val parse_buffer : 'a t -> Lex.blank -> Input.buffer -> 'a

(** Parse a whole string *)
val parse_string : 'a t -> Lex.blank -> string -> 'a

(** Parse a whole input channel *)
val parse_channel : 'a t -> Lex.blank -> in_channel -> 'a

(** Partial parsing. Beware, the returned position is not the maximum position
    that can be reached by the grammar *)
val partial_parse_buffer : 'a t -> Lex.blank -> ?blank_after:bool
                           -> Input.buffer -> int -> 'a * Input.buffer * int

(** Returns all possible parse tree. Usefull for natural language but also
    to debug ambiguity in a supposed non ambiguous grammar. *)
val parse_all_buffer : 'a t -> Lex.blank -> Input.buffer -> 'a list

(** {2 combinator constructors, normally not needed by the casual user } *)

(** Always fails *)
val cfail : 'a t

(** Accepting the empty input only *)
val cempty : 'a -> 'a t

(** Accepts a given terminal *)
val cterm : 'a Lex.fterm -> 'a t

(** Sequence of two combinators, parses with the first and then parses the
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
val clpos : (Position.t -> 'a) t -> 'a t

(** To eliminate left recursion, lpos has to be left factored.
    if lpos is one single combinator, this adds a lot of closures
    in action code. To solve this problem, lpos is splitted in
    two combinators, one that pushes the position to a stack and pops
    after parsing and another that reads the position. *)
val cpush : 'a t -> 'a t
val cread : int -> (Position.t -> 'a) t -> 'a t

(** Same as above with the position to the right *)
val crpos : (Position.t -> 'a) t -> 'a t

(** [cls c1 c2] is an optimized version of [let rec r = seq c1 (seq r c2)]
    which is illegal as it is left recursive and loops. The optional charset indicates
    the characteres accepted by [c2] at the beginning of input. *)
val clr : ?cs2:Charset.t -> 'a t -> ('a -> 'a) t -> 'a t

(** Access to a reference to a combinator, use by Grammar.compile
    for recursive grammars (not for left recursion *)
val cref : 'a t ref -> 'a t

(** Change the blank function used to parse with the given combinator.
    we can choose which blank to use at the boundary with the optional
    parameters. *)
val clayout
    : ?old_before:bool -> ?new_before:bool -> ?new_after:bool -> ?old_after:bool
      -> 'a t -> Lex.blank -> 'a t

(** Combinator that caches a grammar to avoid exponential behavior.
    parsing with the grammar from each position is memoized to avoid
    parsing twice the same sequence with the same grammar. *)
val ccache : 'a t -> 'a t
