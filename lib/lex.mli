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

type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a fterm = buf -> int -> 'a * buf * int
type 'a terminal = { n : string    (** name *)
                   ; f : 'a fterm  (** the terminal itself *)
                   ; c : Charset.t (** the set of characters accepted
                                       at the beginning of input *) }
type 'a t = 'a terminal

(** exception when failing,
    - can be raised (but not captured) by terminal
    - can be raised (but not captured) by action code in the grammar,
      see [Combinator.give_up]
    - will be raise and captured by [Combinator.parse_buffer] that will give
      the most advanced position
 *)
exception NoParse

(** Test wether a terminal accept the empty string. Such a terminal
   are illegal in a grammar, but may be used in combinator below to create
   terminals *)
val accept_empty : 'a t -> bool

(** Combinators to create terminals *)

(** Terminal accepting then end of a buffer only.
    remark: [eof] is automatically added at the end of a grammar by
    [Combinator.parse_buffer].
    [name] default is ["EOF"] *)
val eof : ?name:string -> 'a -> 'a t

(** Terminal accepting a given char, remark: [char '\255'] is equivalent to
    [eof]. [name] default is the given charater. *)
val char : ?name:string -> char -> 'a -> 'a t

(** Accept a character for which the test returns [true]. [name] default
    to the result of [Charset.show]. *)
val test : ?name:string -> (char -> bool) -> char t

(** Accept a character in the given charset. [name] default as in [test] *)
val charset : ?name:string -> Charset.t -> char t

(** Reject the input (raises [Noparse]) if the first character of the input
    passed the test. Does not read the character if the test fails.
    [name] default to ["^"] prepended to the result of [Charset.show]. *)
val not_test : ?name:string -> (char -> bool) -> 'a -> 'a t

(** Reject the input (raises [Noparse]) if the first character of the input
    is in the charset. Does not read the character if not in the charset.
    [name] default as in [not_test] *)
val not_charset : ?name:string -> Charset.t -> 'a -> 'a t

(** Compose two terminals in sequence. [name] default is the concatenation
    of the two names. *)
val seq : ?name:string -> 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
val seq1 : ?name:string -> 'a t -> 'b t -> 'a t
val seq2 : ?name:string -> 'a t -> 'b t -> 'b t

(** [alt t1 t2] parses the input with [t1] or [t2].
    Contrary to grammars, terminals does not use continuations,
    if [t1] succeds, no backtrack will be performed to try [t2].
    For instance,
      [seq1 (alt (char 'a' ())
                 (seq1 (char 'a' ()) (char 'b' ())))
            (char 'c' ())]
    will reject "abc".
    If both [t1] and [t2] accept the input, longuest match is selected.
    [name] default to [sprintf "(%s)|(%s)" t1.n t2.n]. *)
val alt : ?name:string -> 'a t -> 'a t -> 'a t

(** [option x t] parses the given terminal 0 or 1 time. [x] is returned if 0.
    [name] defaults to [sprintf "(%s)?" t.n]. *)
val option : ?name:string -> 'a -> 'a t -> 'a t

(** Applies a function to the result of the given terminal.
    [name] defaults to the terminal name. *)
val appl : ?name:string -> ('a -> 'b) -> 'a t -> 'b t

(** [star t a f] Repetition of a given terminal 0,1 or more times.
    The type of function to compose the action allows for ['b = Buffer.t]
    for efficiency. The returned value is
    [f ( ... (f(f (a ()) x_1) x_2) ...) x_n]if [t] returns [x_1] ... [x_n].
    The [name] defaults to [sprintf "(%s)*" t.n] *)
val star : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t

(** Same as above but parses at least once .*)
val plus : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t

(** [string s] Accepts only the given string.
    Raises [Invalid_argument] if [s = ""].
    [name] defaults to [sprintf "%S" s]. *)
val string : ?name:string -> string -> 'a -> 'a t

(** Parses an integer in base 10. ["+42"] is accepted.
    [name] defaults to ["INT"] *)
val int : ?name:string -> unit -> int t

(** Parses a float in base 10. [".1"] is not accepted ["0.1"] is.
    [name] defaults to ["FLOAT"] *)
val float : ?name:string -> unit -> float t

(** [keyword ~name k cs x =
     seq ~name (string k ()) (test f ()) (fun _ _ -> x)]
    usefull to accept a keyword only when not followed by an alpha-numeric char *)
val keyword : ?name:string -> string -> (char -> bool) -> 'a -> 'a t

(** create a terminal from a regexp. Returns the whole matched string *)
val regexp : ?name:string -> Regexp.t -> string t

(** create a terminal from a regexp. Returns the groups list, last to finish
    to be parsed is first in the result *)
val regexp_grps : ?name:string -> Regexp.t -> string list t

(** Functions managing blanks *)

(** Use when you have no blank chars *)
val noblank : blank

(** Blank from a charset *)
val blank_charset : Charset.t -> blank

(** Blank from a terminal *)
val blank_terminal : 'a t -> blank

(** Functions managing positions *)

(** Functions managing positions *)

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; utf8_col : int (** column number with unicode *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

(** a phnatom position, used for grammar accepting the empty input *)
val phantom : pos

(** the max of to position (further in the file *)
val max_pos : pos -> pos -> pos

(** if false (the default) [utf8_col] field is set to [-1] by [get_pos] *)                   val compute_utf8_col : bool ref

(** Get a position from an input buffer and a column number *)
val get_pos : buf -> int -> pos
