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

(** A small module for efficient regular expressions. *)

open Input

(** Type of a regular expression. *)
type regexp =
  | Chr of char        (* Single character.                *)
  | Set of Charset.t   (* Any character in a charset.      *)
  | Seq of regexp list (* Sequence of regular expressions. *)
  | Alt of regexp list (* Alternative between regexps.     *)
  | Opt of regexp      (* Optional regexp.                 *)
  | Str of regexp      (* Zero or more times the regexp.   *)
  | Pls of regexp      (* One  or more times the regexp.   *)
  | Sav of regexp      (* save what is read    *)

type t = regexp

(** Exception that is raised when a regexp cannot be read. *)
exception Regexp_error of buffer * int

val print_regexp : out_channel -> regexp -> unit

val accept_empty : regexp -> bool

val accepted_first_chars : regexp -> Charset.t

val regexp_from_string : string -> regexp

(** [read_regexp re buf pos] attempts to parse using the buffer [buf] at
    position [pos] using the regular expression [re]. The return value is
    a triple of the parsed string, the buffer after parsing and the
    position after parsing. The exception [Regexp_error(err_buf, err_pos]
    is raised in case of failure at the given position. *)
val read_regexp : regexp -> buffer -> int -> string list * buffer * int
