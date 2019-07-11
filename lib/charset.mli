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

(** A module providing efficient character sets. *)

(** {2 Type} *)

(** The abstract type for a character set. *)
type charset

(** Synonym of [charset]. *)
type t = charset

(** {2 Charset construction} *)

(** The empty character set. *)
val empty : charset

(** The full character set. *)
val full : charset

(** [singleton c] returns a charset containing only [c]. *)
val singleton : char -> charset

(** [range cmin cmax] returns the charset containing all the  characters
    between [cmin] and [cmax]. *)
val range : char -> char -> charset

(** [from_string s] returns the charset corresponding to the description
    string [s],  which may contain standalone characters (different from
    ['-'], which is only allowed as first character) or ranges. They are
    build of start and end characters, separated by ['-']. An example of
    a valid description is ["-_a-zA-Z0-9"]. Note that [Invalid_argument]
    is raised in case of ill-formed description. *)
val from_string : string -> charset

(** [union cs1 cs2] builds a new charset that contins the union  of  the
    characters of [cs1] and [cs2]. *)
val union : charset -> charset -> charset

(** [complement cs] returns a new charset containing exactly  characters
    that are not in [cs]. *)
val complement : charset -> charset

(** [add cs c] returns a new charset containing the characters  of  [cs]
    and the character [c]. *)
val add : charset -> char -> charset

(** [del cs c] returns a new charset containing the characters  of  [cs]
    but not the character [c]. *)
val del : charset -> char -> charset

(** {2 Membership test} *)

(** [mem cs c] tests whether the charset [cs] contains [c]. *)
val mem : charset -> char -> bool

(** {2 Printing and string representation} *)

(** [print oc cs] prints the charset [cs] to the output channel [oc].  A
    compact format is used for printing: common ranges are used and full
    and empty charsets are abreviated. *)
val print : out_channel -> charset -> unit

(** [print_full oc cs] is the same as [print oc cs] but it does not  use
    abreviations (i.e. all characters are displayed). *)
val print_full : out_channel -> charset -> unit

(** [show oc cs] builds a string representing the charset [cs] using the
    same compact format as [print]. *)
val show : charset -> string

(** [show_full oc cs] is the same as [show oc cs] but it  does  not  use
    abreviations (i.e. all characters appear). *)
val show_full : charset -> string

(** {2 Manipulating charsets imperatively} *)

(** [copy cs] make a copy of the charset [cs]. *)
val copy : charset -> charset

(** [addq cs c] adds the character [c] to the charset [cs].  Users  must
    be particularly careful when using this function. In particular,  it
    should not be used directly on [empty], [full] or the result of  the
    [singleton] function as it would change their value permanently.  It
    is advisable to prefer the use of [add] or to work on a [copy]. *)
val addq : charset -> char -> unit

(** [delq cs c] deletes the character [c] from the charset [cs]. Similar
    recomendatiosn as for [addq] apply. *)
val delq : charset -> char -> unit
