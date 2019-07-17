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

module Option :
  sig
    val map : ('a -> 'b) -> ('a option -> 'b option)
    val get : 'a -> 'a option -> 'a
    val get_map : ('a -> 'b) -> 'b -> 'a option -> 'b
  end

module EqHashtbl :
  sig
    type ('a, 'b) t

    val create : int -> ('a, 'b) t
    val add    : ('a, 'b) t -> 'a -> 'b -> unit
    val find   : ('a, 'b) t -> 'a -> 'b
    val iter   : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  end
