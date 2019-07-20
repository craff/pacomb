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

(** Functions managing positions *)

(** Type to represent position *)
type pos = { name : string  (** file's name *)
           ; line  : int    (** line number *)
           ; col   : int    (** column number *)
           ; utf8_col : int (** column number with unicode *)
           ; phantom : bool (** is the postion a "phantom", i.e. not really
                                in the file *) }

type t = pos

let max_pos p1 p2 =
  if p1.line > p2.line then p1
  else if p1.line < p2.line then p2
  else if p1.col < p2.col then p2
  else p1

let phantom = { name = ""; line = 0; col  = 0; utf8_col = 0; phantom = true }

(** if false (the default) [utf8_col] field is set to [-1] by [get_pos] *)
let compute_utf8_col = ref false

(** build a position from an input buffer and a column number *)
let get_pos : Input.buffer -> int -> pos = fun b n ->
  let open Input in
  { name = filename b;
    line = line_num b;
    col = n;
    utf8_col = if !compute_utf8_col then utf8_col_num b n else (-1);
    phantom = false
  }
