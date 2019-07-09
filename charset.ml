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

type charset = int array
type t = charset

let mask, shift, size =
  match Sys.word_size with
  | 32 -> 15, 4, 256 / 16
  | 64 -> 31, 5, 256 / 32
  | _  -> assert false (* Cannot happen... *)

let empty = Array.make size 0
let full  = Array.make size (-1)

let complement = Array.map ((lxor) (-1))

let mem cs c =
  let i = Char.code c in
  cs.(i lsr shift) land (1 lsl (i land mask)) <> 0

let addq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask))

let add cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask));
  cs

let range cmin cmax =
  let res = ref empty in
  for i = Char.code cmin to Char.code cmax do
    res := add !res (Char.chr i)
  done; !res

let delq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <-
    cs.(i lsr shift) land (lnot (1 lsl (i land mask)))

let del cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <-
    cs.(i lsr shift) land (lnot (1 lsl (i land mask)));
  cs

let union cs1 cs2 =
  Array.mapi (fun i x -> x lor cs2.(i)) cs1

let singleton =
  let tbl = Array.init 256 (fun i -> add empty (Char.chr i)) in
  fun c -> tbl.(Char.code c)

let copy = Array.copy

let from_string s =
  let rec build cs l =
    match l with
    | []                     -> cs
    | '-' :: '-' :: _        -> invalid_arg "bad charset description"
    | '-' :: l               -> build (add cs '-') l
    | _   :: '-' :: '-' :: _ -> invalid_arg "bad charset description"
    | c1  :: '-' :: c2  :: l -> build (union cs (range c1 c2)) l
    | c   :: l               -> build (add cs c) l
  in
  let string_to_list s =
    let l = ref [] in
    String.iter (fun c -> l := c :: !l) s;
    List.rev !l
  in
  build empty (string_to_list s)

let show cs =
  let has_range min max =
    let has_all = ref true in
    for i = (Char.code min) to (Char.code max) do
      if not (mem cs (Char.chr i)) then has_all := false
    done; !has_all
  in
  if cs = full then "<FULL>"
  else if cs = empty then "<EMPTY>"
  else
    let res = ref "" in
    let add_all min max =
      for i = min to max do
        if mem cs (Char.chr i) then
          res := !res ^ (Char.escaped (Char.chr i))
      done
    in
    let has_all_nums  = has_range '0' '9' in
    let has_all_upper = has_range 'A' 'Z' in
    let has_all_lower = has_range 'a' 'z' in
    (* Before character '0' *)
    add_all 0 (Char.code '0' - 1);
    (* Numbers. *)
    if has_all_nums then res := !res ^ "0-9"
    else add_all (Char.code '0') (Char.code '9');
    (* Before character 'A' *)
    add_all (Char.code '9' + 1) (Char.code 'A' - 1);
    (* Uppercase letters. *)
    if has_all_upper then res := !res ^ "A-Z"
    else add_all (Char.code 'A') (Char.code 'Z');
    (* Before character 'a' *)
    add_all (Char.code 'Z' + 1) (Char.code 'a' - 1);
    (* Lowercase letters. *)
    if has_all_lower then res := !res ^ "a-z"
    else add_all (Char.code 'a') (Char.code 'z');
    (* After character 'z'. *)
    add_all (Char.code 'z' + 1) 255;
    !res

let print oc cs =
  output_string oc (show cs)

let show_full cs =
  let res = ref "" in
  for i = 0 to 255 do
    if mem cs (Char.chr i) then
      res := !res ^ (Char.escaped (Char.chr i))
  done; !res

let print_full oc cs =
  output_string oc (show_full cs)
