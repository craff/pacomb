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

(** Dependant association list *)

(* extensible type of key used for elimination of left recursion,
   see elim_left_rec below *)
type _ ty =  ..
type ('a,'b) eq = NEq : ('a, 'b) eq | Eq : ('a, 'a) eq
type 'a key = { k : 'a ty; eq : 'b.'b ty -> ('a,'b) eq }

let new_key : type a. unit -> a key = fun () ->
  let module M = struct type _ ty += T : a ty end in
  let open M in
  let eq : type b. b ty -> (a, b) eq = function T -> Eq | _ -> NEq in
  { k = T; eq }

module type Ty = sig type 'a t end

module type S = sig
  type _ elt
  type t = Nil : t | Cons : 'a key * 'a elt * t -> t
  val empty : t
  val add : 'a key -> 'a elt -> t -> t
  val add_key : 'a elt -> t -> ('a key * t)
  val find : 'a key -> t -> 'a elt
  val mem : 'a key -> t -> bool
end

module Make(Ty:Ty) = struct
  type 'a elt = 'a Ty.t

  type t = Nil : t | Cons : 'a key * 'a elt * t -> t

  let empty = Nil

  let add : 'a key -> 'a elt -> t -> t = fun k x l -> Cons(k,x,l)

  let add_key : 'a elt -> t -> ('a key * t) = fun x l ->
    let k = new_key () in (k, Cons(k,x,l))

  let find : type a.a key -> t -> a elt = fun k l ->
    let rec fn : t -> a elt = function
      | Nil -> raise Not_found
      | Cons(k',x,l) ->
         match k'.eq k.k with
         | Eq -> x
         | NEq -> fn l
    in fn l

  let mem : type a.a key -> t -> bool = fun k l ->
    let rec fn : t -> bool = function
      | Nil -> false
      | Cons(k',_,l) ->
         match k'.eq k.k with
         | Eq -> true
         | NEq -> fn l
    in fn l
end

module Idt = struct type 'a t = 'a end

include Make(Idt)
