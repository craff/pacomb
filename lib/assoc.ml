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
  val remove : 'a key -> t -> t
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

  let remove : type a.a key -> t -> t = fun k l ->
    let rec fn : t -> t = function
      | Nil -> raise Not_found
      | Cons(k',v,l) ->
         match k'.eq k.k with
         | Eq -> l
         | NEq -> Cons(k',v,fn l)
    in fn l

end

module Idt = struct type 'a t = 'a end

include Make(Idt)
