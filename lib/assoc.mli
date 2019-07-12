(** Dependant association list *)

(* extensible type of key used for elimination of left recursion,
   see elim_left_rec below *)
type _ ty =  ..
type ('a,'b) eq = NEq : ('a, 'b) eq | Eq : ('a, 'a) eq
type 'a key = { k : 'a ty; eq : 'b.'b ty -> ('a,'b) eq }

type t

val new_key : unit -> 'a key

val empty : t

val add : 'a key -> 'a -> t -> t

val add_key : 'a -> t -> ('a key * t)

val find : 'a key -> t -> 'a

val mem : 'a key -> t -> bool

module type Ty = sig type 'a t end

module type S = sig
  type _ elt
  type t
  val empty : t
  val add : 'a key -> 'a elt -> t -> t
  val add_key : 'a elt -> t -> ('a key * t)
  val find : 'a key -> t -> 'a elt
  val mem : 'a key -> t -> bool
end

module Make(Ty:Ty) : S with type 'a elt = 'a Ty.t
