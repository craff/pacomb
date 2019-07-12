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

val find : 'a key -> t -> 'a

val mem : 'a key -> t -> bool
