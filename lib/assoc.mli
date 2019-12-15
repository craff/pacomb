(** Dependant association lists. *)

(** Standard equality type using a GADT. *)
type ('a, 'b) eq =
  | Eq  : ('a, 'a) eq
  | NEq : ('a, 'b) eq

(** Type of tokens used to make keys unique, and carrying a type. This type is
    not intended to be extended by the used, hence it is private. *)
type _ token = ..

(** Type of a key for a value of type ['a]. It contains a unique token and the
    corresponding (very efficient) equality test. *)
type 'a key = { tok : 'a token ; uid:int; eq : 'b. 'b token -> ('a, 'b) eq }

(** To store keys in lists *)
type any_key = K : 'a key -> any_key [@@unboxed]

(** [new_key ()] generates a new unique key for a value of type ['a]. *)
val new_key : unit -> 'a key

(** Type of an association list. *)
type t

(** [empty] is the empty association list. *)
val empty : t

(** compare keys by uid *)
val compare : 'a key -> 'b key -> int

(** [add k v l] inserts a new binding of [k] to [v] at the head of [l]. *)
val add : 'a key -> 'a -> t -> t

(** [length l] returns the size of the association list [l]. *)
val length : t -> int

(** [add_key v l] is equivalent to [let k = new_key () in (k, add k v l)]. *)
val add_key : 'a -> t -> 'a key * t

(** [find k l] returns the latest inserted value with key [k] in list [l]. The
    exception {!exception:Not_found} is raised if there is none. *)
val find : 'a key -> t -> 'a

(** [mem k l] tells whether an element is mapped to [k] in the list [l]. *)
val mem : 'a key -> t -> bool

(** [remove k l] removes the latest inserted binding of the key [k] in [l]. If
    there is no such binding, then {!exception:Not_found} is raised. *)
val remove : 'a key -> t -> t

val append : t -> t -> t
