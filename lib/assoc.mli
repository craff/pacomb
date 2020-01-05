(** Dependant association lists. *)

(** Standard equality type using a GADT. *)
type ('a, 'b) eq =
  | Eq  : ('a, 'a) eq
  | NEq : ('a, 'b) eq

(** Type of tokens used to make keys unique, and carrying a type. This type is
    not intended to be extended by the user, hence it is private... but not
    declared priveta as it fails if 4.04 *)
type _ token = ..

(** Type of a key for a value of type ['a]. It contains a unique token and the
    corresponding (very efficient) equality test. *)
type 'a key = { tok : 'a token ; uid:int; eq : 'b. 'b token -> ('a, 'b) eq }

(** To store keys in lists *)
type any_key = K : 'a key -> any_key [@@unboxed]

(** [new_key ()] generates a new unique key for a value of type ['a]. *)
val new_key : unit -> 'a key

(** Type of an association list, where items may have different types *)
type t

(** [empty] is the empty association list. *)
val empty : t

(** compare keys by uid *)
val compare : 'a key -> 'b key -> int

(** [add k v l] inserts a new binding of [k] to [v] at the head of [l]. A previous
    binding of [k] will not be removed. Hence removing [k] will uncover a previous
    binding. *)
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

(** [replace k l] replaces a previous binding if it exists ]. If two bindings
    existed, only the first is removed to be replaced. *)
val replace : 'a key -> 'a -> t -> t

(** [append l1 l2] concatenate the two association lists. Duplicated are not
    removed. *)
val append : t -> t -> t

(** Iterator *)
type iter = { f : 'a. 'a key -> 'a -> unit }
val iter : iter -> t -> unit

(** Variation on the above to associate value of type ['a data] to key of type
   ['a key]. The abobe function are obatained with
   [Make(struct type 'a data = 'a end)] *)
module Make(T:sig type 'a data end) :
sig
  type t
  val empty : t

  val add : 'a key -> 'a T.data -> t -> t
  val length : t -> int
  val add_key : 'a T.data -> t -> 'a key * t
  val find : 'a key -> t -> 'a T.data
  val mem : 'a key -> t -> bool
  val remove : 'a key -> t -> t
  val replace : 'a key -> 'a T.data -> t -> t
  val append : t -> t -> t

  type iter = { f : 'a. 'a key -> 'a T.data -> unit }
  val iter : iter -> t -> unit
end
