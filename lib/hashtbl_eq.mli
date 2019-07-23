(** Custom hash-table module. *)

(** The [Hashtbl] module (of the standard library) does not work when keys can
    contain closures, since they are compared with polymorphic equality [(=)].
    For memoization though, using a perfect equality test is not important. In
    other words, it does not really matter if the equality test produces false
    negatives when comparing closures. We thus use an alternative, polymorphic
    equality function that behaves as [(=)], but compares closures with [(==)]
    instead of failing. *)

(** Representation of a hash table with keys of type ['a] and elements of type
    ['b], with a custom equality test. *)
type ('a, 'b) t

(** [create ~eq_key n] creates an empty hash table with initial size n (if [n]
    is not reasonable then a default value is used),  and using [eq_key] as an
    equality test for keys. If no [eq_key] is given, the function described in
    the preamble of this module is used. *)
val create : ?eq_key:('a -> 'a -> bool) -> int -> ('a, 'b) t

(** [add htbl k v] extends the hash table [htbl] with a new binding of key [k]
    to value [v]. Any previous binding for key [k] is removed. *)
val add : ('a, 'b) t -> 'a -> 'b -> unit

(** [find htbl k] returns the value maped to [k] in the hash table [htbl]. The
    exception [Not_found] is raised if there is no such binding. *)
val find : ('a, 'b) t -> 'a -> 'b

(** [iter f htbl] calls [f k v] for every binding of a key [k] to a value [v],
    in the hash table [htbl]. The order of iteration is unspecified. *)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
