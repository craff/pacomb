
(** type of a priority heap, holding elements of type 'a *)
type 'a t

(** empty heap *)
val empty : 'a t

(** [add cmp x h] add [x] to the heap [h] with the priority [p] *)
val add : ('a -> 'a -> int) -> 'a -> 'a t -> 'a t

(** [remove h] returns an element with  the least priority from the heap and the
   resulting heap. Semantics is unspecified for equal priorities *)
val remove : 'a t -> ('a * 'a t)
