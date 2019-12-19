
module Make(T:Set.OrderedType) : sig
  type elt

  (** type of a priority heap, holding elements of type 'a *)
  type 'a t

  (** empty heap *)
  val empty : 'a t

  (** [add p x h] add [x] to the heap [h] with the priority [p] *)
  val add : T.t -> 'a -> 'a t -> 'a t

  (** [remove h] return an element with the least priority from the heap and the
     resulting heap. Semantics is unspecified for equal priorities *)
  val remove : 'a t -> ('a * 'a t)
end
