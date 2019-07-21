module Option :
  sig
    val map : ('a -> 'b) -> ('a option -> 'b option)
    val get : 'a -> 'a option -> 'a
    val get_map : ('a -> 'b) -> 'b -> 'a option -> 'b
  end

module EqHashtbl :
  sig
    type ('a, 'b) t

    val create : int -> ('a, 'b) t
    val add    : ('a, 'b) t -> 'a -> 'b -> unit
    val find   : ('a, 'b) t -> 'a -> 'b
    val iter   : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  end
