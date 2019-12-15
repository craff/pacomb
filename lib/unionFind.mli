(* type a union find equivalent classes with data of type 'a attached *)
type 'a t

(* creationg of a new equivalent class *)
val root : 'a -> 'a t

(* get the value and root of an equivalent class *)
val find : 'a t -> ('a * 'a t)

(* merge 2 equivalent class *)
val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> unit

(* set the root of a root element of an equivalent class.
   raise Invalig_argument "set_root on non root" is the
   first argument is not a root (i.e. the result of find *)
val set_root : 'a t -> 'a -> unit
