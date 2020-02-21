(* Comparison function accepting to compare everything, in particular closures.
   (Marshal.to_string f [Closures] = Marshal.to_string g [Closures]) does not
   seem to work ... not enough equality detected *)
let eq_closure : type a. a -> a -> bool =
  fun f g ->
    let open Obj in
    let adone = ref [] in
    let rec fn f g =
      f == g ||
        match is_int f, is_int g with
        | true, true -> f == g
        | false, true | true, false -> false
        | false, false ->
           let ft = tag f and gt = tag g in
           if ft = forward_tag then (
             fn (field f 0) g)
           else if gt = forward_tag then (
             fn f (field g 0))
           else if ft <> gt then false
           else
           if ft = string_tag || ft = double_tag || ft = double_array_tag
             then f = g
           else if ft = abstract_tag || ft = out_of_heap_tag
                   || ft = no_scan_tag || ft = custom_tag || ft = infix_tag
                 (* FIXME #22: we could certainly do better with infix_tag
                           i.e. mutually recursive functions *)
             then f == g
           else
             size f == size g &&
               let rec gn i =
                 if i < 0 then true
                 else fn (field f i) (field g i) && gn (i - 1)
               in
               List.exists (fun (f',g') -> f == f' && g == g') !adone ||
                (List.for_all (fun (f',g') -> f != f' && g != g') !adone &&
                 (adone := (f,g)::!adone;
                  gn (size f - 1)))

    in fn (repr f) (repr g)

type ('a, 'b) t =
  { eq_key             : 'a -> 'a -> bool
  (** Equality function for keys. *)
  ; mutable nb_buckets : int
  (** Number of buckets. *)
  ; mutable buckets    : ('a * 'b) list array
  (** Array of buckets. *)
  ; mutable max_size   : int
  (** Current maximum bucket size. *)
  ; mutable size_limit : int
  (** Maximum size allowed for a bucket. *) }

(** Create an empty hash table. *)
let create : ?eq_key:('a -> 'a -> bool) -> int -> ('a, 'b) t =
    fun ?(eq_key=eq_closure) nb_buckets ->
  let rec log2 n = if n <= 0 then 0 else 1 + log2 (n lsr 1) in
  let nb_buckets = max nb_buckets 8 in
  let buckets = Array.make nb_buckets [] in
  let size_limit = log2 nb_buckets + 7 in
  { eq_key ; nb_buckets ; buckets ; max_size = 0 ; size_limit }

(** Iterates a function over the bindings of the given hash table. *)
let iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit = fun f htbl ->
  Array.iter (List.iter (fun (k,v) -> f k v)) htbl.buckets

(** Finds the bucket corresponding to the given key in the hash table. *)
let find_bucket : ('a, 'b) t -> 'a -> int = fun htbl k ->
  Hashtbl.hash k mod htbl.nb_buckets

(** Lookup function. *)
let find : ('a, 'b) t -> 'a -> 'b = fun h k ->
  let i = find_bucket h k in
  let rec find = function
    | []            -> raise Not_found
    | (kv, v) :: xs -> if h.eq_key k kv then v else find xs
  in
  find h.buckets.(i)

(** Insertion function (replacing existing binding). *)
let rec add : ('a, 'b) t -> 'a -> 'b -> unit = fun h k v ->
  (* Doubles the size of the hash table. *)
  let grow : ('a, 'b) t -> unit = fun h ->
    let old_tbl = h.buckets in
    h.nb_buckets <- h.nb_buckets * 2;
    h.buckets <- Array.make h.nb_buckets [];
    h.size_limit <- h.size_limit + 1;
    h.max_size <- 0;
    Array.iter (List.iter (fun (k,v) -> add h k v)) old_tbl
  in
  (* Removes existing binding, or returns size of bucket with exception. *)
  let exception Size_is of int in
  let rec remove sz l =
    match l with
    | []     -> raise (Size_is sz)
    | b :: l -> if h.eq_key k (fst b) then l else b :: remove (sz+1) l
  in
  (* Find the right bucket and replace the binding (if any). *)
  let i = find_bucket h k in
  try h.buckets.(i) <- (k,v) :: remove 0 h.buckets.(i) with Size_is(sz) ->
  (* Otherwise insert the new binding. *)
  h.buckets.(i) <- (k,v) :: h.buckets.(i);
  h.max_size <- max h.max_size sz;
  (* Grow the table if the bucket is too large. *)
  if h.max_size > h.size_limit then grow h
