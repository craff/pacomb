module Option = struct
  let map : ('a -> 'b) -> ('a option -> 'b option) =
    fun f -> function None -> None | Some x -> Some (f x)
  let get : 'a -> 'a option -> 'a =
    fun d -> function None -> d | Some x -> x
  let get_map : ('a -> 'b) -> 'b -> 'a option -> 'b =
    fun f d -> function None -> d | Some x -> f x
end

(* Comparison function accepting to compare everything. *)
let eq_closure : type a. a -> a -> bool =
  fun f g ->
    let open Obj in
    (* repr f == repr g
       || (Marshal.to_string f [Closures] = Marshal.to_string g [Closures]) *)
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
                 (* FIXME: we could certainly do better with infix_tag
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

(* Custom hash table module. [Hashtbl] won't  do  because  it  does  not
   accept keys that contain closures. Here a custom  comparing  function
   can be provided at the creation of the hash table. *)
module EqHashtbl :
  sig
    type ('a, 'b) t

    val create : int -> ('a, 'b) t
    val add    : ('a, 'b) t -> 'a -> 'b -> unit
    val find   : ('a, 'b) t -> 'a -> 'b
    val iter   : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  end =
  struct
    type ('a, 'b) t =
      { mutable nb_buckets : int
      ; mutable buckets    : ('a * 'b) list array
      ; mutable max_size   : int
      ; mutable size_limit : int }

    let rec log2 n = if n <= 0 then 0 else 1 + log2 (n lsr 1)

    let create : int -> ('a, 'b) t =
      fun nb_buckets ->
        let nb_buckets = max nb_buckets 8 in
        let buckets = Array.make nb_buckets [] in
        let size_limit = log2 nb_buckets + 7 in
        { nb_buckets ; buckets ; max_size = 0 ; size_limit }

    let iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit =
      fun fn h ->
        Array.iter (List.iter (fun (k,v) -> fn k v)) h.buckets

    let hash = Hashtbl.hash

    let find_bucket : ('a, 'b) t -> 'a -> int =
      fun h k -> hash k mod h.nb_buckets

    exception Size_is of int
    let rec add : ('a, 'b) t -> 'a -> 'b -> unit =
      fun h k v ->
        let i = find_bucket h k in
        let rec remove sz = function
          | []                             -> raise (Size_is sz)
          | (kv,_) :: ls when eq_closure k kv -> ls
          | e      :: ls                   -> e :: remove (sz+1) ls
        in
        try h.buckets.(i) <- (k,v) :: remove 0 h.buckets.(i)
        with Size_is(sz) ->
          h.buckets.(i) <- (k,v) :: h.buckets.(i);
          h.max_size <- max h.max_size sz;
          if h.max_size > h.size_limit then grow h

    and grow : ('a, 'b) t -> unit =
      fun h ->
        let old_tbl = h.buckets in
        h.nb_buckets <- h.nb_buckets * 2;
        h.buckets <- Array.make h.nb_buckets [];
        h.size_limit <- h.size_limit + 1;
        h.max_size <- 0;
        Array.iter (List.iter (fun (k,v) -> add h k v)) old_tbl

    let find : ('a, 'b) t -> 'a -> 'b =
      fun h k ->
        let i = find_bucket h k in
        let rec find = function
          | []         -> raise Not_found
          | (kv,v)::xs -> if eq_closure k kv then v else find xs
        in
        find h.buckets.(i)
  end
