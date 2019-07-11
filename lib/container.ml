(** Standard eq-type. *)
type ('a,'b) eq =
  | Y : ('a,'a) eq
  | N : ('a,'b) eq

(** GADT to represent types in the syntax (extended when needed). *)
type _ tag = ..

module Make(V:sig type ('a,'b) elt end) = struct
  include V

  (** Non-uniform list (containing elements of possibly different types). *)
  type 'b nu_list =
    | Cons : 'a tag * ('a,'b) elt * 'b nu_list -> 'b nu_list
    | Nil  : 'b nu_list

  (** Actual container. *)
  type 'a container =
    { mutable data : 'a nu_list (** Contents for each table. *)
    ;         uid  : 'a tag     (** Unique identifier.     *)
    ;         eq   : 'b. 'b tag -> ('a,'b) eq}

  (** Creation function for containers. *)
  let create : type a. unit -> a container =
    fun () ->
      let module M = struct type _ tag += T : a tag end in
      let eq : type b. b tag -> (a, b) eq = function M.T -> Y | _ -> N in
      {data = Nil; uid = M.T; eq }

  (** Obtain the UID of a container. *)
  let address : 'b container -> 'b tag = fun c -> c.uid

  (** Equality on containers *)
  let eq : 'a container -> 'b container -> ('a, 'b) eq =
    fun c1 c2 -> c1.eq c2.uid

  (** unboxed mandatory for weak hashtbl to work, 4.04.0 only
      we use Obj until 4.04.0 is more spreaded *)
  (* type any = C : 'b container -> any [@@unboxed] *)
  type any = Obj.t container
  let cast : 'b container -> any = Obj.magic

  (** Container table. *)
  type 'a table =
    { tag  : 'a tag                   (** Unique tag for this table.   *)
    ; eq   : 'b. 'b tag -> ('a,'b) eq (** Equality to the table's tag. *)
    ; mutable elts : any list
    }

  (** Insert a new value associated to the given table and container. If a
    value is already pre sent, it is overwriten. *)
  let add : type a b. a table -> b container -> (a, b) elt -> unit =
    fun tab c v ->
    let rec fn = function
      | Nil           ->
         raise Exit
      | Cons(t, w, r) ->
         match tab.eq t with
         | Y -> Cons(t, v, r)
         | N -> Cons(t, w, fn r)
    in
    try
      c.data <- fn c.data
    with Exit ->
      c.data <- Cons(tab.tag, v, c.data);
      tab.elts <- cast c :: tab.elts

  (* Find the value associated to the given table and container. *)
  let find : type a b. a table -> b container -> (a, b) elt =
    let rec find : type a. a table -> b nu_list -> (a, b) elt = fun tab c ->
      match c with
      | Nil         -> raise Not_found
      | Cons(t,v,r) -> match tab.eq t with Y -> v | N -> find tab r
    in
    fun tab c -> find tab c.data

  (** Removes the given table from the given list. *)
  let rec remove_table : type a b. a table -> b nu_list -> b nu_list =
    fun tab l ->
    match l with
    | Nil           -> Nil
    | Cons(t, v, r) ->
       match tab.eq t with
       | Y -> r
       | N -> Cons(t, v, remove_table tab r)

  let clear : type a. a table -> unit = fun tab ->
    List.iter (fun c -> c.data <- remove_table tab c.data) tab.elts;
    tab.elts <- []

  let create_table : type a. unit -> a table = fun () ->
    let module M = struct type _ tag += T : a tag end in
    let eq : type b. b tag -> (a, b) eq = function M.T -> Y | _ -> N in
    let res = { tag  = M.T ; eq ; elts = [] } in
    Gc.finalise clear res;
    res

  type 'a iter = { f : 'b.('a, 'b) elt -> unit }

  let iter : type a. a iter -> a table -> unit = fun f tab ->
    let rec fn : Obj.t nu_list -> unit = function
      | Nil -> ()
      | Cons(t,v,r) ->
         begin
           match tab.eq t with
           | Y -> f.f v;
           | N -> ()
         end;
         fn r
    in
    List.iter (fun c -> fn c.data) tab.elts

  type ('a,'c) fold = { f : 'b.('a, 'b) elt -> 'c -> 'c }

  let fold : type a c. (a, c) fold -> a table -> c -> c = fun f tab acc ->
    let rec fn :  Obj.t nu_list -> c -> c = fun l acc ->
      match l with
      | Nil -> acc
      | Cons(t,v,r) ->
         let acc =
           match tab.eq t with
           | Y -> f.f v acc;
           | N -> acc
         in
         fn r acc
    in
    List.fold_left (fun acc c -> fn c.data acc) acc tab.elts

end

module type Param = sig
  type 'a table
  type 'b container
  type ('a, 'b) elt
  val create : unit -> 'b container
  val create_table : unit -> 'a table
  val address : 'b container -> 'b tag
  val eq : 'a container -> 'b container -> ('a, 'b) eq
  val add : 'a table -> 'b container -> ('a, 'b) elt -> unit
  val find : 'a table -> 'b container -> ('a, 'b) elt
  val clear : 'a table -> unit
  type 'a iter = { f : 'b.('a, 'b) elt -> unit }
  val iter : 'a iter -> 'a table -> unit
  type ('a,'c) fold = { f : 'b.('a, 'b) elt -> 'c -> 'c }
  val fold : ('a, 'c) fold -> 'a table -> 'c -> 'c
end

type ('a, 'b) el = 'a

include Make(struct type ('a, 'b) elt = ('a, 'b) el end)

type ('a, 'b) le = 'b
module Ref = Make(struct type ('a, 'b) elt = ('a, 'b) le end)

(** Exported name for [container]. *)
type t = unit container

let eq c1 c2 = match eq c1 c2 with Y -> true | N -> false

(* This does not work !
let iter : type a.(a -> unit) -> a table -> unit =
  fun f tabl ->
    iter { f = (let f : type b.(a, b) el -> unit = f in f) } tab
*)
