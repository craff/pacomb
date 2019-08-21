(** Standard eq-type. *)
type ('a,'b) eq =
  | Y : ('a,'a) eq
  | N : ('a,'b) eq

(** GADT to represent types in the syntax (extended when needed). *)
type _ tag = ..

module Make(V : sig type ('a,'b) elt end) = struct
  include V

  (** Non-uniform list (containing elements of possibly different types). *)
  type ptag = T : 'a tag -> ptag [@@unboxed]

  type 'b etag = E : 'a tag * ('a,'b) elt -> 'b etag

  type 'b nu_list = (ptag, 'b etag) Hashtbl.t


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
      {data = Hashtbl.create 16; uid = M.T; eq }

  (** Obtain the UID of a container. *)
  let address : 'b container -> 'b tag = fun c -> c.uid

  (** Equality on containers *)
  let eq : 'a container -> 'b container -> ('a, 'b) eq =
    fun c1 c2 -> c1.eq c2.uid

  (** unboxed mandatory for weak hashtbl to work, from 4.04.0 *)
  type any = C : 'b container -> any [@@unboxed]

  (** Container table. *)
  type 'a table =
    { tag  : 'a tag                   (** Unique tag for this table.   *)
    ; eq   : 'b. 'b tag -> ('a,'b) eq (** Equality to the table's tag. *)
    ; mutable elts : any list
    }

  (* Find the value associated to the given table and container. *)
  let find : type a b. a table -> b container -> (a, b) elt =
    fun tab c ->
      match Hashtbl.find c.data (T tab.tag) with E(t,v) ->
        match tab.eq t with
        | Y -> v
        | N -> assert false

  (** Insert a new value associated to the given table and container. If a
    value is already present, it is overwriten. *)
  let add : type a b. a table -> b container -> (a, b) elt -> unit =
    fun tab c v ->
      let mem = Hashtbl.mem c.data (T tab.tag) in
      Hashtbl.replace c.data (T tab.tag) (E(tab.tag, v));
      if not mem then tab.elts <- C c :: tab.elts

  let clear : type a. a table -> unit = fun tab ->
    List.iter (fun (C c) -> Hashtbl.remove c.data (T tab.tag)) tab.elts;
    tab.elts <- []

  let create_table : type a. unit -> a table = fun () ->
    let module M = struct type _ tag += T : a tag end in
    let eq : type b. b tag -> (a, b) eq = function M.T -> Y | _ -> N in
    let res = { tag  = M.T ; eq ; elts = [] } in
    Gc.finalise clear res;
    res

  let length : type a. a table -> int = fun tab ->
    let n = ref 0 in
    let fn : type b. b nu_list -> unit = fun data ->
      Hashtbl.iter (fun (T t) _ ->
          match tab.eq t with
          | Y -> incr n;
          | N -> assert false) data
    in
    List.iter (fun (C c) -> fn c.data) tab.elts;
    !n

  type 'a iter = { f : 'b.('a, 'b) elt -> unit }

  let iter : type a. a iter -> a table -> unit = fun f tab ->
    let fn : type b. b nu_list -> unit = fun data ->
      Hashtbl.iter (fun _ (E (t,v)) ->
          match tab.eq t with
          | Y -> f.f v;
          | N -> assert false) data
    in
    List.iter (fun (C c) -> fn c.data) tab.elts

  type ('a,'c) fold = { f : 'b.('a, 'b) elt -> 'c -> 'c }

  let fold : type a c. (a, c) fold -> a table -> c -> c = fun f tab acc ->
    let fn : type b. b nu_list -> c -> c = fun data acc ->
      Hashtbl.fold (fun _ (E (t,v)) acc ->
          match tab.eq t with
          | Y -> f.f v acc
          | N -> assert false) data acc
    in
    List.fold_left (fun acc (C c) -> fn c.data acc) acc tab.elts

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
  val length : 'a table -> int
  type 'a iter = { f : 'b.('a, 'b) elt -> unit }
  val iter : 'a iter -> 'a table -> unit
  type ('a,'c) fold = { f : 'b.('a, 'b) elt -> 'c -> 'c }
  val fold : ('a, 'c) fold -> 'a table -> 'c -> 'c
end

type ('a, 'b) el = 'a
include Make(struct type ('a, 'b) elt = ('a, 'b) el end)

(* redefine iter and fold, to avoid the useless record but also to avoid
   https://caml.inria.fr/mantis/view.php?id=7636
 *)
let iter : type a. (a -> unit) -> a table -> unit = fun f tab ->
  let fn : type b. b nu_list -> unit = fun data ->
  Hashtbl.iter (fun _ (E (t,v)) ->
      match tab.eq t with
      | Y -> f v;
      | N -> assert false) data
  in
  List.iter (fun (C c) -> fn c.data) tab.elts

let fold : type a c. (a -> c -> c) -> a table -> c -> c = fun f tab acc ->
  let fn : type b. b nu_list -> c -> c = fun data acc ->
    Hashtbl.fold (fun _ (E (t,v)) acc ->
      match tab.eq t with
      | Y -> f v acc
      | N -> assert false) data acc
  in
  List.fold_left (fun acc (C c) -> fn c.data acc) acc tab.elts

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
