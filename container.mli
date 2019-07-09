(** This library provide a type [Container.t], which can be used as a
    list of polymorphic references.

    Another way to see it, is map with access time in O(N) where
    N is the number of tables! Note: O(1) is possible, but usually
    there are very few table at the same time.

    The typical use case is to have a record with a field of type
    [Container.t]. Then when you want to store some information in
    that field of type [a], you create with [Container.create_table],
    a value of type [a Container.table]. The with [Container.add]
    and [Container.find], you can store value in your field of
    type [Container.t].

    More precisely, consider the following type for oriented graphs:
{[
    type node = { name: string;
                  mutable next: node list;
                  ptrs : Container.t }
    type graph = node list (* at least on node per component *)
]}
    If you want to traverse the graphe, you create a table
    to associate a boolean to each note:
{[
    let iter graph f =
      let visited : bool Container.table = Container.create_table 101 in
      ....
      (* the table is automatically freed when visited is collected *)
]}
    If you want to compute the distance between two nodes:
{[
    let distance a b =
      let distance_to_a : int Container.table = Container.create_table 101 in
      ....
]}

    The functorial interface is useful when you have a parametric
    type.  Considier a record type [ 'a t ] to which you want to
    associate values of type [ ('b,'a) v ]. It is enough for this to
    call the functor with

    [ module M = Constainer.Make(struct type ('a,'b) v end) ]

    and have a field of type [ 'a M.container ] inside your record.
    Then, you use the type [ 'b M.table ] when you want to start to
    associate values of type [ ('a,'b) v ] to the record. The same
    module [ M ] can be used for many types [ 'b ].

    Remark: the non funtorial version is just defined by:
      [ Container.Make(struct type ('a, 'b) elt = 'a end ]

 *)

(** Type of a container cell *)
type t

(** Type of a container table. You must create a container table of
    type [ a table ] to store value of type [ a ] in a container
    cell. Many table can have the same type, the value are associated
    to the pair (cell, table), not just to the cell. *)
type 'a table

(** [ create () ] creates a new container cell *)
val create : unit -> t

(** [ add tab cell v ] associates the value v to the pair (tab, cell).
    complexity if O(N) where N is the number of tables with a value
    associated to this cell. *)
val add : 'a table -> t -> 'a -> unit

(** [ find tab cell ] return the value associated to (tab, cell).
    raises Not_found if the are no such value *)
val find : 'a table -> t -> 'a

(** [ clear tab ] removed all value associated to a table. *)
val clear : 'a table -> unit

(** [ create_table () ] creates a new table *)
val create_table : unit -> 'a table

(** GADT to represent types in the syntax (extended when needed). *)
type _ tag = ..

(** [ address n ] return a unique id of each cell *)
val address   : t -> unit tag

(** Standard eq-type. *)
type ('a,'b) eq =
  | Y : ('a,'a) eq
  | N : ('a,'b) eq

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

val eq : t -> t -> bool

module Make(T:sig type ('a,'b) elt end) : Param
       with type ('a, 'b) elt = ('a,'b) T.elt

type ('a, 'b) elt = 'a

type 'a iter = { f : 'b.('a, 'b) elt -> unit }
val iter : 'a iter -> 'a table -> unit
type ('a,'c) fold = { f : 'b.('a, 'b) elt -> 'c -> 'c }
val fold : ('a, 'c) fold -> 'a table -> 'c -> 'c

module Ref : Param with type ('a,'b) elt = 'b
