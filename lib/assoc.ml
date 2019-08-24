type _ token =  ..

type ('a, 'b) eq =
  | Eq  : ('a, 'a) eq
  | NEq : ('a, 'b) eq

type 'a key = { tok : 'a token ; eq : 'b. 'b token -> ('a, 'b) eq }

let new_key : type a. unit -> a key = fun () ->
  let module M = struct type _ token += T : a token end in let open M in
  let eq : type b. b token -> (a, b) eq = function T -> Eq | _ -> NEq in
  { tok = T ; eq }

type t =
  | Nil  :                    t
  | Cns : 'a key * 'a * t -> t

let empty : t = Nil

let length : t -> int =
  let rec length acc l =
    match l with
    | Nil        -> acc
    | Cns(_,_,l) -> length (acc+1) l
  in
  length 0

let add : type a. a key -> a -> t -> t = fun k x l ->
  Cns(k,x,l)

let add_key : type a. a -> t -> a key * t = fun x l ->
  let k = new_key () in (k, Cns(k,x,l))

let rec find : type a. a key -> t -> a = fun k l ->
  match l with
  | Nil        -> raise Not_found
  | Cns(j,x,l) -> match j.eq k.tok with Eq -> x | NEq -> find k l

let rec mem : type a. a key -> t -> bool = fun k l ->
  match l with
  | Nil        -> false
  | Cns(j,_,l) -> match k.eq j.tok with Eq -> true | NEq -> mem k l

let rec remove : type a. a key -> t -> t = fun k l ->
  match l with
  | Nil        -> raise Not_found
  | Cns(j,v,l) -> match k.eq j.tok with Eq  -> l | NEq -> Cns(j,v,remove k l)
