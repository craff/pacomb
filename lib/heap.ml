
module Make(T:Set.OrderedType) = struct
  type elt = T.t

  (** we use append list for task with equal priority.  append is used in fusion
     and therefore all append in the append list come from a node in the heap,
     hence the depth of the app_list + depth of heap is O(ln(N)) were
     n is the number of distinct priorities. *)
  type 'a app_list = Nil
                   | Cons of 'a * 'a app_list
                   (** invariant: l1 size > 1, l2 size > 0 *)
                   | App of 'a app_list * 'a app_list

  let app l1 l2 =
    match l1, l2 with
    | Nil, l | l, Nil -> l
    | Cons(x,Nil), l -> Cons(x,l)
    | _ -> App(l1,l2)

  let rec remove_one = function
    | Nil -> raise Not_found
    | Cons(x,l) -> (x,l)
    | App(l1,l2) -> let (x,l1) = remove_one l1 in (x, app l1 l2)

  type 'a heap = E
            | N of elt * 'a app_list * 'a heap * 'a heap * int

  let size = function
    | E -> 0
    | N(_,_,_,_,s) -> s

  (** insert in a heap at the correct position *)
  let add : elt -> 'a app_list -> 'a heap -> 'a heap = fun p l h ->
  let rec fn p l h =
    match h with
    | E                 -> N(p,l,E,E,1)
    | N(p',l',h1,h2,s) ->
       match T.compare p p' with
       | 0               -> N(p,app l l',h1,h2,s)
       | n when n < 0    ->
          begin
            if size h1 < size h2 then
              N(p,l,fn p' l' h1, h2, s+1)
            else
              N(p,l,h1,fn p' l' h2, s+1)
          end
       | _               ->
          begin
            if size h1 < size h2 then
              N(p',l',fn p l h1, h2, s+1)
            else
              N(p',l',h1,fn p l h2, s+1)
          end
  in
  fn p l h

  (** Extract  from the  heap. Does  not keep  balancing, but  the depth  may only
   decrease so it remains amortiz    ed logarithmic *)
  let remove : 'a heap -> (T.t * 'a app_list * 'a heap) option = fun h ->
    let rec fusion h01 h02 = match (h01, h02) with
      | (E, h) | (h, E) -> h
      | N(ok,l,h1,h2,s), N(ok',l',h1',h2',s') ->
         match T.compare ok ok' with
         | 0            -> N(ok,app l l',fusion h1 h1',fusion h2 h2',s+s'-1)
         | n when n < 0 -> N(ok,l,fusion h1 h2,h02,s+s')
         | _            -> N(ok',l',h01,fusion h1' h2',s+s')
    in
    match h with
    | E              -> None
    | N(p,l,h1,h2,_) -> Some (p, l, fusion h1 h2)


  (* keep best current prio appart *)
  type 'a t = (T.t * 'a app_list * 'a heap) option

  let empty = None

  let add p x = function
    | None -> Some (p, Cons(x,Nil), E)
    | Some (p',l,h) ->
       match T.compare p p' with
       | 0 -> Some(p,Cons(x,l),h)
       | n when n < 0 -> Some(p,Cons(x,Nil),add p' l h)
       | _ -> Some(p',l,add p (Cons(x,Nil)) h)

  let remove = function
    | None -> raise Not_found
    | Some(p,l,h) ->
       let (x,l) = remove_one l in
       if l = Nil then (x, remove h) else (x,Some(p,l,h))
end
