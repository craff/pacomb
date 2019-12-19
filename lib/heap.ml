
module Make(T:Set.OrderedType) = struct
  type elt = T.t
  type 'a t = E
               | N of elt * 'a list * 'a t * 'a t * int

  let size = function
    | E -> 0
    | N(_,_,_,_,s) -> s

  let empty = E

  (** insert in a heap at the correct position *)
  let add : elt -> 'a -> 'a t -> 'a t = fun p r h ->
  let rec fn p l h =
    match h with
    | E                 -> N(p,l,E,E,1)
    | N(p',l',h1,h2,s) ->
       match T.compare p p' with
       | 0               -> N(p,l@l',h1,h2,s)
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
  fn p [r] h

  (** Extract  from the  heap. Does  not keep  balancing, but  the depth  may only
   decrease so it remains amortiz    ed logarithmic *)
  let remove : 'a t -> 'a * 'a t = fun h ->
    let rec fusion h01 h02 = match (h01, h02) with
      | (E, h) | (h, E) -> h
      | N(ok,l,h1,h2,s), N(ok',l',h1',h2',s') ->
         match T.compare ok ok' with
         | 0            -> N(ok,l@l',fusion h1 h1',fusion h2 h2',s+s'-1)
         | n when n < 0 -> N(ok,l,fusion h1 h2,h02,s+s')
         | _            -> N(ok',l',h01,fusion h1' h2',s+s')
    in
    match h with
    | E                  -> raise Not_found
    | N(_,[],_,_,_)      -> assert false
    | N(_,[r],h1,h2,_)   -> (r, fusion h1 h2)
    | N(ok,r::l,h1,h2,s) -> (r, N(ok,l,h1,h2,s))



end
