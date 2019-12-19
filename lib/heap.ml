
module Make(T:Set.OrderedType) = struct
  type elt = T.t

  type 'a t = (T.t * 'a list) list

  let empty = []

  let add p x h =
    let rec fn acc h = match h with
    | [] -> List.rev_append acc [(p, [x])]
    | (p',l as c)::h' ->
       match T.compare p p' with
       | 0 -> List.rev_append acc ((p, x::l) :: h')
       | n when n < 0 -> List.rev_append acc ((p, [x]):: h)
       | _ -> fn (c::acc) h'
    in
    fn [] h

  let remove l = match l with
    | [] -> raise Not_found
    | (_,[x])::h -> x, h
    | (p,(x::l))::h -> (x, ((p,l)::h))
    | (_,[])::_ -> assert false
end
