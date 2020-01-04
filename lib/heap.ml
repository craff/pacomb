
let empty = []

type 'a t = 'a list list

let add cmp x h =
  let rec fn acc h = match h with
    | [] -> List.rev_append acc [[x]]
    | []::_ -> assert false
    | (y::_ as l)::h' ->
       match cmp x y with
       | 0 -> List.rev_append acc ((x::l) :: h')
       | n when n < 0 -> List.rev_append acc ([x]:: h)
       | _ -> fn (l::acc) h'
  in
  fn [] h

let remove l = match l with
  | [] -> raise Not_found
  | [x]::h -> x, h
  | (x::l)::h -> (x, (l::h))
  | []::_ -> assert false
