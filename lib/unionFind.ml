
type 'a t = 'a cell ref

and 'a cell = Link of 'a t | Root of 'a

let root x = ref (Root x)

let rec find x =
  match !x with
  | Root r -> (r, x)
  | Link y -> let (_, y' as c) = find y in
              if y != y' then x := Link y';
              c

let union fn x y =
  let (rx,x) = find x in
  let (ry,y) = find y in
  if x != y then
    begin
      let r = fn rx ry in
      x := Link y;
      y := Root r
    end

let set_root x r =
  match !x with
  | Root _ -> x := Root r
  | Link _ -> invalid_arg "set_root on non root"
