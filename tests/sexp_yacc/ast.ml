type sexp = { l: Lexing.position; r: Lexing.position; e : sexp' }
 and sexp' =
   | Idt of string
   | Lst of sexp list

let rec size e = match e.e with
  | Idt _ -> 1
  | Lst l -> List.fold_left (fun a e -> a + size e) 1 l
