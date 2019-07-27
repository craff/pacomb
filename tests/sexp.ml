open Pacomb
open Comb
open Grammar

type sexp = { l: Pos.t; r: Pos.t; e : sexp' }
and sexp' =
  | Idt of string
  | Lst of sexp list

let rec size e = match e.e with
  | Idt _ -> 1
  | Lst l -> List.fold_left (fun a e -> a + size e) 1 l


[%%parser
 let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"
 let rec sexp
   = (x::RE id)         => { l = x_lpos; r = x_rpos; e = Idt x }
   ; '(' (l::sexps) ')' => { l = l_lpos; r = l_rpos; e = Lst (List.rev l) }
 and sexps = () => []
           ; (l::sexps) (e::sexp) => e::l

 let blank = Lex.blank_charset (Charset.from_string " \t\n\r")

 let g = compile sexp

 let _ =
   let e = parse_channel g blank stdin in
   Printf.printf "=> %d\n%!" (size e)
]
