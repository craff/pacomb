open Ast

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"

let%parser rec sexp = (x::RE id)   => { l = x_lpos; r = x_rpos; e = Idt x }
              ; '(' (lazy l::sexps) ')' => { l = l_lpos; r = l_rpos; e = Lst l }
and sexps = () => lazy []
           ; (e::sexp) (lazy l::sexps) => lazy (e::l)

let%parser top = (s::sexp) => s
