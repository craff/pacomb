open Ast

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"

let%parser rec sexp = (x::RE id)   => { p = _pos; e = Idt x }
              ; '(' (lazy l::sexps) ')' => { p = _pos; e = Lst l }
and sexps = () => lazy []
           ; (e::sexp) (lazy l::sexps) => lazy (e::l)

let%parser top = (s::sexp) => s
