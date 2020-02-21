open Ast

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"

let%parser rec sexp
   = ERROR(["id";"("])
   ; (x::RE id)         => { p = _pos; e = Idt x }
   ; '(' (l::sexps) => (ERROR(")") ; ')'
                        => { p = _pos; e = Lst l })
and sexps = () => []
          ; (l::sexps) (e::sexp) => e::l

let%parser top = (s::sexp) => s
