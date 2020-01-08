open Ast

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"

let%parser rec sexp
   = ERROR(["id";"("])
   ; (x::RE id)         => { l = x_lpos; r = x_rpos; e = Idt x }
   ; '(' (l::sexps) => (ERROR(")") ; ')'
                        => { l = l_lpos; r = l_rpos; e = Lst l })
and sexps = () => []
          ; (l::sexps) (e::sexp) => e::l

let%parser top = (s::sexp) => s
