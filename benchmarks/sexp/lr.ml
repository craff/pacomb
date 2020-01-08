open Ast

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"
let%parser rec sexp
   = (x::RE id)         => { l = _lpos; r = _rpos; e = Idt x }
   ; '(' (l::sexps) ')' => { l = _lpos; r = _rpos; e = Lst (List.rev l) }
and sexps = () => []
          ; (l::sexps) (e::sexp) => e::l

let%parser top = (s::sexp) => s
