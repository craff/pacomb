open Pacomb

type sexp = { p: Pos.t * Pos.t; e : sexp' }
and sexp' =
  | Idt of string
  | Lst of sexp list

let id = "[a-zA-Z_][a-zA-Z_0-9]*[']*"

let%parser rec sexp
   = ERROR(["id";"("])
   ; (x::RE id)         => { p = _pos; e = Idt x }
   ; '(' (l::sexps)     => (ERROR(")") ; ')'
                        => { p = _pos; e = Lst (List.rev l) })
and sexps = () => []
          ; (l::sexps) (e::sexp) => e::l

(* blank c++ like end of line comments *)
let blank = Blank.line_comments "\\\\"

let _ =
  let f () = Grammar.parse_channel sexps blank stdin in
  Pos.handle_exception f ()
