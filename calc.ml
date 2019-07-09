
open Grammar
open Grammar__Combinator
module Lex = Grammar__Lex
module Charset = Grammar__Charset

let int = appl(Term(Lex.charset (Charset.range '0' '9')),int_of_string)
let plus = Term(Lex.char '+' ())
let moins = Term(Lex.char '-' ())
let mul = Term(Lex.char '*' ())
let div = Term(Lex.char '/' ())
let opar = Term(Lex.char '(' ())
let cpar = Term(Lex.char ')' ())

let seq3 (a,b,c,f) = seq(seq(a,b,fun x _ -> x),c,f)

let expr = fixpoint(fun expr ->
  let atom =
    alt(int,
        seq(seq(opar,expr,fun _ x -> x),cpar, fun x _ -> x))
  in

  let pro = fixpoint (fun pro ->
                alt(atom,
                    alt(seq3(pro,mul,atom,( * )),
                        seq3(pro,div,atom,( / )))))
  in

  alt(pro,
      alt(seq3(expr,plus,pro,(+)),
          seq3(expr,moins,pro,(-)))))

let g = compile expr

let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let line = input_line stdin in
      let n = parse_string g blank line in
      Printf.printf "%d\n%!" n
    done
  with
    End_of_file -> ()
