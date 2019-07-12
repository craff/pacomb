
open Grammar
open Combinator

let int = appl(term(
                   Lex.plus (Lex.charset (Charset.range '0' '9'))
                            (fun () -> Buffer.create 16)
                            (fun b c -> Buffer.add_char b c; b)),
               (fun b -> int_of_string (Buffer.contents b)))
let plus = term(Lex.char '+' ())
let moins = term(Lex.char '-' ())
let mul = term(Lex.char '*' ())
let div = term(Lex.char '/' ())
let opar = term(Lex.char '(' ())
let cpar = term(Lex.char ')' ())

let seq3 (a,b,c,f) = seq(seq(a,b,fun x _ -> x),c,f)

let expr = fixpoint ~name:"expr" (fun expr ->
  let atom =
    alt(int,
        seq(seq(opar,expr,fun _ x -> x),cpar, fun x _ -> x))
  in

  let pro = fixpoint ~name:"prod" (fun pro ->
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
