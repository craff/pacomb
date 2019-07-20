
open Grammar
open Combinator

type p = Atom | Prod | Sum

[%%parser
 let rec
     expr p = (p=Atom) (x::FLOAT)                        => x
            ; (p=Atom) '(' (e::expr Sum) ')'             => e
            ; (p=Prod) (a::expr Atom)                    => a
            ; (p=Prod) (x::expr Prod) '*' (y::expr Atom) => x*.y
            ; (p=Prod) (x::expr Prod) '/' (y::expr Atom) => x/.y
            ; (p=Sum ) (a::expr Prod)                    => a
            ; (p=Sum ) (x::expr Sum ) '+' (y::expr Prod) => x+.y
            ; (p=Sum ) (x::expr Sum ) '-' (y::expr Prod) => x-.y
]

let g = compile (expr Sum)

let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      try
        let line = input_line stdin in
        let n = parse_string g blank line in
        Printf.printf "%f\n%!" n
      with Parse_error (_,c) ->
        Printf.eprintf "parse error at %d\n%!" c
    done
  with
    End_of_file -> ()
