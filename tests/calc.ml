
open Grammar
open Combinator

let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

[%%parser
 let atom = (x::FLOAT) => x
          ; '(' (e::exp) ')' => e
 let prod = (a::atom) => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y
 let exp = (a::prod) => a
          ; (x::exp) '+' (y::prod) => x+.y
          ; (x::exp) '-' (y::prod) => x-.y
]

let g = compile exp

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
