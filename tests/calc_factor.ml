open Pacomb
open Grammar
open Comb

(* factorisation ... just  a test for the ppx,
   here left factorisation is done by the elimination of left recursion,
   to the result is the same as with calc_prio.ml *)
[%%parser
 type p = Atom | Prod | Sum
 let rec
     expr p = Atom < Prod < Sum
            ; (p=Atom) (x::FLOAT)                        => x
            ; (p=Atom) '(' (e::expr Sum) ')'             => e
            ; (p=Prod) (x::expr Prod) => ( '*' (y::expr Atom) => x*.y
                                         ; '/' (y::expr Atom) => x/.y)
            ; (p=Sum ) (x::expr Sum ) => ('+' (y::expr Prod) => x+.y
                                         ; '-' (y::expr Prod) => x-.y)
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
