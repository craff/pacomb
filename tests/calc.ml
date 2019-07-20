
open Grammar
open Combinator

let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

let float =
  term(Lex.appl float_of_string
         (Lex.regexp(Regexp.from_string
                       "\\([-+]?[0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\)")))
[%%parser
 let rec
     atom_pos = (x::FLOAT) => x
              ; (l::'(') (e::exp) (r::')') =>
                  let open Lex in Printf.printf "%d-%d: %f\n" l_lpos.col r_rpos.col e; e
 and atom_nopos = (x::FLOAT) => x
                ; '(' (e::exp) ')' => e
 and atom = if show_sub then atom_pos else atom_nopos
 and prod = (a::atom) => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y
 and exp  = (a::prod) => a
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
