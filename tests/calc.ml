open Pacomb
open Grammar
open Comb

let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

let float =
  term(Lex.appl float_of_string
         (Lex.regexp(Regexp.from_string
                       "\\([-+]?[0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?\\)")))
let%parser rec
        atom = (x::FLOAT)        => x
             ; (show_sub=false) '(' (e::expr) ')' => e
             ; (show_sub=true) (l::'(') (e::expr) (r::')') =>
                 let open Pos in
                 Printf.printf "%d-%d: %f\n" l_lpos.col r_rpos.col e;
                 e
and prod = (a::atom)               => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y
and expr = (a::prod)               => a
         ; (x::expr) '+' (y::prod) => x+.y
         ; (x::expr) '-' (y::prod) => x-.y

let g = compile expr

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
