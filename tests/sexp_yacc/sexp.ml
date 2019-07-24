open Ast
open Parsing


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let x = Parser.main Lexer.token lexbuf in
    Printf.printf "=> %d\n" (size x)
  with
    Parse_error -> Printf.fprintf stderr "Parse error\n%!"
