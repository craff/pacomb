open Parsing

let _ =
  if (Unix.fstat (Unix.descr_of_in_channel stdin)).Unix.st_kind = Unix.S_REG
  then
      try
        let lexbuf = Lexing.from_channel stdin in
        Parser.main Lexer.token lexbuf
      with
        Parse_error -> Printf.fprintf stderr "Parse error\n%!"
  else
    try
      while true do
        try
          Printf.printf ">> %!";
          let lexbuf = Lexing.from_channel stdin in
          Parser.main Lexer.token lexbuf;
          raise End_of_file
        with
          Parse_error -> Printf.fprintf stderr "Parse error\n%!"

      done
    with End_of_file -> ()
