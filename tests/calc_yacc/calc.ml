open Parsing

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
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
