        {
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        }
        rule token = parse
            [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip blanks *)
          | eof        { EOL }
          | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as lxm { ID lxm }
          | '(' { LPAREN }
          | ')' { RPAREN }
