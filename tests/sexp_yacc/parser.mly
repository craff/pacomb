/* File parser.mly */
%{
open Ast
let mkloc e = { l=Parsing.symbol_start_pos ()
              ; r=Parsing.symbol_end_pos  ()
              ; e}
%}
%token <string> ID
%token LPAREN RPAREN
%token EOL
%start main             /* the entry point */
%type <Ast.sexp> main
%%

  main:
    sexp EOL                { $1 }
    ;
  sexp:
      ID                      { mkloc (Ast.Idt $1) }
    | LPAREN sexps RPAREN     { mkloc (Ast.Lst (List.rev $2)) }
  sexps:
                              { [] }
    | sexps sexp              { $2 :: $1 }
