/* File parser.mly */
%token <float> FLOAT
%token PLUS MINUS TIMES DIV POW
%token LPAREN RPAREN
%token EOL EOF
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%right POW
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <unit> main
%%
main:
                          { () }
| main top                { () }
;
top: expr EOL             { Printf.printf "%f\n>> " $1 }
;
expr:
  FLOAT                   { $1 }
| LPAREN expr RPAREN      { $2 }
| expr PLUS expr          { $1 +. $3 }
| expr MINUS expr         { $1 -. $3 }
| expr TIMES expr         { $1 *. $3 }
| expr DIV expr           { $1 /. $3 }
| expr POW expr           { $1 ** $3 }
| MINUS expr %prec UMINUS { -. $2 }
| PLUS expr %prec UMINUS  { $2 }
;
