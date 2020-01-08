/* File parser.mly */
%token A
%token EOF
%start lr_top             /* the entry point */
%start rr_top             /* the entry point */
%type <unit> lr_top
%type <unit> rr_top
%%
lr:
                      { () }
| lr A                { () }
;
rr:
                      { () }
| A rr                { () }
;
lr_top: lr EOF             { $1 }
;
rr_top: rr EOF             { $1 }
;
