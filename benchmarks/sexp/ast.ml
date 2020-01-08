type 'a sexp = { l: 'a; r: 'a; e : 'a sexp' }
and 'a sexp' =
  | Idt of string
  | Lst of 'a sexp list
