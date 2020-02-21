type 'a sexp = { p: 'a; e : 'a sexp' }
and 'a sexp' =
  | Idt of string
  | Lst of 'a sexp list
