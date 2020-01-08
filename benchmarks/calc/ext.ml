open Pacomb

let eps = 1e-10

type assoc = RightAssoc | LeftAssoc | NonAssoc

let cs = Charset.(complement (from_string "0-9()"))

let bins = Word_list.create ~cs ()

let _ =
  Word_list.add_ascii bins "*" (( *. ), 4.0, LeftAssoc);
  Word_list.add_ascii bins "/" (( /. ),4.0, LeftAssoc);
  Word_list.add_ascii bins "+" (( +. ),6.0, LeftAssoc);
  Word_list.add_ascii bins "-" (( -. ),6.0, LeftAssoc)

let%parser op pmin pmax =
  ((f,p,a)::Word_list.word bins) => (
      let good = match a with
        | NonAssoc -> pmin < p && p < pmax
        | LeftAssoc -> pmin <= p && p < pmax
        | RightAssoc -> pmin < p && p <= pmax
      in
      if not good then Lex.give_up ();
      let p = match a with
        | RightAssoc -> p
        | _          -> p -. 1e-10
      in
      (p,f))

let%parser rec
 expr pmax = ((pe,e1)>:expr pmax) ((pop,b)>:op pe pmax) ((__,e2)::expr pop)
                                                  => (pop, b e1 e2)
            ; (x::FLOAT)                          => (0.0,x)
            ; '(' (e::expr_top) ')'               => (0.0,e)

and expr_top = ((__,e)::expr 1000.0) => e

let%parser rec top = () => () ; top (__::expr_top) '\n' => ()
