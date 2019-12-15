open Pacomb
open Lex
open Pos
open Grammar

(* factorisation ... just  a test for the ppx,
   here left factorisation is done by the elimination of left recursion,
   to the result is the same as with calc_prio.ml *)

let eps = 1e-10

type assoc = RightAssoc | LeftAssoc | NonAssoc

let prios =  [("^", (( ** ), 2.0, RightAssoc))
             ;("*", (( *. ), 4.0, LeftAssoc))
             ;("/", (( /. ),4.0, LeftAssoc))
             ;("+", (( +. ),6.0, LeftAssoc))
             ;("-", (( -. ),6.0, LeftAssoc))
             ]

let%parser bin = (c::RE("[-&~^+=*/\\$!:]+\\(_[a-zA-Z0-9_]+\\)?")) => c

let%parser op pmin pmax =
  (c::bin) =>
    try let (f,p,a) = List.assoc c prios in
        let good = match a with
          | NonAssoc -> pmin < p && p < pmax
          | LeftAssoc -> pmin <= p && p < pmax
          | RightAssoc -> pmin < p && p <= pmax
        in
        if not good then give_up ();
        let p = match a with
          | RightAssoc -> p
          | _          -> p -. 1e-10
        in
        (p,f)
    with Not_found -> give_up ~msg:("unbound op bin "^c) ()

let%parser rec
 expr pmax = ((pe,e1)>:expr pmax) ((pop,b)>:op pe pmax) ((__,e2)::expr pop)
                                                  => (pop, b e1 e2)
            ; (x::FLOAT)                          => (0.0,x)
            ; '(' (e::expr_top) ')'               => (0.0,e)

and expr_top = ((__,e)::expr 1000.0) => e

let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!";
        let line = input_line stdin in
        Printf.printf "%f\n%!" (parse_string expr_top blank line )
      in handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
