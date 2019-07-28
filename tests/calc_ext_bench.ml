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
        (p,f)
    with Not_found -> give_up ()

let%parser rec
  expr pmax = (r::dseq (expr pmax) ~cs:(Charset.from_string "-&~^+=*/\\$!:")
                    (fun pe ->
                      dseq (op pe pmax)
                        ~cs:(Charset.from_string "-+0-9(")
                        (fun pop -> seq (expr pop)
                           (empty (fun (_,e2) b e1 -> (pop, b e1 e2))))))
                                                  => r
            ; (x::FLOAT)                          => (0.0,x)
            ; '(' (e::expr_top) ')'               => (0.0,e)

and expr_top = ((__,e)::expr 1000.0) => e

let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!";
        let line = input_line stdin in
        Printf.printf "%f\n%!" (parse_string expr_top blank line )
      in handle_exception f ()
    done
  with
    End_of_file -> ()
