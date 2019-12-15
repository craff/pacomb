open Pacomb
open Pos
open Grammar

(* factorisation ... just  a test for the ppx,
   here left factorisation is done by the elimination of left recursion,
   to the result is the same as with calc_prio.ml *)
type p = Atom | Prod | Sum
let%parser rec
     expr p = Atom < Prod < Sum
            ; (p=Atom) (x::FLOAT)                        => x
            ; (p=Atom) '(' (e::expr Sum) ')'             => e
            ; (p=Prod) (x::expr Prod) => ( '*' (y::expr Atom) => x*.y
                                         ; '/' (y::expr Atom) => x/.y)
            ; (p=Sum ) (x::expr Sum ) => ('+' (y::expr Prod) => x+.y
                                         ; '-' (y::expr Prod) => x-.y)

let top = expr Sum

let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!";
        let line = input_line stdin in
        let n = parse_string top blank line in
        Printf.printf "%f\n%!" n
      in
      handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
