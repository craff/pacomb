open Pacomb
open Grammar
open Pos

(*  This  example  (read  calc.ml  first)  illustrates  another  way  to  handle
   priorities with parametric grammars. *)

(* The three levels of priorities *)
type p = Atom | Prod | Sum
let%parser rec
              (* This includes each priority level in the next one *)
     expr p = Atom < Prod < Sum
            (* all other rule are selected by their priority level *)
            ; (p=Atom) (x::FLOAT)                        => x
            ; (p=Atom) '(' (e::expr Sum) ')'             => e
            ; (p=Prod) (x::expr Prod) '*' (y::expr Atom) => x*.y
            ; (p=Prod) (x::expr Prod) '/' (y::expr Atom) => x/.y
            ; (p=Sum ) (x::expr Sum ) '+' (y::expr Prod) => x+.y
            ; (p=Sum ) (x::expr Sum ) '-' (y::expr Prod) => x-.y

(* The parsing calling expression, with immediate evaluation (==>)
   printing the result and the next prompt. *)
let%parser top =
  (e::expr Sum) => Printf.printf "%f\n=> %!" e

let%parser rec exprs = () => () ; exprs top '\n' ==> ()

(* blanks *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!"; (* initial prompt *)
        parse_channel exprs blank stdin;
        raise End_of_file
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
