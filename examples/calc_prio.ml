open Pacomb
open Grammar
open Pos

(*  This  example  (read  calc.ml  first)  illustrates  another  way  to  handle
   priorities with parametric grammars. *)

(* The three levels of priorities *)
type p = Atom | Prod | Sum

let string_of_prio = function
  | Atom -> "A"
  | Prod -> "P"
  | Sum  -> "S"

let%parser [@print_param string_of_prio] rec
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
let%parser rec exprs =
    () => ()
  ; exprs (e::expr Sum) '\n' => Printf.printf "%f\n=> %!" e

(* blanks *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  Printf.printf "parsing with:\n%a\n%!" (fun ch -> print_grammar ch) exprs;
  Printf.printf "compiled to:\n%a\n%!" (fun ch -> print_grammar ~def:false ch) exprs;
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
