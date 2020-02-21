open Pacomb

(*  This  example  (read  calc.ml  first)  illustrates  another  way  to  handle
   priorities with parametric grammars. *)

(* The three levels of priorities *)
type p = Atom | Prod | Sum

let string_of_prio = function
  | Atom -> "A"
  | Prod -> "P"
  | Sum  -> "S"

(* for printing, we provide a function to convert priorities to string *)
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

(* A subtlety : we want to parse expression, one by one and print the
   result. Pacomb needs to do things that require buffer examination after
   each token. So printing after parsing the newline does not work.
   A trick that works is to test for the newline, not parsing it,
   using Grammar.test_after. Another solution would be to read each
   line with input_line and use Grammar.parse_string on the result.
*)
let nl _ b i _ _ =
  let (c,_,_) = Input.read b i in c = '\n'
let%parser rec top =
  (t::Grammar.test_after nl (expr Sum)) => Printf.printf "%g\n=> %!" t
let%parser rec exprs =
  () => () ; exprs top '\n' => ()

(* parsing command line arguments, illustrating grammar printing *)
let usage_msg = Printf.sprintf "%s [options]" Sys.argv.(0)

let rec help () =
  Arg.usage spec usage_msg;
  Printf.eprintf "\nParsing with:\n\n%a\n%!"
    (fun ch -> Grammar.print_grammar ch) exprs

and spec = [( "-help", Arg.Unit help, "print help message")
           ;("--help", Arg.Unit help, "print help message")]

let _ = Arg.parse spec (fun s -> raise (Arg.Bad s)) usage_msg

(* blanks *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!"; (* initial prompt *)
        (* no need to stack the buffer of in_channel and those of Pacomb. So
           file desciptor are preferred *)
        Grammar.parse_fd exprs blank Unix.stdin;
        raise End_of_file
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      Pos.handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
