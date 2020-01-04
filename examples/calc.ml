open Pacomb
open Pos
open Grammar

(*  If -v  option  is given,  the  value of  expression  between parenthesis  is
   printed *)
let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

(* Here is the definition of the  parser with the ppx syntax extension described
   in the documentation.

   Here, we deal with priorities  by manually defining three different grammars.
   Starting with the grammar for atomic expressions. *)
let%parser rec
        atom = (x::FLOAT)        => x                             (* constant *)
             ; (show_sub=false) '(' (e::expr) ')' => e (*   rule for parenthesis
                                                       when show_sub is false *)
             ; (show_sub=true) '(' (e::expr) ')' =>(* idem with show_sub true *)
                 (Printf.printf "%a: %f\n" (Pos.print_interval ())
                    (Pos.interval_of_spos _pos) e;
                                          (* ^^^^^^ to access position of l   *)
                  e)

(* Here is the grammar for products *)
and prod = (a::atom)               => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y

(* and finally all remaining expressions *)
and expr = (a::prod)               => a
         ; (x::expr) '+' (y::prod) => x+.y
         ; (x::expr) '-' (y::prod) => x-.y

(* A subtlety : we want to parse expression, one by one and print the
   result. Pacomb needs to do things that require buffer examination avec
   each token to printing after parsing the newline does not work.
   A trick that works is to test for the newline, not parsing it.
*)

(* The parsing calling expression, printing the result and the next prompt. *)

let nl _ b i _ _ =
  let (c,_,_) = Input.read b i in c = '\n'
let%parser rec top = (t::Grammar.test_after nl expr) => Printf.printf "%f\n=> %!" t
let%parser rec exprs = () => () ; exprs top '\n' => ()

(* blanks *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!"; (* initial prompt *)
        parse_fd exprs blank Unix.stdin;
        raise End_of_file
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
