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
             ; (show_sub=true) (l::'(') (e::expr) (r::')') =>
                 let open Pos in                   (* idem with show_sub true *)
                 Printf.printf "%d-%d: %f\n" l_lpos.col r_rpos.col e;
                                          (* ^^^^^^ to access position of l   *)
                 e

(* Here is the grammar for products *)
and prod = (a::atom)               => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y

(* and finally all remaining expressions *)
and expr = (a::prod)               => a
         ; (x::expr) '+' (y::prod) => x+.y
         ; (x::expr) '-' (y::prod) => x-.y

(* A subtlety : we want to parse expression, on by one and print the
   result. Pacomb evaluates action after the next token to avoid some useless
   evaluation but still make give_up usable. So we evaluate after newline...
   But if we where parsing blank after newline, we still would have to wait
   the input after the newline.

   To solve this, we parse line with no blank, use [Grammar.layout] to accept
   blank inside expression.

   A simpler solution (used in calc_ext.ml) is to read the input line
   by line and parse each line using [Grammar.parse_string].
*)

let config =
  Blank.{ default_layout_config with
          new_blanks_before = true
        ; new_blanks_after = true}

(* The parsing calling expression and changing the blank,
   printing the result and the next prompt. *)
let%parser top =
  (e::expr) => Printf.printf "%f\n=> %!" e

let%parser rec exprs = () => () ; exprs top '\n' ==> ()

(* we define the characters to be ignored, here space only *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!"; (* initial prompt *)
        parse_channel ~utf8:Utf8.UTF8 exprs blank stdin;
        raise End_of_file
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
