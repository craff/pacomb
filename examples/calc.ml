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
 *)
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

(* we define the characters to be ignored, here space only *)
let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        Printf.printf "=> %!";
        (* we read one line and call the parser *)
        let line = input_line stdin in
        let n = parse_string expr blank line in
        Printf.printf "%f\n%!" n
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
