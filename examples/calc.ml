open Pacomb

(* classical calculator example, with a non terminal for each priority level *)

(* Here is the definition of the  parser with the ppx syntax extension described
   in the documentation.

   Here, we deal with priorities  by manually defining three different grammars.
   Starting with the grammar for atomic expressions. *)
let%parser rec
        atom = (x::FLOAT)        => x                             (* constant *)
             ; '(' (e::expr) ')' => e                 (* rule for parenthesis *)

(* Here is the grammar for products *)
and prod = (a::atom)               => a
          ; (x::prod) '*' (y::atom) => x*.y
          ; (x::prod) '/' (y::atom) => x/.y

(* and finally all remaining expressions *)
and expr = (a::prod)               => a
         ; (x::expr) '+' (y::prod) => x+.y
         ; (x::expr) '-' (y::prod) => x-.y

(* A subtlety : we want to parse expression, one by one and print the
   result. Pacomb needs to do things that require buffer examination after
   each token. So printing after parsing the newline does not work.
   A trick that works is to test for the newline, not parsing it,
   using Grammar.test_after. Another solution would be to read each
   line with input_line and use Grammar.parse_string on the result.
*)
let nl _ b i _ _ =
  let (c,_,_) = Input.read b i in c = '\n'
let%parser rec top = (t::Grammar.test_after nl expr) => Printf.printf "%g\n=> %!" t
let%parser rec exprs = () => () ; exprs top '\n' => ()

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
           file desciptor are preferred. *)
        (* as we parse stdin, we need to keed the whole buffer in memory
           to have line and column number, ~rescan:false only give byte
           position *)
        Grammar.parse_fd ~rescan:false exprs blank Unix.stdin;
        raise End_of_file
      in
      (* [Pos] module provides a function to handle exception with
         an optional argument to call for error (default is to exit with
         code 1 *)
      Pos.handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
