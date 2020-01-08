
(* classical calculator example, with a non terminal for each priority level *)
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

let%parser rec top = () => () ; top (__::expr) '\n' => ()
