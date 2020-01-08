
(* classical calculator example, with parameters priority level *)
(* The three levels of priorities *)
type p = Atom | Prod | Sum

(* for printing, we provide a function to convert priorities to string *)
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

let%parser rec top = () => () ; top (__::expr Sum) '\n' => ()
