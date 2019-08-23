
let rec seq op n atom =
  if n <= 0 then atom ()
  else (atom (); print_string op; seq op (n-1) atom)

let paren atom =
  print_string "("; atom (); print_string ")"

let int () =
  print_string (string_of_int (Random.int 0x20000000))

let rec expr n p s =
  if n <= 0 then int ()
  else seq (if Random.bool () then " + " else " - ")
         s (fun () -> seq
                        (if Random.bool () then "*" else "/")
                        p (fun () ->
                          if n = 1 then int () else
                            paren (fun () -> expr (n - 1) p s)))

let n = int_of_string (Sys.argv.(1))
let p = int_of_string (Sys.argv.(2))
let s = int_of_string (Sys.argv.(3))

let _ = expr n p s; print_newline ()
