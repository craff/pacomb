open Pacomb
open Grammar


let gamma_gen n =
  let b = Buffer.create (2*n) in
  let rec gen n =
    if n <= 0 then () else
    if Random.bool () then
      begin
        Buffer.add_string b "a";
        gen (n-1);
        Buffer.add_string b "b"
      end
    else
      begin
        Buffer.add_string b "a";
        gen (n-1);
        Buffer.add_string b "c"
      end
  in
  gen n;
  Buffer.contents b

let parse_string c = parse_string c (Blank.from_charset (Charset.singleton ' '))

let char_a = term(Lex.char 'a' 1)
let char_b = term(Lex.char 'b' 1)
let char_c = term(Lex.char 'c' 1)

let%parser [@cache] rec g =   () => ()
                       ; 'a' g 'b' => ()
                       ; 'a' g 'c' => ()

let n = int_of_string Sys.argv.(1)

let chrono_parse g s =
  let n = String.length s in
  Printf.printf "parsing %d chars in %!" n;
  let t0 = Unix.gettimeofday () in
  let r = parse_string g s in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%f seconds\n%!" (t1 -. t0);
  r

let _ =
  for i = 1 to 4 do
    let str = gamma_gen (n*i) in
    chrono_parse g str
  done
