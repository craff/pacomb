open Pacomb
open Grammar


let gamma_gen n =
  let b = Buffer.create (2*n) in
  let rec gen n =
    if n <= 0 then () else
      match Random.int 3 with
      | 0 ->
           Buffer.add_string b "a";
           gen (n-1);
           Buffer.add_string b "b"
      | 1 ->
         Buffer.add_string b "a";
         Buffer.add_string b "b";
         gen (n-1);
         Buffer.add_string b "c"
      | 2 ->
         Buffer.add_string b "a";
         Buffer.add_string b "b";
         Buffer.add_string b "c";
         gen (n-1);
         Buffer.add_string b "d"
      | _ -> assert false
  in
  gen n;
  Buffer.contents b

let parse_string c = parse_string c (Blank.from_charset (Charset.singleton ' '))

let%parser [@cache] rec g =     () => ()
                       ; 'a' g 'b' => ()
                       ; 'a' g 'b' g 'c' => ()
                       ; 'a' g 'b' g 'c' g 'd' => ()

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
