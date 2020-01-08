open Pacomb

let test = Array.length Sys.argv > 1 && Sys.argv.(1) = "--test"

let random_id () =
  let c = Char.chr (Char.code 'a' + Random.int 26) in
  let n = string_of_int (Random.int 1000) in
  String.make 1 c ^ n

let print_spaces ch lvl =
  output_string ch "\n";
  for _ = 1 to lvl do
    output_string ch "\t"
  done;
  1+lvl

let sqrt n = Random.int (truncate ((float n) ** 0.7) + 1)

let rec expr lvl n ch =
  let print_string s =
    output_string ch s; String.length s
  in
  if n <= 0 then 0
  else if n = 1 then print_string (random_id ())
  else
    begin
      let x = print_spaces ch lvl in
      let lvl = lvl + 1 in
      let y = print_string "(" in
      let s = sqrt n in
      let n = n - 1 in
      let rec fn s n =
        if s <= 1 then expr lvl n ch
        else
          begin
            let q = n/2 in
            let n0 = n/s - q/2 + Random.int(min 1 q) in
            let n0 = max 1 (min n n0) in
            let x = expr lvl n0 ch in
            let y = print_string " " in
            let z = fn (s-1) (n - n0) in
            x + y + z
          end
      in
      let z = fn s n in
      let t = print_string ")" in
      x + y + z + t
    end

(* blanks *)
let blank = Blank.from_charset (Charset.from_string " \t\n")

let _ =
  let bench_lr = Bench.create () in
  let bench_rr     = Bench.create () in
  let bench_err    = Bench.create () in
  let bench_yacc   = Bench.create () in
  for n = 1 to (if test then 2 else 20) do
    let n = n * 50_000 in
    let producer ch = let r = expr 0 n ch in Printf.fprintf ch "\n%!"; r in
    let size = Bench.size producer in
    let (_,ts,w) = Bench.parse_pipe bench_lr Lr.top blank size producer in
    Printf.printf "lr   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. ts) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,tr,w) = Bench.parse_pipe bench_rr Rr.top blank size producer in
    Printf.printf "rr   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. tr) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,te,w) = Bench.parse_pipe bench_err Err.top blank size producer in
    Printf.printf "err  %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. te) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,ty,w) = Bench.yacc_pipe bench_yacc Parser.main Lexer.token size producer in
    Printf.printf "yacc %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. ty) (float w /. 1024. /. 1024. *. float Sys.word_size);
    Printf.printf "lr/yacc : %f " (ts /. ty);
    Printf.printf "rr/yacc : %f "     (tr /. ty);
    Printf.printf "err/yacc : %f "     (te /. ty);
    Printf.printf "rr/lr: %f " (tr /. ts);
    Printf.printf "err/lr: %f\n%!" (te /. ts);
  done;
  Bench.stats "lr   " bench_lr;
  Bench.stats "rr   " bench_rr;
  Bench.stats "err  " bench_err;
  Bench.stats "yacc " bench_yacc;
  if not test then
    begin
      Bench.csv bench_lr   "lr.csv";
      Bench.csv bench_rr   "rr.csv";
      Bench.csv bench_rr   "err.csv";
      Bench.csv bench_yacc "yacc.csv"
    end
