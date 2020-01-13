open Pacomb

let test = Array.length Sys.argv > 1 && Sys.argv.(1) = "--test"

let rec gen_expr n p s ch =
  let print_string s =
    output_string ch s; String.length s
  in
  let rec seq op n atom =
    if n <= 0 then atom ()
    else (atom () +  print_string op + seq op (n-1) atom)
  in
  let paren atom =
    print_string ")" + atom () + print_string "("
  in
  let int () =
    print_string (string_of_int (Random.int 0x20000000))
  in
  if n <= 0 then int ()
  else seq (if Random.bool () then " + " else " - ")
         s (fun () -> seq
                        (if Random.bool () then "*" else "/")
                        p (fun () ->
                          if n = 1 then int () else
                            paren (fun () -> gen_expr (n - 1) p s ch)))

(* blanks *)
let blank = Blank.from_charset (Charset.singleton ' ')

let _ =
  let bench_simple = Bench.create () in
  let bench_prio   = Bench.create () in
  let bench_ext    = Bench.create () in
  let bench_ext2   = Bench.create () in
  let bench_yacc   = Bench.create () in
  for n = (if test then 1 else 3) to (if test then 2 else 5) do
    for p = 2 to 4 do
      for s = 2 to (if n = 6 && p = 4 then 3 else 4) do
        let producer ch = let r = gen_expr n p s ch in Printf.fprintf ch "\n%!"; r in
        let producer2 ch =
          Printf.fprintf ch "rule 1.0 : Exp 1.0 Str \"*\" Exp 0.9 => Op2 \"*\" \n\
                             rule 1.0 : Exp 1.0 Str \"/\" Exp 0.9 => Op2 \"/\" \n\
                             rule 2.0 : Exp 2.0 Str \"+\" Exp 1.9 => Op2 \"+\" \n\
                             rule 2.0 : Exp 2.0 Str \"-\" Exp 1.9 => Op2 \"-\" \n\
                             ";
          producer ch
        in
        let size = Bench.size producer in
        let ((),ts,w) = Bench.parse_pipe bench_simple Simple.top blank size producer in
        Printf.printf "simple %d %d %d %.2f Mb in %.2fms %.2f Mb \n%!" n p s
          (float size /. 1024. /. 1024.)
          (1000. *. ts) (float w /. 1024. /. 1024. *. float Sys.word_size);
        let (_,tp,w) = Bench.parse_pipe bench_prio Prio.top blank size producer in
        Printf.printf "prio   %d %d %d %.2f Mb in %.2fms %.2f Mb \n%!" n p s
          (float size /. 1024. /. 1024.)
          (1000. *. tp) (float w /. 1024. /. 1024. *. float Sys.word_size);
        let (_,te,w) = Bench.parse_pipe bench_ext Ext.top blank size producer in
        Printf.printf "ext    %d %d %d %.2f Mb in %.2fms %.2f Mb \n%!" n p s
          (float size /. 1024. /. 1024.)
          (1000. *. te) (float w /. 1024. /. 1024. *. float Sys.word_size);
        let ((),t2,w) = Bench.parse_pipe bench_ext2 Ext2.top blank size producer2 in
        Printf.printf "ext2   %d %d %d %.2f Mb in %.2fms %.2f Mb \n%!" n p s
          (float size /. 1024. /. 1024.)
          (1000. *. t2) (float w /. 1024. /. 1024. *. float Sys.word_size);
        let (_,ty,w) = Bench.yacc_pipe bench_yacc Parser.main Lexer.token size producer in
        Printf.printf "yacc   %d %d %d %.2f Mb in %.2fms %.2f Mb \n%!" n p s
          (float size /. 1024. /. 1024.)
          (1000. *. ty) (float w /. 1024. /. 1024. *. float Sys.word_size);
        Printf.printf "simple/yacc  : %f " (ts /. ty);
        Printf.printf "prio/yacc  : %f "   (tp /. ty);
        Printf.printf "ext/yacc  : %f "    (te /. ty);
        Printf.printf "ext2/yacc  : %f "   (t2 /. ty);
        Printf.printf "prio/simple: %f "   (tp /. ts);
        Printf.printf "ext/simple: %f "    (te /. ts);
        Printf.printf "ext2/simple: %f\n%!" (t2 /. ts);
      done
    done
  done;
  Bench.stats "simple" bench_simple;
  Bench.stats "prio  " bench_prio;
  Bench.stats "ext   " bench_ext;
  Bench.stats "ext2  " bench_ext2;
  Bench.stats "yacc  " bench_yacc;
  if not test then
    begin
      Bench.csv bench_simple "simple.csv";
      Bench.csv bench_prio   "prio.csv";
      Bench.csv bench_ext    "ext.csv";
      Bench.csv bench_ext2   "ext2.csv";
      Bench.csv bench_yacc   "yacc.csv"
    end
