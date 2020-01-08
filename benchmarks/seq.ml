open Pacomb


let expr n ch =
  for _ = 1 to n do
    output_char ch 'a'
  done;
  n

module Lr = struct
  let%parser rec lr = ()     => ()
                    ; lr 'a' => ()

  let%parser top = lr EOF => ()
end

module Ld = struct
  let%parser rec lr = ()                => ((),())
                    ; ((__,__)>:lr) 'a' => ((),())

  let%parser top = lr EOF => ()
end

module Rr = struct
  let%parser rec rr = ()                => lazy ()
                    ; 'a' (lazy __::rr) => lazy ()

  let%parser top = (x::rr) EOF => Lazy.force x
end

module Rd = struct
  let%parser a = 'a' => ((), ())
  let%parser rec rr = ()                         => lazy ()
                    ; ((__,__)>:a) (lazy __::rr) => lazy ()

  let%parser top = (x::rr) EOF => Lazy.force x
end

module Lp = struct
  let%parser rec lr = ()     => ()
                    ; lr 'a' => let _ = _pos in ()

  let%parser top = lr EOF => ()
end

module Rp = struct
  let%parser rec rr = ()                => lazy ()
                    ; 'a' (lazy __::rr) => lazy (let _ = _pos in ())

  let%parser top = (x::rr) EOF => Lazy.force x
end


let blank = Blank.none

let _ =
  let bench_lr = Bench.create () in
  let bench_rr = Bench.create () in
  let bench_ld = Bench.create () in
  let bench_rd = Bench.create () in
  let bench_lp = Bench.create () in
  let bench_rp = Bench.create () in
  let bench_ly = Bench.create () in
  let bench_ry = Bench.create () in
  let bench_dum = Bench.create () in
  let producer0 ch = let r = expr 0 ch in Printf.fprintf ch "%!"; r in
  for n = 1 to 20 do
    let n = n * 200_000 in
    let producer ch = let r = expr n ch in Printf.fprintf ch "%!"; r in
    let size = Bench.size producer in
    let (_,tl,w) = Bench.parse_pipe bench_lr Lr.top blank size producer in
    Printf.printf "lr   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. tl) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,tr,w) = Bench.parse_pipe bench_rr Rr.top blank size producer in
        Printf.printf "rr   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
          (float size /. 1024. /. 1024.)
          (1000. *. tr) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,tld,w) = Bench.parse_pipe bench_ld Ld.top blank size producer in
        Printf.printf "ld   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
          (float size /. 1024. /. 1024.)
          (1000. *. tld) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,trd,w) = Bench.parse_pipe bench_rd Rd.top blank size producer in
        Printf.printf "rd   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
          (float size /. 1024. /. 1024.)
          (1000. *. trd) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,tlp,w) = Bench.parse_pipe bench_lp Lp.top blank size producer in
        Printf.printf "lp   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
          (float size /. 1024. /. 1024.)
          (1000. *. tlp) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,trp,w) = Bench.parse_pipe bench_rp Rp.top blank size producer in
        Printf.printf "rp   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
          (float size /. 1024. /. 1024.)
          (1000. *. trp) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let (_,tly,w) = Bench.yacc_pipe bench_ly Parser.lr_top Lexer.token size producer in
    Printf.printf "ly   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. tly) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let _ = Bench.yacc_pipe bench_dum Parser.lr_top Lexer.token 0 producer0 in
    let (_,try_,w) = Bench.yacc_pipe bench_ry Parser.rr_top Lexer.token size producer in
    Printf.printf "ry   %d %.2f Mb in %.2fms %.2f Mb \n%!" n
      (float size /. 1024. /. 1024.)
      (1000. *. try_) (float w /. 1024. /. 1024. *. float Sys.word_size);
    let _ = Bench.yacc_pipe bench_dum Parser.rr_top Lexer.token 0 producer0 in
    Printf.printf "lr/ly: %f " (tl  /. tly);
    Printf.printf "ry/ly: %f " (try_  /. tly);
    Printf.printf "rr/ly: %f " (tr  /. tly);
    Printf.printf "ld/ly: %f " (tld /. tly);
    Printf.printf "rd/ly: %f " (trd /. tly);
    Printf.printf "lp/ly: %f " (tlp /. tly);
    Printf.printf "rp/ly: %f\n%!" (trp /. tly);
    Printf.printf "rr/lr: %f " (tr  /. tl);
    Printf.printf "ld/lr: %f " (tld /. tl);
    Printf.printf "rd/lr: %f " (trd /. tl);
    Printf.printf "lp/lr: %f " (tlp /. tl);
    Printf.printf "rp/lr: %f " (trp /. tl);
    Printf.printf "rd/rr: %f " (trd /. tr);
    Printf.printf "rp/rr: %f\n%!" (trp /. tr);
  done;
  Bench.stats "lr   " bench_lr;
  Bench.stats "ld   " bench_ld;
  Bench.stats "rr   " bench_rr;
  Bench.stats "rd   " bench_rd;
  Bench.stats "lp   " bench_lp;
  Bench.stats "rp   " bench_rp;
  Bench.csv bench_lr   "lr.csv";
  Bench.csv bench_rr   "rr.csv";
  Bench.csv bench_ld   "ld.csv";
  Bench.csv bench_rd   "rd.csv";
  Bench.csv bench_lp   "lp.csv";
  Bench.csv bench_rp   "rp.csv"
