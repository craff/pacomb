
(* This test is for measuring the performance of Input *)
open Pacomb

let utf8 =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "--utf8" then
    Utf8.UTF8 else Utf8.ASCII

let buf = Input.from_file ~utf8 Sys.argv.(Array.length Sys.argv - 1)

let _ =
  let t0 = Unix.gettimeofday () in
  let rec fn n buf pos =
    let (c,buf,pos) = Input.read buf pos in
    if c = '\255' then Input.line_num buf, n else fn (n+1) buf pos
  in
  let l, n = fn 0 buf Input.init_pos in
  let t1 = Unix.gettimeofday () in
  let d = t1 -. t0 in
  let s = (float) n /. d /. (1024. *. 1024.) in
  let m = (float n) /.  (1024. *. 1024.) in
  Printf.printf "%.2f Mo/%d lines read in %f.3s (%.2f Mo/s) with utf8 mode %b\n"
    m l d s (utf8 = Utf8.UTF8)
