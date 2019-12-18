
(* This test is for measuring the performance of Input *)
open Pacomb

let utf8 = ref Utf8.ASCII

let files = ref []

let spec = Arg.[("--utf8", Unit(fun () ->  utf8 := Utf8.UTF8), "use utf8 mode")]

let _ = Arg.parse spec
          (fun x -> files := x :: ! files)
          "scan [--utf8] files ('-' to read stdin)"

let utf8 = !utf8
let files = if !files = [] then ["-"] else !files

let do_file file =
  let buf =
    if file = "-" then
      Input.from_fd ~utf8 Unix.stdin
    else Input.from_file ~utf8 file
  in
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
  Printf.printf
    "%s: %.2f Mo/%d lines read in %f.3s (%.2f Mo/s) with utf8 mode %b\n"
    file m l d s (utf8 = Utf8.UTF8)

let _ = List.iter do_file files
