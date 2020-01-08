
let create () =
  (* when bench are created, no more compaction to keep it manual *)
  Gc.(set { (get ()) with max_overhead = 1_000_000 });
  ref []

let parse_string bench gram blank s =
  let _ = Grammar.compile gram in
  let _ = Gc.compact () in
  let size = String.length s in
  let t0 = Unix.gettimeofday () in
  let r = Grammar.parse_string gram blank s in
  let t1 = Unix.gettimeofday () in
  let dw = (Gc.quick_stat ()).heap_words in
  let dt = t1 -. t0 in
  bench := (size, dt, float dw) :: !bench;
  (r, dt, dw)

let parse_ch bench gram blank size ch =
  let _ = Grammar.compile gram in
  let _ = Gc.compact () in
  let t0 = Unix.gettimeofday () in
  let r = Grammar.parse_channel ~rescan:false gram blank ch in
  let t1 = Unix.gettimeofday () in
  let dw = (Gc.quick_stat ()).heap_words in
  let dt = t1 -. t0 in
  bench := (size, dt, float dw) :: !bench;
  (r, dt, dw)

let parse_fd bench gram blank size fd =
  let _ = Grammar.compile gram in
  let _ = Gc.compact () in
  let t0 = Unix.gettimeofday () in
  let r = Grammar.parse_fd ~rescan:false gram blank fd in
  let t1 = Unix.gettimeofday () in
  let dw = (Gc.quick_stat ()).heap_words in
  let dt = t1 -. t0 in
  bench := (size, dt, float dw) :: !bench;
  (r, dt, dw)

let parse_yacc main lex ch =
  let _ = Gc.compact () in
  let t0 = Unix.gettimeofday () in
  let lexbuf = Lexing.from_channel ch in
  let r = main lex lexbuf in
  let t1 = Unix.gettimeofday () in
  let dw = (Gc.quick_stat ()).heap_words in
  let dt = t1 -. t0 in
  (r, dt, dw)

let parse_pipe bench gram blank size producer =
  let (fd_in, fd_out) = Unix.pipe () in
  let pid = Unix.fork () in
  if pid = 0 then
    (Unix.close fd_in;
     let ch_out = Unix.out_channel_of_descr fd_out in
     let _ = producer ch_out in
     close_out ch_out;
     exit 0)
  else
    (Unix.close fd_out;
     (parse_fd bench gram blank size fd_in))

let yacc_pipe bench entry lex size producer =
  let (fd_in, fd_out) = Unix.pipe () in
  let pid = Unix.fork () in
  if pid = 0 then
    (Unix.close fd_in;
     let ch_out = Unix.out_channel_of_descr fd_out in
     let _ = producer ch_out in
     close_out ch_out;
     exit 0)
  else
    (Unix.close fd_out;
     let ch_in = Unix.in_channel_of_descr fd_in in
     let (res_in, res_out) = Unix.pipe () in
     (* Parsing.clear_parser () does not seem to work! we fork again! *)
     let pid = Unix.fork () in
     if pid = 0 then
       (Unix.close res_in;
        let ch_out = Unix.out_channel_of_descr res_out in
        let res = parse_yacc entry lex ch_in in
        output_value ch_out res;
        flush ch_out;
        close_out ch_out; exit 0)
     else
       (Unix.close res_out;
        let ch_in = Unix.in_channel_of_descr res_in in
        let ((_,dt,dw) as r) = input_value ch_in in
        bench := (size, dt, float dw) :: !bench;
        close_in ch_in;
        r))

let size producer =
  let null = open_out "/dev/null" in
  let size = producer null in
  close_out null;
  size


(* ln(C N^e) = c + e ln(N) + f ln(ln(N)) *)
module Base = struct
  type input = int
  let base = [| (fun _ -> 1.0)
              ; (fun n -> log (float (n + 1)))
             |]
end

module Interpolate = Interpolate.Make(Base)

let corr samples =
  let nb = float (Array.length samples) in
  let sx, sy =
    Array.fold_left (fun (sx,sy) (n,y) ->
        let x = log (float (1+n)) in
        (sx +. x, sy +. y))
      (0.,0.) samples
  in
  let ax  = sx  /. nb in
  let ay  = sy  /. nb in
  let dxy, dx2, dy2 =
    Array.fold_left (fun (dxy,dx2,dy2) (n,y) ->
        let x = log (float (1+n)) in
        ( dxy +. (x -. ax) *. (y -. ay)
        , dx2 +. (x -. ax) *. (x -. ax)
        , dy2 +. (y -. ay) *. (y -. ay)))
      (0.,0.,0.) samples
  in
  dxy /. sqrt (dx2 *. dy2)


let stats msg bench =
  let open Interpolate in
  let time = Array.of_list (List.map (fun (n,t,_) -> (n, log(t))) !bench) in
  let coefs = compute_coefs time in
  let c = corr time in
  let coefs = get coefs in
  Printf.printf "%s Time ~ %g N^%g (corr: %.2f)\n%!"
    msg (exp coefs.(0)) coefs.(1) c;
  let space = Array.of_list (List.map (fun (n,_,s) -> (n, log(s))) !bench) in
  let coefs = compute_coefs space in
  let c = corr space in
  let coefs = get coefs in
  Printf.printf "%s Space ~ %g N^%g (corr: %.2f)\n%!"
    msg (exp coefs.(0)) coefs.(1) c

let csv bench file =
  let replace = String.map (function '.' -> ',' | c -> c) in
  let ch = open_out file in
  let prf ch x =
    output_string ch (replace (Printf.sprintf "%f" x))
  in
  let m = List.length !bench + 3 in
  Printf.fprintf ch "pente(t) corr(t) pente(s) corr(s)\n%!";
  Printf.fprintf ch "N ln(1+N) T ln(T) S ln(S)\n%!";
  Printf.fprintf ch "=pente($D$4:$D$%d;$B$4:$B$%d) "
    m m;
  Printf.fprintf ch "=coefficient.correlation($D$4:$D$%d;$B$4:$B$%d) "
    m m;
  Printf.fprintf ch "=pente($F$4:$F$%d;$B$4:$B$%d) "
    m m;
  Printf.fprintf ch "=coefficient.correlation($F$4:$F$%d;$B$4:$B$%d)\n%!"
    m m;
  List.iter (fun (n, t, s) ->
      Printf.fprintf ch "%d %a %a %a %a %a\n%!" n
        prf (log (float (1 + n)))
        prf t
        prf (log t)
        prf s
        prf (log s)) (List.rev !bench);
  close_out ch
