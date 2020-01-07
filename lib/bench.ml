
let create () = ref []

let parse_string bench gram blank s =
  let _ = Grammar.compile gram in
  let _ = Gc.compact () in
  let size = String.length s in
  let t0 = Unix.gettimeofday () in
  let r = Grammar.parse_string gram blank s in
  let t1 = Unix.gettimeofday () in
  let dw = (Gc.quick_stat ()).top_heap_words in
  let dt = t1 -. t0 in
  bench := (size, dt, float dw) :: !bench;
  (r, dt, dw)

(* ln(C N^e) = c + e ln(N) + f ln(ln(N)) *)
module Base = struct
  type input = int
  let base = [| (fun _ -> 1.0)
              ; (fun n -> log (float (n + 1)))
             |]
end

module Interpolate = Interpolate.Make(Base)

let stats bench =
  let open Interpolate in
  let time = Array.of_list (List.map (fun (n,t,_) -> (n, log(t))) !bench) in
  let coefs = get (compute_coefs time) in
  Printf.printf "Time ~ %g N^%g\n%!" (exp coefs.(0)) coefs.(1);
  let space = Array.of_list (List.map (fun (n,_,s) -> (n, log(s))) !bench) in
  let coefs = get (compute_coefs space) in
  Printf.printf "Space ~ %g N^%g\n%!" (exp coefs.(0)) coefs.(1)
