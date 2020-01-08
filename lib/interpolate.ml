
let solve mat vector =
  let dim = Array.length vector in

  for i = 0 to dim - 2 do

    let pivot, pivot_val =
      let r = ref (-1, 0.0) in
      for j = i to dim - 1 do
	let v = abs_float mat.(j).(i) in
	if v > snd !r then r := (j, v)
      done;
      !r
    in
    if pivot = -1 then failwith "non invertible";
    for j = i to dim-1 do
      let v = mat.(pivot).(j) in
      mat.(pivot).(j) <- mat.(i).(j);
      mat.(i).(j) <- v
    done;
    let v = vector.(pivot) in
    vector.(pivot) <- vector.(i);
    vector.(i) <- v;

    for j = i+1 to dim-1 do
      let v = mat.(j).(i) in
      mat.(j).(i) <- 0.0;
      for k = i+1 to dim-1 do
	mat.(j).(k) <- mat.(j).(k) -. v *. mat.(i).(k) /. pivot_val
      done;
      vector.(j) <- vector.(j) -. v *. vector.(i) /. pivot_val
    done;

  done;

  let r = Array.copy vector in

  for i = dim - 1 downto 0 do
    for j = i + 1 to dim - 1 do
      r.(i) <- r.(i) -. r.(j) *. mat.(i).(j)
    done;
    r.(i) <- r.(i) /. mat.(i).(i)
  done;

  r

type 'a base = ('a -> float) array

module type Base = sig
  type input
  val base : input base
end

module type Interpolation = sig
  type input
  type interpolation

  val get : interpolation -> float array

  val zero : interpolation

  val compute : interpolation -> input -> float

  val compute_coefs : (input * float) array -> interpolation

  val correlation :  (input * float) array -> interpolation -> float

  val print : out_channel -> interpolation -> unit
end

module Make(B:Base) = struct

  let funs = B.base
  let dim = Array.length funs

  let get x = x

  let zero = Array.make dim 0.0

  type input = B.input
  type interpolation = float array

  let compute (coefs:float array) (x:input) =
    let r = ref 0.0 in
    for i = 0 to dim - 1 do
      r := !r +. coefs.(i) *. funs.(i)(x)
    done;
    !r

  let compute_coefs (samples:(input * float) array) =
    let ns = Array.length samples in
    let m i j = funs.(i)(fst samples.(j)) in
    let v j = snd samples.(j) in
    let a =
      Array.init dim (fun i ->
      Array.init dim (fun i' ->
                   let r = ref 0.0 in
                   for j = 0 to ns - 1 do
                     r := !r +. m i j *. m i' j
                   done;
                 !r))
    in
    let b =
      Array.init dim (fun i ->
                   let r = ref 0.0 in
                   for j = 0 to ns - 1 do
                     r := !r +. m i j *. v j
                   done;
                   !r)
    in
    solve a b

  let print ch a =
    Printf.fprintf ch "(";
    for i = 0 to dim - 1 do
      Printf.fprintf ch "%s%e" (if i > 0  then ", " else "") a.(i)
    done;
    Printf.fprintf ch ")"

  let correlation (samples:(input * float) array) coefs =
    let nb  = float (Array.length samples) in
    let avg = Array.fold_left (fun s (_, x) -> x +. s) 0.0 samples /. nb in
    let error = Array.fold_left (fun e (n, x) ->
                    let dx = x -. compute coefs n in
                    e +. dx *. dx) 0.0  samples
    in
    sqrt (error /. nb) /. abs_float avg

end

(*
let pi = acos(-1.0)

module TestBase = struct
  type input = float
  let base = [| (fun _ -> 1.0); cos; sin |]
end

module Test = Make(TestBase)

let test0 () =
  let samples = [| (0.0, 1.0); (pi/.2.0, 1.0); (pi, 1.0) |] in
  let coefs = Test.compute_coefs samples in
  Printf.printf "%a\n%!" Test.print coefs;
  for i = 0 to 100 do
    let x = float i /. 50.0 *. pi in
    let error = Test.compute coefs x -. 1.0 in
    assert (abs_float error < 1e-12)
  done

let test1 () =
  let samples = [| (0.0, 1.0); (pi/.2.0, 0.0); (pi, -1.0) |] in
  let coefs = Test.compute_coefs samples in
  Printf.printf "%a\n%!" Test.print coefs;
  for i = 0 to 100 do
    let x = float i /. 50.0 *. pi in
    let error = Test.compute coefs x -. cos x in
    assert (abs_float error < 1e-12)
  done

let test2 () =
  let samples = [| (0.0, 0.0); (pi/.2.0, 1.0); (pi, 0.0) |] in
  let coefs = Test.compute_coefs samples in
  Printf.printf "%a\n%!" Test.print coefs;
  for i = 0 to 100 do
    let x = float i /. 50.0 *. pi in
    let error = Test.compute coefs x -. sin x in
    assert (abs_float error < 1e-12)
  done

let test3 () =
  let samples = [| (0.0, 2.0); (pi/.2.0, 2.0); (pi, 0.0) |] in
  let coefs = Test.compute_coefs samples in
  Printf.printf "%a\n%!" Test.print coefs;
  for i = 0 to 100 do
    let x = float i /. 50.0 *. pi in
    let error = Test.compute coefs x -. sin x -. cos x -. 1.0 in
    assert (abs_float error < 1e-12)
  done

let _ = test0(); test1(); test2(); test3()
 *)
