open Pacomb

(* This (useless) example illustrate the use of merge on ambiguous grammars. We
   define two ambiguous grammars parsing sequences of characters to generate all
   binary and ternary trees.

   "catalan 10 2" will compute number of binary trees of size 0 to 10
   "catalan 10 3" will compute number of ternary trees of size 0 to 10

*)


(* A parser parsing arbitrary sequence of charaters 'a' as a set of binary
   trees. We use merge in case of ambiguity and, as [@merge ...] implies
   [@cache] pacomb parses only once each part of the input. We just return the
   number of trees. *)
let%parser [@merge (+.)] rec bin_seq =
    ()                              => 1.0
  ; (t1::bin_seq) 'a' (t2::bin_seq) => t1 *. t2

let plus ~start ~end_ x y =
  let pos = Pos.mk_pos start end_ Input.phantom_infos in
  Format.printf "merge at %a\n" (fun fmt p -> Pos.print_pos () fmt p) pos;
  x +. y

let%parser [@merge_with_pos plus] rec bin_seq =
    ()                              => 1.0
  ; (t1::bin_seq) 'a' (t2::bin_seq) => t1 *. t2

let _ = Grammar.print_grammar stdout ~def:false bin_seq; print_newline ()

(* Idem for ternary tree, we need an internal cache to achieve polynomial
   complexity. *)
let%parser [@merge (+.)] rec ter_seq =
    ()                             => 1.0
  ; (t1::ter_seq) 'a' (t2t3::Grammar.cache ~merge:(Merge (+.))
                           ((t2::ter_seq) (t3::ter_seq) => t2 *. t3))
    => t1 *. t2t3

let _ = Grammar.print_grammar stdout ~def:false ter_seq; print_newline ()

(* To test, here is Catalan number, i.e. the number of binary trees of a given size *)
let catalan =
  let memo = Hashtbl.create 128 in
  let rec fn n =
    if n <= 1 then 1.0 else
    try Hashtbl.find memo n
    with Not_found ->
      let r = ref 0.0 in
      for i = 0 to n-1 do
        r := fn i *. fn (n - i - 1) +. !r
      done;
      Hashtbl.add memo n !r;
      !r
  in
  fn

(* idem for ternary trees *)
let catalan3 =
  let memo = Hashtbl.create 128 in
  let rec fn n =
    if n <= 1 then 1.0 else
    try Hashtbl.find memo n
    with Not_found ->
      let r = ref 0.0 in
      for i = 0 to n-1 do
        for j = 0 to n-i-1 do
          r := fn i *. fn j *. fn (n - i - j - 1) +. !r
        done
      done;
      Hashtbl.add memo n !r;
      !r
  in
  fn

(* parsing command line *)
let catalan_max, branches =
  try
    if Array.length Sys.argv <> 3 then raise Not_found;
    int_of_string Sys.argv.(1), int_of_string Sys.argv.(2)
  with _ ->
    Printf.eprintf "usage: %s max_len [2|3]\n%!" Sys.argv.(0);
    exit 1

let _ =
  Printf.printf "checking the number of parsetrees on an ambiguous example,\
                 using merge and cache\n%!";
  let (p,f) = if branches = 2 then bin_seq, catalan
              else ter_seq, catalan3
  in
  let bench = Bench.create () in
  for i = 0 to catalan_max do
    let s = String.make i 'a' in
    let (t,dt,w) = Bench.parse_string bench p Blank.none s in
    let k = f i in
    Printf.printf "catalan(%d): %d => %e=%e in %.2fms %.2f Mb \n%!"
      branches i t k (1000. *. dt) (float w /. 1024. /. 1024. *. float Sys.word_size);
  done;
  Bench.stats "catalan " bench;
  Bench.csv bench (Printf.sprintf "catalan%d.csv" branches)
