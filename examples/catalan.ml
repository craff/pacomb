open Pacomb

(* This (useless) example illustrate the use of merge
   on ambiguous grammars. We an ambiguous grammar parsing
   sequences of characters to generate all binary trees,
   but we "merge" sub tree to keep a polynomial behaviour *)

(* Here is a type to represent set of trees *)
type trees =
  | Nil                  (* the empty tree *)
  | Uni of trees * trees * int (* union of two sets *)
  | Bin of trees * trees * int (* all binary nodes with one tree in each set *)

(* the above type carries the cardinal of the set becaue computing it
   is hard as we have a lot of sharing *)
let rec uni x y = Uni(x,y,nb_tree x + nb_tree y)

and bin x y = Bin(x,y,nb_tree x * nb_tree y)

and nb_tree = function
  | Nil -> 1 | Uni(_,_,s) | Bin(_,_,s) -> s

(* A parser parsing arbitrary sequence of charaters 'a' as a set of binary
   trees. We use merge in case of ambiguity and pacomb keep sharing when we
   parse twice the same part of the input (@merge implies @cache) *)
let%parser rec bin_seq =
    ()                             => Nil
  ; (t1::bin_seq) 'a' (t2::bin_seq) => bin t1 t2

(* function computing Catalan number, i.e. the number of binary trees of a given
   size *)
let catalan =
  let memo = Hashtbl.create 128 in
  let rec fn n =
    if n = 0 then 1 else if n = 1 then 1 else
    try Hashtbl.find memo n
    with Not_found ->
      let r = ref 0 in
      for i = 0 to n-1 do
        r := fn i * fn (n - i - 1) + !r
      done;
      Hashtbl.add memo n !r;
      !r
  in
  fn

let catalan_max =
  try
    if Array.length Sys.argv <> 2 then raise Not_found;
    int_of_string Sys.argv.(1)
  with _ ->
    Printf.eprintf "usage: %s max_len\n%!" Sys.argv.(0);
    exit 1

let _ =
  Printf.printf "checking the number of parsetrees on an ambiguous example,\
                 using merge and cache\n%!";
  for i = 0 to catalan_max  do
    let s = String.make i 'a' in
    let t0 = Unix.gettimeofday () in
    let t = Grammar.parse_string (Grammar.cache ~merge:uni bin_seq) Lex.noblank s in
    let t1 = Unix.gettimeofday () in
    let j = nb_tree t in
    let k = catalan i in
    Printf.printf "catalan: %d => %d=%d in %.2fms\n%!"
      i j k (1000. *. (t1 -. t0));
    assert (j = k)
  done
