open Combinator
open Lex

(* a cell for compilation of recursive grammar, see compile below *)
type 'a comb_memo = { mutable c : 'a comb
                    ; mutable ready : bool
                    ; mutable ae : bool option }

let cref : 'a comb_memo -> 'a comb = fun m -> { c = fun s n -> m.c.c s n }

let init_memo =
  let cassert : type a .a comb =
    { c = fun _b _s _n _k -> assert false }
  in
  fun () -> { c = cassert; ready = false; ae = None }

(* extensible type of key used for elimination of left recursion,
   see elim_left_rec below *)
type _ ty =  ..

(* type of a grammar *)
type 'a grammar =
  | Fail : 'a grammar
  | Vide : 'a -> 'a grammar
  | Term : 'a terminal -> 'a grammar
  | Alt  : 'a grammar * 'a grammar -> 'a grammar
  | Seq  : 'a grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c grammar
  | Appl : 'a grammar * ('a -> 'b) -> 'b grammar
  | Lr   : 'a grammar * ('a -> 'a) grammar -> 'a grammar
  (* Ref below is for internal use only to build recursive grammar *)
  | Ref  : 'a grammar ref * 'a ty * 'a comb_memo -> 'a grammar

 and 'a reduced =  (unit -> 'a) option * (char * 'a grammar Lazy.t) list

let rec print_grammar : type a. out_channel -> a grammar -> unit = fun ch g ->
  let pr x = Printf.fprintf ch x in
  let pg x = print_grammar x in
  match g with
  | Fail -> pr "0"
  | Vide _ -> pr "1"
  | Term _ -> pr "X"
  | Alt(g1,g2) -> pr "(%a|%a)" pg g1 pg g2
  | Seq(g1,g2,_) -> pr "%a%a" pg g1 pg g2
  | Appl(g,_) -> pr "[%a]" pg g
  | Lr(g,s) -> pr "[%a|%a*]" pg g pg s
  | Ref(_) -> pr "..."

let rec accept_empty : type a. a grammar -> bool =
  function
  | Fail -> false
  | Vide _ -> true
  | Term(_) -> false
  | Alt(g1,g2) -> accept_empty g1 || accept_empty g2
  | Seq(g1,g2,_) -> accept_empty g1 && accept_empty g2
  | Ref(ptr,_,m) ->
     begin
       match m.ae with
       | None ->
          m.ae <- Some true;
          let ae = accept_empty !ptr in
          m.ae <- Some ae;
          ae
       | Some ae -> ae
     end
  | Lr(g,_) -> accept_empty g
  | Appl(g,_) -> accept_empty g

(* optimized construtors *)
let appl(g,f) =
  match g with
  | Fail -> Fail
  | Vide x -> Vide(f x)
  | Appl(g,h) -> Appl(g,fun x -> f (h x))
  | Seq(g1,g2,h) -> Seq(g1,g2,fun x y -> f (h x y))
  | _ -> Appl(g,f)

let alt(g1,g2) =
  match (g1,g2) with
  | Fail, _ -> g2
  | _, Fail -> g1
  | _ -> Alt(g1,g2)

let lr(g1,s) =
  match (g1,s) with
  | Fail, _ -> Fail
  | _, Fail -> g1
  | _, Vide _ -> assert false
  | _ -> Lr(g1,s)

let seq : type a b c. a grammar * b grammar * (a -> b -> c) -> c grammar =
  fun (g1,g2,f) ->
    match (g1,g2) with
    | Fail, _ -> Fail
    | _, Fail -> Fail
    | Vide x, Vide y -> Vide(f x y)
    | Vide x, _ -> appl(g2,fun y -> f x y)
    | _, Vide y -> appl(g1,fun x -> f x y)
    | _ -> Seq(g1,g2,f)

let choose o1 o2 = match o1 with None -> o2 | Some _ -> o1

let rec remove_empty : type a. a grammar -> a option * a grammar =
  fun g -> match g with
  | Fail -> (None, Fail)
  | Vide x -> (Some x, Fail)
  | Term(_) -> (None, g)
  | Alt(g1,g2) ->
     let (x1,g1) = remove_empty g1 in
     let (x2,g2) = remove_empty g2 in
     (choose x1 x2, alt(g1, g2))
  | Seq(g1,g2,f) ->
     begin
       let (x1,g1) = remove_empty g1 in
       match x1 with
       | None -> (None, g)
       | Some x1 ->
          let (x2,g2') = remove_empty g2 in
          match x2 with
          | None -> (None, g)
          | Some x2 ->
             let x = try Some(f x1 x2) with NoParse -> None in
             (* FIXME: avoid choose below and try all? *)
             (x, alt(appl(g2',fun y -> f x1 y), seq(g1,g2,f)))
     end
  | Ref(_,_,_) -> (None, g)
  | Lr(g1,s) ->
     begin
       let (x,g1) = remove_empty g1 in
       match x with
       | None -> (None, g)
       | Some x ->
          (Some x, alt(lr(g1,s),lr(seq(Vide x,s,fun x f -> f x),s)))
     end
  | Appl(g1,f) ->
     begin
       let (x,g1) = remove_empty g1 in
       match x with
       | None -> (None, g)
       | Some x ->
          let x = try Some(f x) with NoParse -> None in
          (x, appl(g1,f))
     end

(* construction of recursive grammar *)
let fixpoint : type a. (a grammar -> a grammar) -> a grammar = fun g ->
  let module M = struct type _ ty += T : a ty end in
  let open M in

  let ptr = ref (Fail) in
  let r = Ref(ptr, T, init_memo ()) in

  let rec elim_left_rec : type b. b grammar -> b grammar * (a -> b) grammar =
    fun g ->
      match g with
      | Fail -> Fail, Fail
      | Vide _ -> (g, Fail)
      | Term _ -> (g, Fail)
      | Seq(g1,g2,f) ->
         let (g1, s1) = elim_left_rec g1 in
         if accept_empty g1 then
           begin
             let (_,s2) = elim_left_rec g2 in
             if s2 <> Fail then failwith "unsupported"
           end;
         if s1 = Fail then
           (g, Fail)
         else
           (seq(g1,g2,f),
            seq(s1,g2,fun g y a -> f (g a) y))
      | Alt(g1,g2) ->
         let (g1, s1) = elim_left_rec g1 in
         let (g2, s2) = elim_left_rec g2 in
         (alt(g1,g2), alt(s1,s2))
      | Ref (_,T,_) -> (Fail, Vide(fun x -> x))
      | Ref (_,_,_) -> (g, Fail)
      | Lr(g1,s)   ->
         let (g1,s1) = elim_left_rec g1 in
         if accept_empty g then
           begin
             let (_,s2) = elim_left_rec s in
             if s2 <> Fail then failwith "unsupported";
           end;
         if s1 = Fail then (g, Fail)
         else (lr(g1,s),lr(s1,appl(s,fun f g a -> f (g a))))
      | Appl(g1,f) ->
         let (g1,s) = elim_left_rec g1 in
         (appl(g1,f), appl(s,fun g a -> f (g a)))
   in
   let (g, s) = elim_left_rec (g r) in
   let (_, s) = remove_empty s in
   let r = lr(g, s) in
   ptr := r;
   r


(* compilation of a grammar to combinator *)
let use_info = true
let rec compile : type a. a grammar -> a comb = fun g ->
  match g with
  | Fail -> cfail
  | Vide x -> cempty x
  | Term(c) -> cterm c
  | Alt(g1,g2) ->
     let ae = accept_empty g2 in
     let (g1,g2) = if ae then (g1,g2) else (g2,g1) in
     calt (compile g1) (compile g2)
  | Seq(g1,g2,f) -> cseq (compile g1) (compile g2) f
  | Appl(g1,f) -> capp (compile g1) f
  | Lr(g,s) -> clr (compile g) (compile s)
  | Ref(ptr,_,m) ->
     if not m.ready then (m.ready <- true; m.c <- compile !ptr);
     cref m
