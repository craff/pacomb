open Combinator
open Lex

(* a cell for compilation of recursive grammar, see compile below *)
type 'a comb_memo = { mutable c : 'a comb
                    ; mutable ready : bool
                    ; mutable ae : bool option }

let cref : 'a comb ref -> 'a comb = fun ptr -> { c = fun s0 n0 s n -> !ptr.c s0 n0 s n }

let init_memo =
  let cassert : type a .a comb =
    { c = fun _b _s0 _n0 _s _n _k -> assert false }
  in
  fun () -> { c = cassert; ready = false; ae = None }

(* extensible type of key used for elimination of left recursion,
   see elim_left_rec below *)
type _ ty =  ..
type ('a,'b) eq = NEq : ('a, 'b) eq | Eq : ('a, 'a) eq
type 'a key = { k : 'a ty; eq : 'b.'b ty -> ('a,'b) eq }

(* type of a grammar *)
type 'a ne_grammar =
  | Fail : 'a ne_grammar
  | Term : 'a terminal -> 'a ne_grammar
  | Alt  : 'a ne_grammar * 'a ne_grammar -> 'a ne_grammar
  | Seq  : 'a ne_grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c ne_grammar
  | Appl : 'a ne_grammar * ('a -> 'b) -> 'b ne_grammar
  | Lr   : 'a ne_grammar * ('a -> 'a) ne_grammar -> 'a ne_grammar
  | Rest : 'a grammar -> 'a ne_grammar
  | Tmp  : 'a ne_grammar

 and 'a grammar = { mutable e: 'a option
                  ; mutable g : 'a ne_grammar
                  ; recursive : bool
                  ; n : string; u : 'a ty; eq : 'b. 'b ty -> ('a,'b) eq
                  ; mutable compiled : 'a comb ref option
                  }

let new_key : type a. unit -> a key = fun () ->
  let module M = struct type _ ty += T : a ty end in
  let open M in
  let eq : type b. b ty -> (a, b) eq = function T -> Eq | _ -> NEq in
  { k = T; eq }


let mkg : ?name:string -> ?recursive:bool -> 'a option -> 'a ne_grammar -> 'a grammar =
  fun ?(name="...") ?(recursive=false) e g ->
    let k = new_key () in
    { e; g; n = name; u = k.k; eq = k.eq; recursive; compiled = None }

type ety = E : 'a ty -> ety [@@unboxed]

let rec print_ne_grammar
    : type a. ety list -> out_channel -> a ne_grammar -> unit =
  fun adone ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_ne_grammar adone x in
    let pv x = print_grammar adone x in
    match g with
    | Fail -> pr "0"
    | Term t -> pr "%s" t.n
    | Alt(g1,g2) -> pr "(%a|%a)" pg g1 pg g2
    | Seq(g1,g2,_) -> pr "%a%a" pg g1 pv g2
    | Appl(g,_) -> pr "App(%a)" pg g
    | Lr(g,s) -> pr "%a(%a)*" pg g pg s
    | Rest(g) -> pr "(%a\\0)" pv g
    | Tmp     -> pr "TMP"

and print_grammar
    : type a. ety list -> out_channel -> a grammar -> unit = fun adone ch {n;e;g;u;_} ->
  let pr x = Printf.fprintf ch x in
  if List.mem (E u) adone then Printf.fprintf ch "%s" n
  else
    begin
      let adone = E u :: adone in
      let pg x = print_ne_grammar adone x in
      match e with
      | None -> pr "%a" pg g
      | Some _ -> pr "(1|%a)" pg g
    end

let print_ne_grammar ch s = print_ne_grammar [] ch s
let print_grammar ch s = print_grammar [] ch s

let fail () = mkg ~name:"FAIL" None Fail

let empty(x) = mkg ~name:"EMPTY" (Some x) Fail

let term(x) = mkg ~name:x.Lex.n None (Term x)

let get g = if g.recursive then Rest g else g.g

let ne_appl(g,f) =
  match g with
  | Fail -> Fail
  | Appl(g,h) -> Appl(g,fun x -> f (h x))
  | Seq(g1,g2,h) -> Seq(g1,g2,fun x y -> f (h x y))
  | _ -> Appl(g,f)

let appl({e;_} as g,f) =
  let e = match e with None -> None | Some x -> Some (f x) in
  mkg e (ne_appl(get g,f))

let ne_alt = function
  | Fail, g2 -> g2
  | g1, Fail -> g1
  | (g1,g2) -> Alt(g1,g2)

let alt({e=e1;_} as g1,({e=e2;_} as g2)) =
  let e = match e1 with None -> e2 | Some _ -> e1 in
  mkg e (ne_alt(get g1,get g2))

let ne_lr = function
  | Fail, _ -> Fail
  | g1, Fail -> g1
  | (g1,s) -> Lr(g1,s)

let lr({e=e1;_} as g1,s) = mkg e1 (ne_lr(get g1,get s))

let ne_seq(g1,g2,f) = match(g1,g2) with
  | Fail, _                -> Fail
  | _, {e=None;g=Fail;_}   -> Fail
  | _, {e=Some y;g=Fail;_} -> Appl(g1,fun x -> f x y)
  | _, _                   -> Seq(g1,g2,f)

let seq : type a b c. a grammar * b grammar * (a -> b -> c) -> c grammar =
  fun ({e=e1;_} as g1,g2,f) ->
  let ga = mkg None (ne_seq(get g1,g2,f)) in
  let gb = match e1 with
    | None -> fail()
    | Some x -> appl(g2,f x)
  in
  alt(ga,gb)

(* construction of recursive grammar *)
let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?(name="") g ->

  let r = mkg ~name ~recursive:true None Tmp in
  let {e;g;_} = g r in
  r.e <- e;
  let u = r.u in

  let rec elim_left_rec : type b. b ne_grammar -> b grammar * (a -> b) grammar =
    fun g ->
      match g with
      | Fail -> (fail (), fail ())
      | Term _ -> (mkg None g, fail ())
      | Seq(g1,g2,f) ->
         let (g1, s1) = elim_left_rec g1 in
         (seq(g1,g2,f),
          seq(s1,g2,fun fx y a -> f (fx a) y))
      | Alt(g1,g2) ->
         let (g1, s1) = elim_left_rec g1 in
         let (g2, s2) = elim_left_rec g2 in
         (alt(g1,g2), alt(s1,s2))
      | Lr(g1,s)   ->
         let (g1,s1) = elim_left_rec g1 in
         let s = mkg None s in
         (lr(g1,s),lr(s1,appl(s,fun f g a -> f (g a))))
      | Appl(g1,f) ->
         let (g1,s) = elim_left_rec g1 in
         (appl(g1,f), appl(s,fun g a -> f (g a)))
      | Rest(g) -> elim_e g
      | Tmp -> failwith "unsupported"

   and elim_e : type b. b grammar -> b grammar * (a -> b) grammar =
    fun g ->
    match g.eq u with
    | Eq  -> assert g.recursive; (fail (), empty(fun x -> x))
    | NEq ->
       if g.recursive then (mkg None (Rest g), fail()) (* FIXME: handle mutual left recursion *)
       else elim_left_rec g.g
   in
   Printf.printf "elim %a\n%!" print_ne_grammar g;
   let ({e=e';g;_}, {g=s;_}) = elim_left_rec g in
   assert(e'=None);
   Printf.printf "  g = %a\n%!" print_ne_grammar g;
   Printf.printf "  s = %a\n%!" print_ne_grammar s;
   let g =
     match e with
     | None -> ne_lr(g,s)
     | Some x -> ne_lr(ne_alt(g,ne_appl(s,fun f -> f x)), s)
   in
   Printf.printf "  g = %a\n%!" print_ne_grammar g;
   r.g <- g;
   r


let first_charset : type a. a ne_grammar -> Charset.t = fun g ->
  let rec fn : type a. a ne_grammar -> Charset.t = fun g ->
    match g with
    | Fail -> Charset.empty
    | Term(c) -> c.c
    | Alt(g1,g2) -> Charset.union (fn g1) (fn g2)
    | Seq(g,_,_) -> fn g
    | Appl(g,_) -> fn g
    | Lr(g,_) -> fn g
    | Rest g -> fn g.g
    | Tmp -> assert false
  in fn g

(* compilation of a grammar to combinator *)
let use_info = true
let rec compile_ne : type a. a ne_grammar -> a comb = fun g ->
  match g with
  | Fail -> cfail
  | Term(c) -> cterm c.f
  | Alt(g1,g2) -> calt (first_charset g1) (compile_ne g1)
                       (first_charset g2) (compile_ne g2)
  | Seq(g1,g2,f) -> cseq (compile_ne g1) (compile g2) f
  | Appl(g1,f) -> capp (compile_ne g1) f
  | Lr(g,s) -> clr (compile_ne g) (first_charset s) (compile_ne s)
  | Rest g -> compile ~restrict:true g
  | Tmp -> assert false

and compile : type a. ?restrict:bool -> a grammar -> a comb =
  fun ?(restrict=false) g ->
  let cg =
    match g.recursive, g.compiled with
    | true, Some ptr -> cref ptr
    | true, None ->
       let ptr = ref cfail in
       g.compiled <- Some ptr;
       ptr := compile_ne g.g;
       cref ptr
    | false, Some _ -> assert false
    | false, None -> compile_ne g.g
  in
  match g.e with
  | Some x when not restrict ->
     if g.g = Fail then cempty x else calt (first_charset g.g) cg
                                           Charset.full (cempty x)
  | _ ->
     if g.g = Fail then cfail else cg
