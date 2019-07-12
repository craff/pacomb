open Combinator
open Lex

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
  | DSeq : 'a ne_grammar * ('a -> 'b grammar) * ('b -> 'c) -> 'c ne_grammar
  | Appl : 'a ne_grammar * ('a -> 'b) -> 'b ne_grammar
  | Lr   : 'a ne_grammar * ('a -> 'a) ne_grammar -> 'a ne_grammar
  | Rest : 'a grammar -> 'a ne_grammar
  | LPos : (pos -> 'a) ne_grammar -> 'a ne_grammar
  | RPos : (pos -> 'a) ne_grammar -> 'a ne_grammar
  | Tmp  : 'a ne_grammar

 and 'a grammar = { mutable e: 'a empty
                  ; mutable g : 'a ne_grammar
                  ; recursive : bool
                  ; n : string; u : 'a ty; eq : 'b. 'b ty -> ('a,'b) eq
                  ; mutable compiled : 'a Combinator.t ref option
                  }

 and 'a empty =
   | EFail
   | Empty of 'a
   | EPos  of (pos -> 'a) (* only one position for empty rule! *)

type 'a t = 'a grammar

let new_key : type a. unit -> a key = fun () ->
  let module M = struct type _ ty += T : a ty end in
  let open M in
  let eq : type b. b ty -> (a, b) eq = function T -> Eq | _ -> NEq in
  { k = T; eq }


let mkg : ?name:string -> ?recursive:bool -> 'a empty -> 'a ne_grammar -> 'a grammar =
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
    | DSeq(g1,_,_) -> pr "%a???" pg g1
    | Appl(g,_) -> pr "App(%a)" pg g
    | Lr(g,s) -> pr "%a(%a)*" pg g pg s
    | Rest(g) -> pr "(%a\\0)" pv g
    | LPos(g) -> pr "%a" pg g
    | RPos(g) -> pr "%a" pg g
    | Tmp     -> pr "TMP"

and print_grammar
    : type a. ety list -> out_channel -> a grammar -> unit = fun adone ch g ->
  let pr x = Printf.fprintf ch x in
  if List.mem (E g.u) adone then Printf.fprintf ch "%s" g.n
  else if g.recursive then
    begin
      let adone = E g.u :: adone in
      let pg x = print_ne_grammar adone x in
      match g.e with
      | EFail -> pr "%s=(%a)" g.n pg g.g
      | _     -> pr "%s=(1|%a)" g.n pg g.g
    end
  else
    begin
      let pg x = print_ne_grammar adone x in
      match g.e with
      | EFail -> pr "%a" pg g.g
      | _     -> pr "(1|%a)" pg g.g
    end

let print_ne_grammar ch s = print_ne_grammar [] ch s
let print_grammar ch s = print_grammar [] ch s

let fail () = mkg ~name:"FAIL" EFail Fail

let empty(x) = mkg ~name:"EMPTY" (Empty x) Fail

let term(x) =
  if accept_empty x then invalid_arg "term: empty terminals";
  mkg ~name:x.Lex.n EFail (Term x)

let get g = if g.recursive then Rest g else g.g

let ne_appl(g,f) =
  match g with
  | Fail -> Fail
  | Appl(g,h) -> Appl(g,fun x -> f (h x))
  | Seq(g1,g2,h) -> Seq(g1,g2,fun x y -> f (h x y))
  | DSeq(g1,g2,h) -> DSeq(g1,g2,fun y -> f (h y))
  | _ -> Appl(g,f)

let appl({e;_} as g,f) =
  let e = match e with
    | EFail   -> EFail
    | Empty x -> Empty (f x)
    | EPos x  -> EPos (fun pos -> f (x pos))
  in
  mkg e (ne_appl(get g,f))

let ne_alt = function
  | Fail, g2 -> g2
  | g1, Fail -> g1
  | (g1,g2) -> Alt(g1,g2)

let alt({e=e1;_} as g1,({e=e2;_} as g2)) =
  let e = match e1 with EFail -> e2 | _ -> e1 in
  mkg e (ne_alt(get g1,get g2))

let ne_lr = function
  | Fail, _ -> Fail
  | g1, Fail -> g1
  | (g1,s) -> Lr(g1,s)

let lr({e=e1;_} as g1,s) = mkg e1 (ne_lr(get g1,get s))

let lpos{e;g;_} =
  let e = match e with
    | EFail -> EFail
    | Empty f -> EPos f
    | EPos f -> EPos(fun pos -> f pos pos)
  in
  let g = LPos(g) in
  mkg e g

let rpos{e;g;_} =
  let e = match e with
    | EFail -> EFail
    | Empty f -> EPos f
    | EPos f -> EPos(fun pos -> f pos pos)
  in
  let g = RPos(g) in
  mkg e g

let ne_seq(g1,g2,f) = match(g1,g2) with
  | Fail, _                 -> Fail
  | _, {e=EFail;g=Fail;_}   -> Fail
  | _, {e=Empty y;g=Fail;_} -> ne_appl(g1,fun x -> f x y)
  | _, _                    -> Seq(g1,g2,f)

let seq : type a b c. a grammar * b grammar * (a -> b -> c) -> c grammar =
  fun ({e=e1;_} as g1,g2,f) ->
  let ga = mkg EFail (ne_seq(get g1,g2,f)) in
  let gb = match e1 with
    | EFail -> fail()
    | Empty x -> appl(g2,f x)
    | EPos x  -> lpos(appl(g2,fun y pos -> f (x pos) y))
  in
  alt(ga,gb)

let ne_dseq(g1,g2,f) = match g1 with
  | Fail  -> Fail
  | _     -> DSeq(g1,g2,f)

let dseq : type a b c. a grammar * (a -> b grammar) * (b -> c) -> c grammar =
  fun ({e=e1;_} as g1,g2,f) ->
  let ga = mkg EFail (ne_dseq(get g1,g2,f)) in
  let gb = match e1 with
    | EFail -> fail()
    | Empty x -> appl(g2 x,f)
    | EPos x  -> appl(g2 (x phantom),f)
                    (* FIXME: loose position, can not be avoided ? *)
  in
  alt(ga,gb)

type 'a with_pos =
  | Nope
  | With of (pos -> 'a) ne_grammar

(* remove a lpos left prefix.
   FIXME: useless if the prefix is not followed by Tpm *)
let rec remove_lpos : type a. a ne_grammar -> a with_pos = function
  | Fail -> Nope
  | Term _ -> Nope
  | Seq(g1,g2,f) ->
     begin
       match remove_lpos g1 with
       | Nope -> Nope
       | With g1 -> With(ne_seq(g1,g2,fun x y pos -> f (x pos) y))
     end
  | DSeq(g1,_,_) ->
     begin
       match remove_lpos g1 with
       | Nope -> Nope
       | With _ -> Nope (* left recursion in not supported under DSeq *)
     end
  | Alt(g1,g2) ->
     begin
       match remove_lpos g1, remove_lpos g2 with
       | Nope, Nope -> Nope
       | With g1, Nope -> With(ne_alt(g1,ne_appl(g2,fun x _pos -> x)))
       | Nope, With g2 -> With(ne_alt(ne_appl(g1,fun x _pos -> x),g2))
       | With g1, With g2 -> With(ne_alt(g1,g2))
     end
  | LPos(g1) ->
     begin
       match remove_lpos g1 with
       | Nope -> With(g1)
       | With g1 -> With(ne_appl(g1,fun f pos -> f pos pos))
     end
  | RPos(g1) ->
     begin
       match remove_lpos g1 with
       | Nope -> Nope
       | With g1 -> With(RPos(ne_appl(g1, fun f posr posl -> f posl posr)))
     end
  | Appl(g1,f) ->
     begin
       match remove_lpos g1 with
       | Nope -> Nope
       | With g1 -> With(ne_appl(g1,fun x pos -> f (x pos)))
     end
  | Lr(g1,s) ->
     begin
       match remove_lpos g1 with
       | Nope -> Nope
       | With g1 -> With(ne_lr(g1,ne_appl(s,fun f g pos -> f (g pos))))
     end
  | Rest(_) -> Nope
  | Tmp -> Nope

(* construction of recursive grammar *)
let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?(name="...") g ->

  let r = mkg ~name ~recursive:true EFail Tmp in
  let {e;g;_} = g r in
  r.e <- e;
  let u = r.u in

  let rec elim_left_rec : type b. b ne_grammar -> b grammar * (a -> b) grammar =
    fun g ->
      match g with
      | Fail -> (fail (), fail ())
      | Term _ -> (mkg EFail g, fail ())
      | Seq(g1,g2,f) ->
         let (g1, s1) = elim_left_rec g1 in
         (seq(g1,g2,f),
          seq(s1,g2,fun fx y a -> f (fx a) y))
      | DSeq(g1,_,_) ->
         let (_, s1) = elim_left_rec g1 in
         if s1.g <> Fail then
           invalid_arg "fixpoint: left recursion under dependant sequence";
         (mkg EFail g, fail())
      | Alt(g1,g2) ->
         let (g1, s1) = elim_left_rec g1 in
         let (g2, s2) = elim_left_rec g2 in
         (alt(g1,g2), alt(s1,s2))
      | Lr(g1,s)   ->
         let (g1,s1) = elim_left_rec g1 in
         let s = mkg EFail s in
         (lr(g1,s),lr(s1,appl(s,fun f g a -> f (g a))))
      | Appl(g1,f) ->
         let (g1,s) = elim_left_rec g1 in
         (appl(g1,f), appl(s,fun g a -> f (g a)))
      | Rest(g) -> elim_e g
      | LPos _ -> assert false
      | RPos(g1) ->
         let (g1, s1) = elim_left_rec g1 in
         (rpos(g1),rpos(appl(s1,fun f pos a -> f a pos)))
      | Tmp -> failwith "unsupported"

   and elim_e : type b. b grammar -> b grammar * (a -> b) grammar =
    fun g ->
    match g.eq u with
    | Eq  -> assert g.recursive; (fail (), empty(fun x -> x))
    | NEq ->
       if g.recursive then (mkg EFail (Rest g), fail()) (* FIXME: handle mutual left recursion *)
       else elim_left_rec g.g
   in
   let g =
     match remove_lpos g with
     | Nope ->
        begin
          Printf.printf "elim %a\n%!" print_ne_grammar g;
          let ({e=e';g;_}, {g=s;_}) = elim_left_rec g in
          assert(e'=EFail);
          Printf.printf "  g = %a\n%!" print_ne_grammar g;
          Printf.printf "  s = %a\n%!" print_ne_grammar s;
          match e with
          | EFail -> ne_lr(g,s)
          | Empty x -> ne_lr(ne_alt(g,ne_appl(s,fun f -> f x)), s)
          | EPos x -> ne_lr(ne_alt(g,LPos(ne_appl(s,fun f pos -> f (x pos)))), s)
        end
     | With g ->
        begin
          Printf.printf "elim %a\n%!" print_ne_grammar g;
          let ({e=e';g;_}, {g=s;_}) = elim_left_rec g in
          assert(e'=EFail);
          Printf.printf "  g = %a\n%!" print_ne_grammar g;
          Printf.printf "  s = %a\n%!" print_ne_grammar s;
          let s' = ne_appl(s,fun f x pos -> f (x pos) pos) in
          match e with
          | EFail -> LPos(ne_lr(g,s'))
          | Empty x -> LPos(ne_lr(ne_alt(g,ne_appl(s,fun f pos -> f x pos)), s'))
          | EPos x -> LPos(ne_lr(ne_alt(g,ne_appl(s,fun f pos -> f (x pos) pos)), s'))
        end
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
    | DSeq(g,_,_) -> fn g
    | Appl(g,_) -> fn g
    | Lr(g,_) -> fn g
    | Rest g -> fn g.g
    | LPos g -> fn g
    | RPos g -> fn g
    | Tmp -> assert false
  in fn g

(* compilation of a grammar to combinator *)
let rec compile_ne : type a. a ne_grammar -> a Combinator.t = fun g ->
  match g with
  | Fail -> cfail
  | Term(c) -> cterm c.f
  | Alt(g1,g2) -> calt ~cs1:(first_charset g1) ~cs2:(first_charset g2)
                       (compile_ne g1) (compile_ne g2)
  | Seq(g1,g2,f) -> cseq (compile_ne g1) (compile g2) f
  | DSeq(g1,g2,f) -> cdep_seq (compile_ne g1) (fun x -> compile (g2 x)) f
  | Appl(g1,f) -> capp (compile_ne g1) f
  | Lr(g,s) -> clr ~cs2:(first_charset s) (compile_ne g) (compile_ne s)
  | Rest g -> compile ~restrict:true g
  | LPos(g) -> clpos (compile_ne g)
  | RPos(g) -> crpos (compile_ne g)
  | Tmp -> assert false

and compile : type a. ?restrict:bool -> a grammar -> a Combinator.t =
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
  | Empty x when not restrict ->
     let e = cempty x in
     if g.g = Fail then e else calt ~cs2:(first_charset g.g) e cg
  | EPos  x when not restrict ->
     let e = clpos (cempty x) in
     if g.g = Fail then e else calt ~cs2:(first_charset g.g) e cg
  | _ ->
     if g.g = Fail then cfail else cg

let compile g = compile ~restrict:false g
