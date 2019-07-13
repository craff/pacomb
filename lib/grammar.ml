open Combinator
open Lex
open Assoc

(* type of a grammar *)
type 'a ne_grammar =
  | Fail : 'a ne_grammar
  | Term : 'a terminal -> 'a ne_grammar
  | Alt  : 'a ne_grammar * 'a ne_grammar -> 'a ne_grammar
  | Seq  : 'a ne_grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c ne_grammar
  | DSeq : 'a ne_grammar * ('a -> 'b grammar) * ('b -> 'c) -> 'c ne_grammar
  | Appl : 'a ne_grammar * ('a -> 'b) -> 'b ne_grammar
  | Lr   : 'a ne_grammar * ('a -> 'a) ne_grammar -> 'a ne_grammar
  | Ref  : 'a grammar -> 'a ne_grammar
  | Push : 'a ne_grammar -> 'a ne_grammar
  | Read : int * (pos -> 'a) ne_grammar -> 'a ne_grammar
  | RPos : (pos -> 'a) ne_grammar -> 'a ne_grammar
  | Layout : 'a ne_grammar * blank * bool * bool * bool * bool -> 'a ne_grammar
  | Tmp  : 'a ne_grammar

 and 'a grammar = { mutable e: 'a empty
                  ; mutable g : 'a ne_grammar
                  ; recursive : bool
                  ; n : string; u : 'a ty; eq : 'b. 'b ty -> ('a,'b) eq
                  ; mutable compiled : 'a Combinator.t ref option
                  ; mutable charset : Charset.t option
                  }

 and 'a empty =
   | EFail
   | Empty of 'a

type 'a t = 'a grammar

let mkg : ?name:string -> ?recursive:bool -> 'a empty -> 'a ne_grammar -> 'a grammar =
  fun ?(name="...") ?(recursive=false) e g ->
    let k = new_key () in
    { e; g; n = name; u = k.k; eq = k.eq; recursive; compiled = None; charset = None }

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
    | Ref(g)  -> pr "(%a\\0)" pv g
    | Push(g) -> pr "%a" pg g
    | Read(_,g) -> pr "%a" pg g
    | RPos(g) -> pr "%a" pg g
    | Layout(g,_,_,_,_,_) -> pr "%a" pg g
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

let get g = if g.recursive then Ref g else g.g

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
    | Empty x -> (try Empty (f x) with NoParse -> EFail)
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

let push{e;g;_} =
  let g = match g with Fail -> Fail | _ -> Push(g) in
  mkg e g

let read(n, {e;g;_}) =
  let e = match e with
    | EFail -> EFail
    | Empty f -> try Empty(f phantom) with NoParse -> EFail
  in
  let g = match g with Fail -> Fail | _ -> Read(n,g) in
  mkg e g

let lpos g = push(read(0, g))

let rpos{e;g;_} =
  let e = match e with
    | EFail -> EFail
    | Empty f -> try Empty(f phantom) with NoParse -> EFail
  in
  let g = match g with Fail -> Fail | _ -> RPos(g) in
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
  in
  alt(ga,gb)

let ne_dseq(g1,g2,f) = match g1 with
  | Fail  -> Fail
  | _     -> DSeq(g1,g2,f)

(* FIXME: add a cache *)
let dseq : type a b c. a grammar * (a -> b grammar) * (b -> c) -> c grammar =
  fun ({e=e1;_} as g1,g2,f) ->
  let ga = mkg EFail (ne_dseq(get g1,g2,f)) in
  let gb = match e1 with
    | EFail -> fail()
    | Empty x -> appl(g2 x,f)
  in
  alt(ga,gb)

let ne_layout(g,b,ob,nb,na,oa) =
  match g with
  | Fail -> Fail
  | _ -> Layout(g,b,ob,nb,na,oa)

let layout ?(old_before=true) ?(new_before=false)
           ?(new_after=false) ?(old_after=true) (g,b) =
  mkg g.e (ne_layout(g.g,b,old_before,new_before,new_after,old_after))

(* remove a lpos left prefix.
   FIXME: useless if the prefix is not followed by Tmp *)
let remove_push : type a b. b ty -> a ne_grammar -> bool * a ne_grammar =
  fun k g ->
    let found = ref false in
    let rec fn : type a. bool -> a ne_grammar -> a ne_grammar =
      fun r -> function
      | Seq(g1,g2,f) -> ne_seq(fn r g1,g2,f)
      | DSeq(g1,g2,f) -> ne_dseq(fn r g1,g2,f)
      | Alt(g1,g2) -> ne_alt(fn r g1, fn r g2)
      | Push(g1) -> fn true g1
      | Read(n,g1) -> Read((if r then n else n+1), fn r g1)
      | RPos(g1) -> RPos(fn r g1)
      | Appl(g1,f) -> ne_appl(fn r g1,f)
      | Lr(g1,s) -> ne_lr(fn r g1,s)
      | Layout(g1,b,ob,nb,na,oa) -> ne_layout(fn r g1, b,ob,nb,na,oa)
      | Ref g0 as g ->
         if r && (match g0.eq k with Eq -> true | _ -> false)
         then found := true;
         g
      | g -> g
    in
    let r = fn false g in
    if !found then (true, r) else (false, g)

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
      | Ref(g) -> elim_e g
      | Push(g1) as g ->
         assert ((snd (elim_left_rec g1)).g = Fail);
         (mkg EFail g, fail())
      | Read(n,g1) ->
         let (g1, s1) = elim_left_rec g1 in
         (read(n,g1),read(n,appl(s1,fun f pos a -> f a pos)))
      | RPos(g1) ->
         let (g1, s1) = elim_left_rec g1 in
         (rpos(g1),rpos(appl(s1,fun f pos a -> f a pos)))
      | Layout(g1,_,_,_,_,_) ->
         let (_, s1) = elim_left_rec g1 in
         if s1.g <> Fail then
           invalid_arg "fixpoint: left recursion under layout change";
         (mkg EFail g, fail())
      | Tmp -> failwith "unsupported"

   and elim_e : type b. b grammar -> b grammar * (a -> b) grammar =
     fun g ->
       assert g.recursive;
       match g.eq u with
       | Eq  -> (fail (), empty(fun x -> x))
       | NEq -> (mkg g.e (Ref g), fail())
   in
   let (push, g) = remove_push u g in
   let g =
     let ({e=_e';g;_}, {g=s;_}) = elim_left_rec g in
     (*assert(e'=EFail);*)
     match e with
     | EFail -> ne_lr(g,s)
     | Empty x -> ne_lr(ne_alt(g,ne_appl(s,fun f -> f x)), s)
   in
   let g = if push then Push(g) else g in
   Printf.printf "  g = %a\n%!" print_ne_grammar g;
   r.g <- g;
   mkg ~name e g (* for mutual recursion, expand fixpoint once *)

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
    | Ref g -> gn g
    | Push g -> fn g
    | Read(_,g) -> fn g
    | RPos g -> fn g
    | Layout(g,_,ob,na,_,_) -> if ob && not na then fn g else Charset.full
    | Tmp -> assert false
  and gn : type a. a grammar -> Charset.t = fun g ->
    assert g.recursive;
    match g.charset with
    | Some c -> c
    | None ->
       g.charset <- Some Charset.empty;
       let r = fn g.g in
       g.charset <- Some r;
       r
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
  | Ref g -> compile g
  | Push(g) -> cpush (compile_ne g)
  | Read(n,g) -> cread n (compile_ne g)
  | RPos(g) -> crpos (compile_ne g)
  | Layout(g,b,ob,nb,na,oa) -> clayout ~old_before:ob ~new_before:nb
                                       ~new_after:na ~old_after:oa (compile_ne g) b
  | Tmp -> assert false

and compile : type a. a grammar -> a Combinator.t = fun g ->
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
  | Empty x ->
     let e = cempty x in
     if g.g = Fail then e else calt ~cs2:(first_charset g.g) e cg
  | EFail ->
     if g.g = Fail then cfail else cg
