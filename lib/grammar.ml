open Combinator
open Lex
open Assoc

(* types for a grammar *)

(* Type at definition *)
type 'a def_grammar =
  | Fail : 'a def_grammar
  | Empty : 'a -> 'a def_grammar
  | Term : 'a terminal -> 'a def_grammar
  | Alt  : 'a grammar * 'a grammar -> 'a def_grammar
  | Appl : 'a grammar * ('a -> 'b) -> 'b def_grammar
  | Seq  : 'a grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c def_grammar
  | DSeq : 'a grammar * ('a -> 'b grammar) * ('b -> 'c) -> 'c def_grammar
  | Lr   : 'a grammar * ('a -> 'a) grammar -> 'a def_grammar
  | LPos : (pos -> 'a) grammar -> 'a def_grammar
  | RPos : (pos -> 'a) grammar -> 'a def_grammar
  | Read : int * (pos -> 'a) grammar -> 'a def_grammar
  | Layout : 'a grammar * blank * bool * bool * bool * bool -> 'a def_grammar
  | Tmp  : 'a def_grammar

 (* type after elimination of empty and for later phase *)
and 'a ne_grammar =
  | EFail : 'a ne_grammar
  | ETerm : 'a terminal -> 'a ne_grammar
  | EAlt  : 'a ne_grammar * 'a ne_grammar -> 'a ne_grammar
  | EAppl : 'a ne_grammar * ('a -> 'b) -> 'b ne_grammar
  | ESeq  : 'a ne_grammar * 'b grammar * ('a -> 'b -> 'c) -> 'c ne_grammar
  | EDSeq : 'a ne_grammar * ('a -> 'b grammar) * ('b -> 'c) -> 'c ne_grammar
  | ELr   : 'a ne_grammar * ('a -> 'a) grammar -> 'a ne_grammar
  | ERef  : 'a grammar -> 'a ne_grammar
  | EPush : 'a ne_grammar -> 'a ne_grammar
  | ERead : int * (pos -> 'a) ne_grammar -> 'a ne_grammar
  | ERPos : (pos -> 'a) ne_grammar -> 'a ne_grammar
  | ELayout : 'a ne_grammar * blank * bool * bool * bool * bool -> 'a ne_grammar
  | ETmp  : 'a ne_grammar

 and 'a grammar = { mutable d : 'a def_grammar
                  ; n : string; u : 'a ty; eq : 'b. 'b ty -> ('a,'b) eq
                  ; recursive : bool  (* really means declared first and defined after *)
                  ; mutable phase : phase
                  ; mutable e: 'a option        (* valid from phase Empty Removed *)
                  ; mutable ne : 'a ne_grammar  (* valid from phase Empty Removed *)
                  ; mutable push : bool         (* valid from pahse PushFactored  *)
                  ; mutable compiled : 'a Combinator.t ref (* valid from phase Compiled *)
                  ; mutable charset : Charset.t option     (* valid from phase Compiled *)
                  }

 and phase = Defined | EmptyRemoved | PushFactored | LeftRecEliminated | Compiled

type 'a t = 'a grammar

let mkg : ?name:string -> ?recursive:bool -> 'a def_grammar -> 'a grammar =
  fun ?(name="...") ?(recursive=false) d ->
    let k = new_key () in
    { e = None; d; n = name; u = k.k; eq = k.eq; recursive; compiled = ref cfail
    ; charset = None; phase = Defined; ne = ETmp; push = false }

type ety = E : 'a ty -> ety [@@unboxed]

let rec print_ne_grammar
    : type a. ety list -> out_channel -> a ne_grammar -> unit =
  fun adone ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_ne_grammar adone x in
    let pv x = print_grammar_ne adone x in
    match g with
    | EFail -> pr "0"
    | ETerm t -> pr "%s" t.n
    | EAlt(g1,g2) -> pr "(%a|%a)" pg g1 pg g2
    | EAppl(g,_) -> pr "App(%a)" pg g
    | ESeq(g1,g2,_) -> pr "%a%a" pg g1 pv g2
    | EDSeq(g1,_,_) -> pr "%a???" pg g1
    | ELr(g,s) -> pr "%a(%a)*" pg g pv s
    | ERef(g)  -> pr "(%a\\0)" pv g
    | EPush(g) -> pr "%a" pg g
    | ERead(_,g) -> pr "%a" pg g
    | ERPos(g) -> pr "%a" pg g
    | ELayout(g,_,_,_,_,_) -> pr "%a" pg g
    | ETmp     -> pr "TMP"

and print_def_grammar
    : type a. ety list -> out_channel -> a def_grammar -> unit =
  fun adone ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_grammar_def adone x in
    match g with
    | Fail -> pr "0"
    | Empty _ -> pr "1"
    | Term t -> pr "%s" t.n
    | Alt(g1,g2) -> pr "(%a|%a)" pg g1 pg g2
    | Appl(g,_) -> pr "App(%a)" pg g
    | Seq(g1,g2,_) -> pr "%a%a" pg g1 pg g2
    | DSeq(g1,_,_) -> pr "%a???" pg g1
    | Lr(g,s) -> pr "%a(%a)*" pg g pg s
    | Read(_,g) -> pr "%a" pg g
    | RPos(g) -> pr "%a" pg g
    | LPos(g) -> pr "%a" pg g
    | Layout(g,_,_,_,_,_) -> pr "%a" pg g
    | Tmp     -> pr "TMP"

and print_grammar_ne
    : type a. ety list -> out_channel -> a grammar -> unit = fun adone ch g ->
  let pr x = Printf.fprintf ch x in
  if List.mem (E g.u) adone then Printf.fprintf ch "%s" g.n
  else if g.recursive then
    begin
      let adone = E g.u :: adone in
      let pg x = print_ne_grammar adone x in
      match g.e with
      | None -> pr "%s=(%a)" g.n pg g.ne
      | _    -> pr "%s=(1|%a)" g.n pg g.ne
    end
  else
    begin
      let pg x = print_ne_grammar adone x in
      match g.e with
      | None -> pr "%a" pg g.ne
      | _     -> pr "(1|%a)" pg g.ne
    end

and print_grammar_def
    : type a. ety list -> out_channel -> a grammar -> unit = fun adone ch g ->
  let pr x = Printf.fprintf ch x in
  if List.mem (E g.u) adone then Printf.fprintf ch "%s" g.n
  else if g.recursive then
    begin
      let adone = E g.u :: adone in
      let pg x = print_def_grammar adone x in
      pr "%s=(%a)" g.n pg g.d
    end
  else
    begin
      let pg x = print_def_grammar adone x in
      pr "%a" pg g.d
    end

(*let print_ne_grammar ch s = print_ne_grammar [] ch s*)
let print_grammar ?(def=true) ch s =
  if def then
    print_grammar_def [] ch s
  else
    print_grammar_ne [] ch s

(* interface to constructors.
   propagate Fail because it is tested by elim_left_rec for the Lr suffix *)
let fail () = mkg Fail

let empty(x) = mkg (Empty x)

let term(x) =
  if accept_empty x then invalid_arg "term: empty terminals";
  mkg ~name:x.Lex.n (Term x)

let alt(g,f) = mkg (if g.d = Fail && f.d = Fail then Fail else Alt(g,f))

let appl(g,f) = mkg (if g.d = Fail then Fail else Appl(g,f))

let seq(g1,g2,f) = mkg (if g1.d = Fail || g2.d = Fail then Fail else Seq(g1,g2,f))

let dseq(g1,g2,f) = mkg (if g1.d = Fail then Fail else DSeq(g1,g2,f))

let lr(g1,g2) =
  if g2.d = Fail then g1
  else mkg (if g1.d = Fail then Fail else Lr(g1,g2))

let lpos(g) = mkg (if g.d = Fail then Fail else LPos(g))

let rpos(g) = mkg (if g.d = Fail then Fail else RPos(g))

let read(n,g) = mkg (if g.d = Fail then Fail else Read(n,g))

let layout ?(old_before=true) ?(new_before=false)
           ?(new_after=false) ?(old_after=true) (g,b) =
  mkg (if g.d = Fail then Fail else Layout(g,b,old_before,new_before,new_after,old_after))

let declare_grammar ?(name="") () =
  mkg ~name ~recursive:true Tmp

let set_grammar : type a. a grammar -> a grammar -> unit =
  fun g1 g2 ->
    if g1.d <> Tmp then failwith "set_grammar: grammar already set or not created with set_grammar";
    g1.d <- g2.d

let ne_alt = function
  | EFail, g2 -> g2
  | g1, EFail -> g1
  | (g1,g2)   -> EAlt(g1,g2)

let ne_appl(g,f) =
  match g with
  | EFail          -> EFail
  | EAppl(g,h)     -> EAppl(g,fun x -> f (h x))
  | ESeq(g1,g2,h)  -> ESeq(g1,g2,fun x y -> f (h x y))
  | EDSeq(g1,g2,h) -> EDSeq(g1,g2,fun y -> f (h y))
  | _              -> EAppl(g,f)

let ne_seq(g1,g2,f) = match(g1,g2) with
  | EFail, _                 -> EFail
  | _, {e=None;ne=EFail;_}   -> EFail
  | _, {e=Some y;ne=EFail;_} -> ne_appl(g1,fun x -> f x y)
  | _, _                     -> ESeq(g1,g2,f)

let ne_dseq(g1,g2,f) = match g1 with
  | EFail  -> EFail
  | _     -> EDSeq(g1,g2,f)

let ne_lr(g,s) =
  match (g,s) with
  | EFail, _ -> EFail
  | _, {ne=EFail;_} -> g
  | _        -> ELr(g,s)

let ne_lpos(g1) = match g1 with
  | EFail -> EFail
  | _     -> EPush(ERead(0,g1))

let ne_rpos(g1) = match g1 with
  | EFail -> EFail
  | _     -> ERPos(g1)

let ne_read(n,g1) = match g1 with
  | EFail -> EFail
  | _     -> ERead(n,g1)

let ne_layout(g,b,ob,nb,na,oa) =
  match g with
  | EFail -> EFail
  | _ -> ELayout(g,b,ob,nb,na,oa)

let factor_empty g =

  let get g = if g.recursive then ERef g else (assert (g.ne <> ETmp); g.ne )in

  let rec fn : type a. a grammar -> unit = fun g ->
    if g.phase = Defined then
      begin
        g.phase <- EmptyRemoved;
        g.e  <- kn g.d;
        g.ne <- gn g.d;
      end

  and gn : type a. a def_grammar -> a ne_grammar = function
    | Fail -> EFail
    | Empty _ -> EFail
    | Term(x) -> ETerm(x)
    | Alt(g1,g2) -> ne_alt(get g1,get g2)
    | Appl(g,f) -> ne_appl(get g,f)
    | Seq(g1,g2,f) -> let ga = ne_seq(get g1, g2,f) in
                      let gb = match g1.e with
                        | None   -> EFail
                        | Some x -> ne_appl(get g2,fun y -> f x y)
                      in
                      ne_alt(ga,gb)
    | DSeq(g1,g2,f) -> let ga = ne_dseq(get g1, g2,f) in
                       let gb = match g1.e with
                         | None   -> EFail
                         | Some x -> let g2 = g2 x in fn g2; ne_appl(get g2,f)
                       in
                       ne_alt(ga,gb)
    | Lr(g1,g2) -> ne_lr(get g1,g2)
    | LPos(g1) -> ne_lpos(get g1)
    | RPos(g1) -> ne_rpos(get g1)
    | Read(n,g1) -> ne_read(n, get g1)
    | Layout(g,b,ob,nb,na,oa) -> ne_layout(get g,b,ob,nb,na,oa)
    | Tmp           -> failwith "grammar compiled before full definition"

  and kn : type a. a def_grammar -> a option = function
    | Fail -> None
    | Empty x -> Some x
    | Term _     -> None
    | Alt(g1,g2) -> fn g1; fn g2; (match g1.e with Some _ -> g1.e | None -> g2.e)
    | Appl(g,f) -> fn g; (match g.e with
                    | None -> None
                    | Some x -> try Some (f x) with NoParse -> None)
    | Seq(g1,g2,f) -> fn g1; fn g2; (match g1.e with
                       | None -> None
                       | Some x -> match g2.e with
                                   | None -> None
                                   | Some y -> try Some(f x y) with NoParse -> None)
    | DSeq(g1,g2,f) -> fn g1; (match g1.e with
                       | None -> None
                       | Some x -> try
                                   let g2 = g2 x in fn g2;
                                                    match g2.e with
                                                    | None -> None
                                                    | Some y -> Some(f y)
                                 with NoParse -> None)
    | Lr(g1,g2) -> fn g1; fn g2; g1.e
    | LPos(g1)      -> fn g1; (match g1.e with
                               | None -> None
                               | Some x -> Some (x phantom)) (* Loose position *)
    | RPos(g1)      -> fn g1; (match g1.e with
                               | None -> None
                               | Some x -> Some (x phantom)) (* Loose position *)
    | Read(_,g1)    -> fn g1; (match g1.e with
                               | None -> None
                               | Some x -> Some (x phantom)) (* Loose position *)
    | Layout(g,_,_,_,_,_) -> fn g; g.e
    | Tmp           -> failwith "grammar compiled before full definition"
  in fn g

(* remove a lpos left prefix.
   FIXME: useless if the prefix is not followed by Tmp *)
let remove_push : type a. a grammar -> unit =
  let rec fn : type a. a ne_grammar -> bool * a ne_grammar = fun g ->
    let found = ref false in
    let rec fn : type a. bool -> a ne_grammar -> a ne_grammar =
      fun r -> function
        | EAlt(g1,g2) -> ne_alt(fn r g1, fn r g2)
        | EAppl(g1,f) -> ne_appl(fn r g1,f)
        | ESeq(g1,g2,f) -> ne_seq(fn r g1,g2,f)
        | EDSeq(g1,g2,f) -> ne_dseq(fn r g1,g2,f)
        | EPush(g1) -> fn true g1
        | ERead(n,g1) -> ERead((if r then n else n+1), fn r g1)
        | ERPos(g1) -> ERPos(fn r g1)
        | ELr(_,_) -> assert false
        | ELayout(g1,b,ob,nb,na,oa) -> ne_layout(fn r g1, b,ob,nb,na,oa)
        | ERef g0 as g -> gn g0; if r || g0.push then found := true; g
        | ETmp -> assert false
        | EFail | ETerm _ as g -> g
    in
    let g = fn false g in
    (!found, g)

  and gn : type a.a grammar -> unit = fun g ->
    assert(g.phase >= EmptyRemoved);
    if g.phase = EmptyRemoved then
      begin
        g.phase <- PushFactored;
        assert(g.ne <> ETmp);
        let (push, g1) = fn g.ne in
        g.ne <- g1;
        g.push <- push;
      end
  in gn

(* construction of recursive grammar *)

let ne_read(n,g) =
  match g with
  | EFail -> EFail
  | _ -> ERead(n,g)

let rec elim_left_rec : type a. ety list -> a grammar -> unit = fun above g ->

  let u = g.u in

  let rec fn : type b. b ne_grammar -> b ne_grammar * (a -> b) grammar =
    fun g ->
      match g with
      | EFail -> (EFail, fail ())
      | ETerm _ -> (g, fail ())
      | ESeq(g1,g2,f) ->
         let (g1, s1) = fn g1 in
         (ne_seq(g1,g2,f),
          seq(s1,g2,fun fx y a -> f (fx a) y))
      | EDSeq(g1,_,_) ->
         let (_, s1) = fn g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under dependant sequence";
         (g, fail())
      | EAlt(g1,g2) ->
         let (g1, s1) = fn g1 in
         let (g2, s2) = fn g2 in
         (ne_alt(g1,g2), alt(s1,s2))
      | ELr(g1,s)   ->
         let (g1,s1) = fn g1 in
         (ne_lr(g1,s),lr(s1,appl(s,fun f g a -> f (g a))))
      | EAppl(g1,f) ->
         let (g1,s) = fn g1 in
         (ne_appl(g1,f), appl(s,fun g a -> f (g a)))
      | ERef(g) -> gn g
      | EPush(g1) as g ->
         assert ((snd (fn g1)).d = Fail);
         (g, fail())
      | ERead(n,g1) ->
         let (g1, s1) = fn g1 in
         (ne_read(n,g1),read(n,appl(s1,fun f pos a -> f a pos)))
      | ERPos(g1) ->
         let (g1, s1) = fn g1 in
         (ne_rpos(g1),rpos(appl(s1,fun f pos a -> f a pos)))
      | ELayout(g1,_,_,_,_,_) ->
         let (_, s1) = fn g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under layout change";
         (g, fail())
      | ETmp -> assert false

   and gn : type b. b grammar -> b ne_grammar * (a -> b) grammar =
     fun g ->
       match g.eq u with
       | Eq  ->
          (EFail, empty(fun x -> x));
       | NEq ->
          if List.mem (E g.u) above then
            begin
              (ERef g, fail ())
            end
          else
            begin
              elim_left_rec (E u :: above) g;
              let (g',s) = fn g.ne in
              factor_empty s;
              s.e <- None;
              s.phase <- LeftRecEliminated;
              if s.ne = EFail then (ERef g, fail()) else (g', s)

            end
   in
   assert (g.phase>=PushFactored);
   if g.phase = PushFactored then
     begin
       g.phase <- LeftRecEliminated;
       let (g1,s) = fn g.ne in
       factor_empty s;
       s.e <- None;
       s.phase <- LeftRecEliminated;
       if s.ne <> EFail then
         begin
           let g1 =
             match g.e with
             | None -> ne_lr(g1,s)
             | Some x -> ne_lr(ne_alt(g1,ne_appl(s.ne,fun f -> f x)), s)
           in
           g.ne <- g1
         end;
     end



let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?(name="...") g ->

    let g0 = declare_grammar ~name () in
    set_grammar g0 (g g0);
    g0

let first_charset : type a. a ne_grammar -> Charset.t = fun g ->
  let rec fn : type a. a ne_grammar -> Charset.t = fun g ->
    match g with
    | EFail -> Charset.empty
    | ETerm(c) -> c.c
    | EAlt(g1,g2) -> Charset.union (fn g1) (fn g2)
    | ESeq(g,_,_) -> fn g
    | EDSeq(g,_,_) -> fn g
    | EAppl(g,_) -> fn g
    | ELr(g,_) -> fn g
    | ERef g -> gn g
    | EPush g -> fn g
    | ERead(_,g) -> fn g
    | ERPos g -> fn g
    | ELayout(g,_,ob,na,_,_) -> if ob && not na then fn g else Charset.full
    | ETmp -> assert false

  and gn : type a. a grammar -> Charset.t = fun g ->
    assert g.recursive;
    match g.charset with
    | Some c -> c
    | None ->
       g.charset <- Some Charset.empty;
       let r = fn g.ne in
       g.charset <- Some r;
       r
  in fn g

(* compilation of a grammar to combinator *)
let rec compile_ne : type a. a ne_grammar -> a Combinator.t = fun g ->
  match g with
  | EFail -> cfail
  | ETerm(c) -> cterm c.f
  | EAlt(g1,g2) -> calt ~cs1:(first_charset g1) ~cs2:(first_charset g2)
                       (compile_ne g1) (compile_ne g2)
  | ESeq(g1,g2,f) -> cseq (compile_ne g1) (compile g2) f
  | EDSeq(g1,g2,f) -> cdep_seq (compile_ne g1) (fun x -> compile (g2 x)) f
  | EAppl(g1,f) -> capp (compile_ne g1) f
  | ELr(g,s) -> clr ~cs2:(first_charset s.ne) (compile_ne g) (compile_ne s.ne)
  | ERef g -> compile g
  | EPush(g) -> cpush (compile_ne g)
  | ERead(n,g) -> cread n (compile_ne g)
  | ERPos(g) -> crpos (compile_ne g)
  | ELayout(g,b,ob,nb,na,oa) -> clayout ~old_before:ob ~new_before:nb
                                       ~new_after:na ~old_after:oa (compile_ne g) b
  | ETmp -> assert false

 and compile : type a. a grammar -> a Combinator.t = fun g ->
  factor_empty g;
  remove_push g;
  elim_left_rec [] g;
  assert (g.phase >= LeftRecEliminated);
  let get g = if g.recursive then cref g.compiled else !(g.compiled) in
  let cg =
    if g.phase = Compiled then get g
    else
      begin
        g.phase <- Compiled;
        g.compiled := compile_ne g.ne;
        get g
      end
  in
  let cg = if g.push then cpush cg else cg in
  match g.e with
  | Some x ->
     let e = cempty x in
     if g.ne = EFail then e else calt ~cs2:(first_charset g.ne) e cg
  | None ->
     if g.ne = EFail then cfail else cg
