open Utils
open Lex
open Assoc

type layout_config = Comb.layout_config

(** Type for a grammar *)
type 'a grammar =
  { mutable d : 'a grdf   (** the definition of the grammar *)
  ; n : string            (** name of the grammar *)
  ; k : 'a Assoc.key      (** a key used mainly to detect recursion *)
  ; recursive : bool      (** really means declared first and defined after,
                              using declare/set_grammar *)
  ; mutable phase : phase (** which transformation phase reached for that grammar *)
  ; mutable e: 'a option  (** not None if the grammar accepts Empty.
                              keep only one semantics for emptyness, incomplete
                              if action uses [give_up].
                              valid from phase Empty Removed *)
  ; mutable ne : 'a grne  (** the part of the grammar that does not accept empty
                              valid from phase Empty Removed, but transformed
                              at PushFactored and LeftRecEliminated *)
  ; mutable push : bool   (** Does this grammar needs an initial push.
                              valid from phase PushFactored  *)
  ; mutable cache : bool  (** Does this grammar needs a cache
                              valid from phase PushFactored  *)
  ; mutable compiled : 'a Comb.t ref
                          (** the combinator for the grammar. One needs a ref
                              for recursion.
                              valid from phase Compiled *)
  ; mutable charset : Charset.t option
                          (** cache for the first charset. Set only if used
                              by compilation. *)
  }

 (** abreviation! *)
 and 'a t = 'a grammar

 (** The various transformation phase before until compilation to combinators *)
 and phase = Defined | EmptyComputed | EmptyRemoved | PushFactored | LeftRecEliminated | Compiled

 (** Grammar constructors at definition *)
 and 'a grdf =
   | Fail : 'a grdf                        (** grammar that always fais *)
   | Empty : 'a -> 'a grdf                 (** accept only the empty input *)
   | Term : 'a Lex.t -> 'a grdf            (** terminals *)
   | Alt  : 'a t * 'a t -> 'a grdf         (** alternatived *)
   | Appl : 'a t * ('a -> 'b) -> 'b grdf   (** application *)
   | Seq  : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c grdf
                                           (** sequence *)
   | DSeq : 'a t * ('a -> 'b t) * ('b -> 'c) -> 'c grdf
                                           (** dependant sequence *)
   | Lr   : 'a t * ('a -> 'a) t -> 'a grdf
                                           (** Lr(g1,g2) represents g1 g2* and is
                                               used to eliminate left recursion.
                                               It can not be exposed as left recursion
                                               under Lr is not supported *)
   | LPos : (Pos.t -> 'a) t -> 'a grdf     (** read the postion before parsing *)
   | RPos : (Pos.t -> 'a) t -> 'a grdf     (** read the postion after parsing *)
   | Read : int * (Pos.t -> 'a) t -> 'a grdf (** read the position from the stack
                                               not accessible by user, but needed
                                               in [elim_let_rec] *)
   | Layout : blank * 'a t * layout_config -> 'a grdf
                                           (** changes the blank function *)
   | Cache : 'a t -> 'a grdf               (** caches the grammar *)
   | Tmp  : 'a grdf                        (** used as initial value for recursive
                                               grammar. *)

(** type after elimination of empty and for later phase.
    same constructors as above prefixed by E, except EPush.
    The left branch does not go trough the 'a grammar record except for
    recursion.  *)
and 'a grne =
  | EFail : 'a grne
  | ETerm : 'a terminal -> 'a grne
  | EAlt  : 'a grne * 'a grne -> 'a grne
  | EAppl : 'a grne * ('a -> 'b) -> 'b grne
  | ESeq  : 'a grne * 'b grammar * ('a -> 'b -> 'c) -> 'c grne
  | EDSeq : 'a grne * ('a -> 'b grammar) * ('b -> 'c) -> 'c grne
  | ELr   : 'a grne * ('a -> 'a) grammar -> 'a grne
  | ERef  : 'a t -> 'a grne
  | EPush : 'a grne -> 'a grne             (** pushes the position before parsing
                                               to a dedicated stack.
                                               LPos(g) is transformed into
                                               EPush(ERead(g)) *)
  | ERead : int * (Pos.t -> 'a) grne -> 'a grne
  | ERPos : (Pos.t -> 'a) grne -> 'a grne
  | ELayout : blank * 'a grne * layout_config -> 'a grne
  | ECache : 'a grne -> 'a grne
  | ETmp  : 'a grne

(** grammar renaming *)
let give_name n g = { g with n }

(** helper to construct the initial ['a grammar] record *)
let mkg : ?name:string -> ?recursive:bool -> 'a grdf -> 'a grammar =
  fun ?(name="...") ?(recursive=false) d ->
    let k = new_key () in
    { e = None; d; n = name; k; recursive; compiled = ref Comb.fail
    ; charset = None; phase = Defined; ne = ETmp; push = false; cache = false }

(** A type to store list of grammar keys *)
type ety = E : 'a ty -> ety [@@unboxed]

(** printing functions *)
let print_grammar ?(def=true) ch s =
  let adone = ref [] in
  let rec print_grne : type a. out_channel -> a grne -> unit =
  fun ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_grne x in
    let pv x = print_negr x in
    match g with
    | EFail         -> pr "0"
    | ETerm t       -> pr "%s" t.n
    | EAlt(g1,g2)   -> pr "(%a|%a)" pg g1 pg g2
    | EAppl(g,_)    -> pr "App(%a)" pg g
    | ESeq(g1,g2,_) -> pr "%a%a" pg g1 pv g2
    | EDSeq(g1,_,_) -> pr "%a???" pg g1
    | ELr(g,s)      -> pr "%a(%a)*" pg g pv s
    | ERef(g)       -> pr "(%a\\0)" pv g
    | EPush(g)      -> pr "%a" pg g
    | ERead(_,g)    -> pr "%a" pg g
    | ERPos(g)      -> pr "%a" pg g
    | ECache(g)     -> pr "%a" pg g
    | ELayout(_,g,_) -> pr "%a" pg g
    | ETmp          -> pr "TMP"

  and print_grdf : type a. out_channel -> a grdf -> unit =
  fun ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_dfgr x in
    match g with
    | Fail         -> pr "0"
    | Empty _      -> pr "1"
    | Term t       -> pr "%s" t.n
    | Alt(g1,g2)   -> pr "(%a|%a)" pg g1 pg g2
    | Appl(g,_)    -> pr "App(%a)" pg g
    | Seq(g1,g2,_) -> pr "%a%a" pg g1 pg g2
    | DSeq(g1,_,_) -> pr "%a???" pg g1
    | Lr(g,s)      -> pr "%a(%a)*" pg g pg s
    | Read(_,g)    -> pr "%a" pg g
    | RPos(g)      -> pr "%a" pg g
    | LPos(g)      -> pr "%a" pg g
    | Cache(g)     -> pr "%a" pg g
    | Layout(_,g,_)-> pr "%a" pg g
    | Tmp          -> pr "TMP"

  and print_negr : type a. out_channel -> a grammar -> unit =
  fun ch g ->
    let pr x = Printf.fprintf ch x in
    if List.mem (E g.k.k) !adone then Printf.fprintf ch "%s" g.n
    else if g.recursive then
      begin
        adone := E g.k.k :: !adone;
        let pg x = print_grne x in
        match g.e with
        | None -> pr "%s=(%a)" g.n pg g.ne
        | _    -> pr "%s=(1|%a)" g.n pg g.ne
      end
    else
      begin
        let pg x = print_grne x in
        match g.e with
        | None -> pr "%a" pg g.ne
        | _    -> pr "(1|%a)" pg g.ne
      end

  and print_dfgr : type a. out_channel -> a grammar -> unit =
  fun ch g ->
    let pr x = Printf.fprintf ch x in
    if List.mem (E g.k.k) !adone then Printf.fprintf ch "%s" g.n
    else if g.recursive then
      begin
        adone := E g.k.k :: !adone;
        let pg x = print_grdf x in
        pr "%s=(%a)" g.n pg g.d
      end
    else
      begin
        let pg x = print_grdf x in
        pr "%a" pg g.d
      end

  in if def then print_dfgr ch s else print_negr ch s

(** Interface to constructors.
   propagate Fail because it is tested by elim_left_rec for the Lr suffix *)
let fail () = mkg Fail

let empty x = mkg (Empty x)

let test b = if b then empty () else fail ()

let term ?name (x) =
  if accept_empty x then invalid_arg "term: empty terminals";
  let name = match name with None -> x.Lex.n | Some n -> n in
  mkg ~name (Term x)

let alt g f  =
  mkg (if g.d = Fail && f.d = Fail then Fail else Alt(g,f))

let appl ?name g f = mkg ?name (if g.d = Fail then Fail else Appl(g,f))

let seq g1 g2 f = mkg (
  match g1.d,g2.d with
  | Fail, _ -> Fail
  | _, Fail -> Fail
  | Empty x, _ -> Appl(g2, fun y -> f x y)
  | _, Empty y -> Appl(g1, fun x -> f x y)
  | _ -> Seq(g1,g2,f))

let dseq g1 g2 f = mkg (if g1.d = Fail then Fail else DSeq(g1,g2,f))

let seq1 g1 g2 = seq g1 g2 (fun x _ -> x)

let seq2 g1 g2 = seq g1 g2 (fun _ x -> x)

let seqf g1 g2 = seq g1 g2 (fun x f -> f x)

let lr g1 g2 =
  if g2.d = Fail then g1
  else mkg (if g1.d = Fail then Fail else Lr(g1,g2))

let lpos g = mkg (if g.d = Fail then Fail else LPos(g))

let rpos g = mkg (if g.d = Fail then Fail else RPos(g))

let seqf_rpos g1 g2 =
  seq (rpos (appl g1 (fun x rpos -> (x, rpos)))) g2 (fun (x,rpos) f -> f x rpos)

let seq2_rpos g1 g2 =
  seq (rpos (appl g1 (fun _ rpos -> rpos))) g2 (fun rpos f -> f rpos)

let seqf_lpos g1 g2 =
  seq (lpos (appl g1 (fun x lpos -> (lpos, x)))) g2 (fun (lpos,x) f -> f lpos x)

let seq2_lpos g1 g2 =
  seq (lpos (appl g1 (fun _ lpos -> lpos))) g2 (fun lpos f -> f lpos)

let seqf_pos g1 g2 =
  seq (lpos (rpos (appl g1 (fun x rpos lpos -> (lpos, x, rpos))))) g2 (fun (lpos,x,rpos) f -> f lpos x rpos)

let seq2_pos g1 g2 =
  seq (lpos (rpos (appl g1 (fun _ rpos lpos -> (lpos, rpos))))) g2 (fun (lpos,rpos) f -> f lpos rpos)

let read n g = mkg (if g.d = Fail then Fail else Read(n,g))

let cache g = mkg (if g.d = Fail then Fail else Cache(g))

let layout ?(config=Comb.default_layout_config) b g =
  mkg (if g.d = Fail then Fail else Layout(b,g,config))

(** function to define mutually recursive grammar:
    - first one declares the grammars
    - second one set the grammars *)
let declare_grammar name =
  mkg ~name ~recursive:true Tmp

let set_grammar : type a. a grammar -> a grammar -> unit =
  fun g1 g2 ->
    if g1.d <> Tmp then failwith "set_grammar: grammar already set or not created with set_grammar";
    g1.d <- g2.d

let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?(name="...") g ->
    let g0 = declare_grammar name in
    set_grammar g0 (g g0);
    g0

(** a function to defined indexed grammars *)
let grammar_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = Utils.EqHashtbl.create 8 in
  let is_set = ref None in
  (fun p ->
    try EqHashtbl.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p) in
      EqHashtbl.add tbl p g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f p);
      );
      g),
  (fun f ->
    is_set := Some f;
    EqHashtbl.iter (fun p r ->
      set_grammar r (f p);
    ) tbl)

(** helpers for the constructors of 'a grne *)
let ne_alt g1 g2 = match (g1,g2) with
  | EFail, g2 -> g2
  | g1, EFail -> g1
  | (g1,g2)   -> EAlt(g1,g2)

let ne_appl g f =
  match g with
  | EFail          -> EFail
  | EAppl(g,h)     -> EAppl(g,fun x -> f (h x))
  | ESeq(g1,g2,h)  -> ESeq(g1,g2,fun x y -> f (h x y))
  | EDSeq(g1,g2,h) -> EDSeq(g1,g2,fun y -> f (h y))
  | _              -> EAppl(g,f)

let ne_seq g1 g2 f = match(g1,g2) with
  | EFail, _                 -> EFail
  | _, {e=None;ne=EFail;_}   -> EFail
  | _, {e=Some y;ne=EFail;_} -> ne_appl g1 (fun x -> f x y)
  | _, _                     -> ESeq(g1,g2,f)

let ne_dseq g1 g2 f = match g1 with
  | EFail  -> EFail
  | _     -> EDSeq(g1,g2,f)

let ne_lr g s =
  match (g,s) with
  | EFail, _ -> EFail
  | _, {ne=EFail;_} -> g
  | _        -> ELr(g,s)

let ne_lpos g1 = match g1 with
  | EFail -> EFail
  | _     -> EPush(ERead(0,g1))

let ne_rpos g1 = match g1 with
  | EFail -> EFail
  | _     -> ERPos(g1)

let ne_read n g1 = match g1 with
  | EFail -> EFail
  | _     -> ERead(n,g1)

let ne_cache g1 = match g1 with
  | EFail -> EFail
  | _     -> ECache(g1)

let ne_layout b g cfg =
  match g with
  | EFail -> EFail
  | _     -> ELayout(b,g,cfg)

(** first phase of transformation:
    - get the result of an empty parse if it exists
    - and returns a grammar with the empty parses are removed
    - store this in the corresponding fields of g *)
let factor_empty g =
  let get g = if g.recursive then ERef g else (assert (g.ne <> ETmp); g.ne )in

  let rec fn : type a. a grammar -> unit = fun g ->
    if g.phase = Defined then
      begin
        g.phase <- EmptyComputed;
        g.e  <- kn g.d;
      end

  and kn : type a. a grdf -> a option = function
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
                               | Some x -> Some (x Pos.phantom)) (* Loose position *)
    | RPos(g1)      -> fn g1; (match g1.e with
                               | None -> None
                               | Some x -> Some (x Pos.phantom)) (* Loose position *)
    | Read(_,g1)    -> fn g1; (match g1.e with
                               | None -> None
                               | Some x -> Some (x Pos.phantom)) (* Loose position *)
    | Layout(_,g,_) -> fn g; g.e
    | Cache(g)      -> fn g; g.e
    | Tmp           -> failwith "grammar compiled before full definition"
  in
  let rec hn : type a. a grammar -> unit = fun g ->
    if g.phase = EmptyComputed then
      begin
        g.phase <- EmptyRemoved;
        g.ne  <- gn g.d;
      end

  and gn : type a. a grdf -> a grne = function
    | Fail -> EFail
    | Empty _ -> EFail
    | Term(x) -> ETerm(x)
    | Alt(g1,g2) -> hn g1; hn g2; ne_alt (get g1) (get g2)
    | Appl(g,f) -> hn g; ne_appl (get g) f
    | Seq(g1,g2,f) -> hn g1; hn g2; let ga = ne_seq (get g1) g2 f in
                      let gb = match g1.e with
                        | None   -> EFail
                        | Some x -> ne_appl (get g2) (fun y -> f x y)
                      in
                      ne_alt ga gb
    | DSeq(g1,g2,f) -> hn g1; let ga = ne_dseq (get g1) g2 f in
                       let gb = match g1.e with
                         | None   -> EFail
                         | Some x -> let g2 = g2 x in fn g2; hn g2; ne_appl (get g2) f (* FIXME *)
                       in
                       ne_alt ga gb
    | Lr(g1,g2) -> hn g1; hn g2; ne_lr (get g1) g2
    | LPos(g1) -> hn g1; ne_lpos (get g1)
    | RPos(g1) -> hn g1; ne_rpos (get g1)
    | Read(n,g1) -> hn g1; ne_read n (get g1)
    | Cache(g1) -> hn g1; ne_cache(get g1)
    | Layout(b,g,cfg) -> hn g; ne_layout b (get g) cfg
    | Tmp           -> failwith "grammar compiled before full definition"

  in fn g; hn g

(** remove a lpos left prefix and also a cache and move it up
    to the grammar definition.
    FIXME: useless if the prefix is not followed by Tmp.
    This is necessary before elimination of left recursion *)
let remove_push : type a. a grammar -> unit =
  let rec fn : type a. a grne -> bool * bool * a grne = fun g ->
    let push = ref false in
    let cache = ref false in
    let rec fn : type a. bool -> a grne -> a grne =
      fun r -> function
        | EAlt(g1,g2) -> ne_alt (fn r g1) (fn r g2)
        | EAppl(g1,f) -> ne_appl (fn r g1) f
        | ESeq(g1,g2,f) -> ne_seq (fn r g1) g2 f
        | EDSeq(g1,g2,f) -> ne_dseq (fn r g1) g2 f
        | EPush(g1) -> push := true; fn true g1
        | ERead(n,g1) -> ne_read (if r then n else n+1) (fn r g1)
        | ERPos(g1) -> ne_rpos(fn r g1)
        | ELr(_,_) -> assert false
        | ECache(g1) -> cache := true; fn r g1
        | ELayout(b,g1,cfg) -> ne_layout b (fn r g1) cfg
        | ERef g0 as g -> gn g0; g
        | ETmp -> assert false
        | EFail | ETerm _ as g -> g
    in
    let g = fn false g in
    (!push, !cache, g)

  and gn : type a.a grammar -> unit = fun g ->
    assert(g.phase >= EmptyRemoved);
    if g.phase = EmptyRemoved then
      begin
        g.phase <- PushFactored;
        assert(g.ne <> ETmp);
        let (push, cache, g1) = fn g.ne in
        g.ne <- g1;
        g.push <- push;
        g.cache <- cache;
      end
  in gn

(** Elimination of left recursion which is not supported by combinators *)
let rec elim_left_rec : type a. ety list -> a grammar -> unit = fun above g ->

  let u = g.k.k in

  let rec fn : type b. b grne -> b grne * (a -> b) grammar =
    fun g ->
      match g with
      | EFail -> (EFail, fail ())
      | ETerm _ -> (g, fail ())
      | ESeq(g1,g2,f) ->
         let (g1, s1) = fn g1 in
         (ne_seq g1 g2 f,
          seq s1 g2 (fun fx y a -> f (fx a) y))
      | EDSeq(g1,_,_) ->
         let (_, s1) = fn g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under dependant sequence";
         (g, fail())
      | EAlt(g1,g2) ->
         let (g1, s1) = fn g1 in
         let (g2, s2) = fn g2 in
         (ne_alt g1 g2, alt s1 s2)
      | ELr(g1,s)   ->
         let (g1,s1) = fn g1 in
         (ne_lr g1 s, lr s1 (appl s (fun f g a -> f (g a))))
      | EAppl(g1,f) ->
         let (g1,s) = fn g1 in
         (ne_appl g1 f, appl s (fun g a -> f (g a)))
      | ERef(g) -> gn g
      | EPush(g1) as g ->
         assert ((snd (fn g1)).d = Fail);
         (g, fail())
      | ECache(g1) as g ->
         assert ((snd (fn g1)).d = Fail);
         (g, fail())
      | ERead(n,g1) ->
         let (g1, s1) = fn g1 in
         (ne_read n g1,read n (appl s1 (fun f pos a -> f a pos)))
      | ERPos(g1) ->
         let (g1, s1) = fn g1 in
         (ne_rpos g1,rpos(appl s1 (fun f pos a -> f a pos)))
      | ELayout(_,g1,_) ->
         let (_, s1) = fn g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under layout change";
         (g, fail())
      | ETmp -> assert false

   and gn : type b. b grammar -> b grne * (a -> b) grammar =
     fun g ->
       match g.k.eq u with
       | Eq  ->
          (EFail, empty(fun x -> x));
       | NEq ->
          if List.mem (E g.k.k) above then
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
       if s.ne <> EFail then g.ne <- ne_lr g1 s;
     end

(** compute the characters set accepted at the beginning of the input *)
let first_charset : type a. a grne -> Charset.t = fun g ->
  let rec fn : type a. a grne -> Charset.t = fun g ->
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
    | ECache g -> fn g
    | ELayout(_,g,cfg) -> if cfg.old_blanks_before && not cfg.old_blanks_after then fn g else Charset.full
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

(** compilation of a grammar to combinators *)
let rec compile_ne : type a. a grne -> a Comb.t = fun g ->
  match g with
  | EFail -> Comb.fail
  | ETerm(c) -> Comb.lexeme c.f
  | EAlt(g1,g2) -> Comb.alt (first_charset g1) (compile_ne g1)
                            (first_charset g2) (compile_ne g2)
  | ESeq(g1,g2,f) -> Comb.seq (compile_ne g1) (compile g2) f
  | EDSeq(g1,g2,f) -> Comb.dep_seq (compile_ne g1) (fun x -> compile (g2 x)) f
  | EAppl(g1,f) -> Comb.app (compile_ne g1) f
  | ELr(g,s) -> Comb.lr (compile_ne g) (first_charset s.ne) (compile_ne s.ne)
  | ERef g -> compile g
  | EPush(g) -> Comb.push (compile_ne g)
  | ERead(n,g) -> Comb.read n (compile_ne g)
  | ERPos(g) -> Comb.right_pos (compile_ne g)
  | ECache(g) -> Comb.cache (compile_ne g)
  | ELayout(b,g,cfg) -> Comb.change_layout ~config:cfg b (compile_ne g)
  | ETmp -> assert false

 and compile : type a. a grammar -> a Comb.t = fun g ->
  factor_empty g;
  remove_push g;
  elim_left_rec [] g;
  assert (g.phase >= LeftRecEliminated);
  let get g = if g.recursive then Comb.deref g.compiled else !(g.compiled) in
  let cg =
    if g.phase = Compiled then get g
    else
      begin
        g.phase <- Compiled;
        g.compiled := compile_ne g.ne;
        get g
      end
  in
  let cg = if g.push then Comb.push cg else cg in
  let cg = if g.cache then Comb.cache cg else cg in
  match g.e with
  | Some x ->
     let e = Comb.empty x in
     if g.ne = EFail then e else Comb.alt Charset.full e (first_charset g.ne) cg
  | None ->
     if g.ne = EFail then Comb.fail else cg

(** get some infos *)
let grammar_info g =
  ignore (compile g);
  let cs = first_charset g.ne in
  (g.e <> None, cs)

let grammar_name g = g.n
