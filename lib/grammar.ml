open Lex

type layout_config = Comb.layout_config

(** Type for a grammar *)
type 'a grammar =
  { mutable d : 'a grdf   (** the definition of the grammar *)
  ; n : string            (** name of the grammar *)
  ; k : 'a Comb.key       (** a key used mainly to detect recursion *)
  ; recursive : bool      (** really means declared first and defined after,
                              using declare/set_grammar *)
  ; mutable phase : phase (** which transformation phase reached for that
                              grammar *)
  ; mutable e: 'a option  (** not None  if the grammar accepts Empty.  keep only
                              one semantics for emptyness, incomplete if action
                              uses [give_up].  valid from phase Empty Removed *)
  ; mutable ne : 'a grne  (** the part of the grammar that does not accept empty
                              valid from phase Empty Removed, but transformed
                              at CacheFactored and LeftRecEliminated *)
  ; mutable cache : bool  (** Does this grammar needs a cache valid from phase
                              CacheFactored *)
  ; mutable compiled : 'a Comb.t ref
  (** the combinator for the grammar. One needs a ref for recursion.  valid from
                              phase Compiled *)
  ; mutable charset : Charset.t option
 (** cache for the first charset. Set only if used by compilation. *)
  }

 (** abreviation! *)
 and 'a t = 'a grammar

 (** The various transformation phase before until compilation to combinators *)
 and phase = Defined | EmptyComputed | EmptyRemoved | CacheFactored
             | LeftRecEliminated | Compiling | Compiled

 (** Grammar constructors at definition *)
 and 'a grdf =
   | Fail : 'a grdf                        (** grammar that always fais *)
   | Empty : 'a -> 'a grdf                 (** accept only the empty input *)
   | Term : 'a Lex.t -> 'a grdf            (** terminals *)
   | Alt  : 'a t list -> 'a grdf           (** alternatives *)
   | Appl : 'a t * ('a -> 'b) -> 'b grdf   (** application *)
   | Seq  : 'a t * ('a -> 'b) t -> 'b grdf
   (** sequence *)
   | DSeq : ('a * 'b) t * Charset.t * ('a -> ('b -> 'c) t) -> 'c grdf
   (** dependant sequence *)
   | Lr   : 'a t * 'a Comb.key * Pos.t Assoc.key option * 'a t -> 'a grdf
   (** Lr(g1,g2) represents g1 g2* and is used to eliminate left recursion.  It
                                               can not be exposed as left
                                               recursion under Lr is not
                                               supported *)
   | Rkey : 'a Comb.key -> 'a grdf
   | LPos : Pos.t Assoc.key option * (Pos.t -> 'a) t -> 'a grdf
   (** read the postion before parsing*)
   | RPos : (Pos.t -> 'a) t -> 'a grdf     (** read the postion after parsing *)
   | Layout : blank * 'a t * layout_config -> 'a grdf
   (** changes the blank function *)
   | Cache : 'a t -> 'a grdf               (** caches the grammar *)
   | Tmp  : 'a grdf                        (** used as initial value for
                                               recursive grammar. *)

 (** type after elimination of empty  and for later phase.  same constructors as
     above prefixed by E, except EPush.  The left branch does  not go trough the
    'a grammar record except for recursion.  *)
 and 'a grne =
   | EFail : 'a grne
   | ETerm : 'a terminal -> 'a grne
   | EAlt  : 'a grne list -> 'a grne
   | EAppl : 'a grne * ('a -> 'b) -> 'b grne
   | ESeq  : 'a grne * ('a -> 'b) t -> 'b grne
   | EDSeq : ('a * 'b) grne * Charset.t * ('a -> ('b -> 'c) t) -> 'c grne
   | ELr   : 'a grne * 'a Comb.key * Pos.t Assoc.key option * 'a grammar
               -> 'a grne
   | ERkey : 'a Comb.key -> 'a grne
   | ERef  : 'a t -> 'a grne
   | ELPos : Pos.t Assoc.key option * (Pos.t -> 'a) grne -> 'a grne
   | ERPos : (Pos.t -> 'a) grne -> 'a grne
   | ELayout : blank * 'a grne * layout_config -> 'a grne
   | ECache : 'a grne -> 'a grne
   | ETmp  : 'a grne

(** grammar renaming *)
let give_name n g = { g with n }

(** helper to construct the initial ['a grammar] record *)
let mkg : ?name:string -> ?recursive:bool -> 'a grdf -> 'a grammar =
  fun ?(name="...") ?(recursive=false) d ->
    let k = Assoc.new_key () in
    { e = None; d; n = name; k; recursive; compiled = ref Comb.assert_false
    ; charset = None; phase = Defined; ne = ETmp; cache = false }

(** A type to store list of grammar keys *)
type ety = E : 'a Assoc.ty -> ety [@@unboxed]

(** printing functions *)


let prl pr sep ch l =
  let rec fn ch = function
    | [] -> ()
    | [x] -> pr ch x
    | x::l -> Printf.fprintf ch "%a%s%a" pr x sep fn l
  in
  fn ch l

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
    | EAlt(gs)      -> pr "(%a)" (prl pg "|") gs
    | EAppl(g,_)    -> pr "App(%a)" pg g
    | ESeq(g1,g2)   -> pr "%a%a" pg g1 pv g2
    | EDSeq(g1,_,_) -> pr "%a???" pg g1
    | ELr(g,_,_,s)  -> pr "%a(%a)*" pg g pv s
    | ERkey _       -> ()
    | ERef(g)       -> pr "%a" pv g
    | ERPos(g)      -> pr "%a" pg g
    | ELPos(_,g)    -> pr "%a" pg g
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
    | Alt(gs)      -> pr "(%a)" (prl pg "|") gs
    | Appl(g,_)    -> pr "App(%a)" pg g
    | Seq(g1,g2)   -> pr "%a%a" pg g1 pg g2
    | DSeq(g1,_,_) -> pr "%a???" pg g1
    | Lr(g,_,_,s)  -> pr "%a(%a)*" pg g pg s
    | Rkey _       -> ()
    | RPos(g)      -> pr "%a" pg g
    | LPos(_,g)    -> pr "%a" pg g
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

(** Interface to constructors.  propagate Fail because it is tested by
   elim_left_rec for the Lr suffix *)
let fail () = mkg Fail

let empty x = mkg (Empty x)

let test b = if b then empty () else fail ()

let term ?name (x) =
  if accept_empty x then invalid_arg "term: empty terminals";
  let name = match name with None -> x.Lex.n | Some n -> n in
  mkg ~name (Term x)

let alt ?name l =
  let l = List.filter (fun g -> g.d <> Fail) l in
  let l = List.map (function { d = Alt(ls); _ } -> ls | x -> [x]) l in
  let l = List.flatten l in
  match l with
  | [] -> fail ()
  | [g] -> g
  | l   -> mkg ?name (Alt(l))

let appl ?name g f = mkg ?name (if g.d = Fail then Fail else Appl(g,f))

let seq g1 g2 = mkg (
  match g1.d,g2.d with
  | Fail, _ -> Fail
  | _, Fail -> Fail
  | Empty x, _ -> Appl(g2, fun y -> y x)
  | _, Empty y -> Appl(g1, y)
  | _ -> Seq(g1,g2))

let dseq g1 ?(cs=Charset.full) g2 =
  mkg (if g1.d = Fail then Fail else DSeq(g1,cs,g2))

let lr g1 k ?(pk) g2 =
  if g2.d = Fail then g1
  else mkg (if g1.d = Fail then Fail else Lr(g1,k,pk,g2))

let lpos ?pk g = mkg (if g.d = Fail then Fail else LPos(pk,g))

let rpos g = mkg (if g.d = Fail then Fail else RPos(g))

let seq1 g1 g2 = seq g1 (appl g2 (fun _ x -> x))

let seq2 g1 g2 = seq g1 (appl g2 (fun x _ -> x))

let seq_rpos g1 g2 =
  seq (rpos (appl g1 (fun x rpos -> (x, rpos)))) g2

let seq_lpos g1 g2 =
  seq (lpos (appl g1 (fun x lpos -> (lpos, x)))) g2

let seq_pos g1 g2 =
  seq (lpos (rpos (appl g1 (fun x rpos lpos -> (lpos, x, rpos)))))
    g2

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
    if g1.d <> Tmp then
      failwith "set_grammar: grammar already set or not created by set_grammar";
    g1.d <- g2.d

let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?(name="...") g ->
    let g0 = declare_grammar name in
    set_grammar g0 (g g0);
    g0

let memo g =
  let tbl = Hashtbl_eq.create 8 in
  (fun x ->
    try Hashtbl_eq.find tbl x
    with Not_found ->
      let r = g x in Hashtbl_eq.add tbl x r; r)

let dseq g1 ?cs g2 = dseq g1 ?cs (memo g2)

(** a function to defined indexed grammars *)
let grammar_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = Hashtbl_eq.create 8 in
  let is_set = ref None in
  (fun p ->
    try Hashtbl_eq.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p) in
      Hashtbl_eq.add tbl p g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f p);
      );
      g),
  (fun f ->
    is_set := Some f;
    Hashtbl_eq.iter (fun p r ->
      set_grammar r (f p);
    ) tbl)

(** helpers for the constructors of 'a grne *)
let ne_alt l =
  let l = List.filter (fun g -> g <> EFail) l in
  let l = List.map (function EAlt(ls) -> ls | x -> [x]) l in
  let l = List.flatten l in
  match l with
  | [] -> EFail
  | [g] -> g
  | l   -> EAlt(l)

let ne_appl g f =
  match g with
  | EFail           -> EFail
  | EAppl(g,h)      -> EAppl(g,fun x -> f (h x))
  | _               -> EAppl(g,f)

let ne_seq g1 g2 = match(g1,g2) with
  | EFail, _                 -> EFail
  | _, {e=None;ne=EFail;_}   -> EFail
  | _, {e=Some y;ne=EFail;_} -> ne_appl g1 y
  | _, _                     -> ESeq(g1,g2)

let ne_dseq g1 cs g2 = match g1 with
  | EFail -> EFail
  | _     -> EDSeq(g1,cs,g2)

let ne_lr g k ?pk s =
  match (g, s) with
  | EFail, _ -> EFail
  | _, {ne=EFail;_} -> g
  | _        -> ELr(g,k,pk,s)

let ne_lpos ?pk g1 = match g1 with
  | EFail -> EFail
  | _     -> ELPos(pk,g1)

let ne_rpos g1 = match g1 with
  | EFail -> EFail
  | _     -> ERPos(g1)

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
  let get g =
    if g.recursive then ERef g else
      begin
        assert (g.ne <> ETmp);
        let ge = if g.cache then ECache g.ne else g.ne in
        ge
      end
  in

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
    | Alt(gs) -> List.iter fn gs;
                   let gn acc g = match acc with Some _ -> acc | None -> g.e in
                   List.fold_left gn None gs
    | Appl(g,f) -> fn g; (match g.e with
                    | None -> None
                    | Some x -> try Some (f x) with NoParse -> None)
    | Seq(g1,g2) -> fn g1; fn g2;
                      (match g1.e with
                       | None -> None
                       | Some x ->
                          match g2.e with
                          | None -> None
                          | Some y -> try Some(y x) with NoParse -> None)
    | DSeq(g1,_,g2) -> fn g1; (match g1.e with
                       | None -> None
                       | Some (x,x') ->
                          try
                            let g2 = g2 x in
                            fn g2;
                            match g2.e with
                            | None -> None
                            | Some y -> Some(y x')
                          with NoParse -> None)
    | Lr(g1,_,_,g2) -> fn g1; fn g2; g1.e
    | Rkey _        -> None
    | LPos(_,g1)    -> fn g1; (match g1.e with
                               | None -> None (* FIXME #14: Loose position *)
                               | Some x -> Some (x Pos.phantom))
    | RPos(g1)      -> fn g1; (match g1.e with
                               | None -> None (* FIXME #14: Loose position *)
                               | Some x -> Some (x Pos.phantom))
    | Layout(_,g,_) -> fn g; g.e
    | Cache(g)      -> fn g; g.e
    | Tmp           -> failwith "grammar compiled before full definition"
  in
  let rec hn : type a. a grammar -> unit = fun g ->
    if g.phase = EmptyComputed then
      begin
        g.phase <- EmptyRemoved;
        g.e <- kn g.d; (* need to computation to reach fixpoint *)
        g.ne  <- gn g.d;
      end

  and gn : type a. a grdf -> a grne = function
    | Fail -> EFail
    | Empty _ -> EFail
    | Term(x) -> ETerm(x)
    | Alt(gs) -> List.iter hn gs; ne_alt (List.map get gs)
    | Appl(g,f) -> hn g; ne_appl (get g) f
    | Seq(g1,g2) -> hn g1; hn g2;
                    let ga = ne_seq (get g1) g2 in
                    let gb = match g1.e with
                      | None   -> EFail
                      | Some x -> ne_appl (get g2) (fun y -> y x)
                    in
                    ne_alt [ga; gb]
    | DSeq(g1,cs,g2) -> hn g1; let ga = ne_dseq (get g1) cs g2 in
                       let gb = match g1.e with
                         | None   -> EFail
                         | Some (x,x') ->
                            let g2 = g2 x in fn g2; hn g2;
                                             ne_appl (get g2) (fun y -> y x')
                            (* FIXME, fn called twice on g2 x *)
                       in
                       ne_alt [ga; gb]
    | Lr(g1,k,pk,g2) -> hn g1; hn g2; ne_lr (get g1) k ?pk g2
    | Rkey k    -> ERkey k
    | LPos(pk,g1) -> hn g1; ne_lpos ?pk (get g1)
    | RPos(g1) -> hn g1; ne_rpos (get g1)
    | Cache(g1) -> hn g1; ne_cache(get g1)
    | Layout(b,g,cfg) -> hn g; ne_layout b (get g) cfg
    | Tmp           -> failwith "grammar compiled before full definition"

  in fn g; hn g

(** remove a cache prefix and move it up to the grammar definition. *)
let remove_cache : type a. a grammar -> unit =
  let rec fn : type a. a grne -> bool * a grne = fun g ->
    let cache = ref false in
    let rec fn : type a. bool -> a grne -> a grne =
      fun r -> function
        | EAlt(gs) -> ne_alt (List.map (fn r) gs)
        | EAppl(g1,f) -> ne_appl (fn r g1) f
        | ESeq(g1,g2) -> ne_seq (fn r g1) g2
        | EDSeq(g1,cs,g2) -> ne_dseq (fn r g1) cs g2
        | ERPos(g1) -> ne_rpos(fn r g1)
        | ELPos(pk,g1) -> ne_lpos ?pk (fn r g1)
        | ELr _  -> assert false
        | ERkey _  -> assert false
        | ECache(g1) -> cache := true; fn r g1
        | ELayout(b,g1,cfg) -> ne_layout b (fn r g1) cfg
        | ERef g0 as g -> gn g0; g
        | ETmp -> assert false
        | EFail | ETerm _ as g -> g
    in
    let g = fn false g in
    (!cache, g)

  and gn : type a.a grammar -> unit = fun g ->
    assert(g.phase >= EmptyRemoved);
    if g.phase = EmptyRemoved then
      begin
        g.phase <- CacheFactored;
        assert(g.ne <> ETmp);
        let (cache, g1) = fn g.ne in
        g.ne <- g1;
        g.cache <- cache;
      end
  in gn

(** Elimination of left recursion which is not supported by combinators *)
let rec elim_left_rec : type a. ety list -> a grammar -> unit = fun above g ->

  let u = g.k.k in
  let pk = ref None in
  let get_pk () = match !pk with
    | None -> let k = Assoc.new_key () in
              pk := Some k;
              !pk
    | _    -> !pk
  in

  let rec fn : type b. b grne -> b grne * b grammar =
    fun g ->
      match g with
      | EFail -> (EFail, fail ())
      | ETerm _ -> (g, fail ())
      | ESeq(g1,g2) ->
         let (g1, s1) = fn g1 in
         (ne_seq g1 g2, seq s1 g2)
      | EDSeq(g1,cs,g2) ->
         let (g1, s1) = fn g1 in
         (ne_dseq g1 cs g2, dseq s1 ~cs g2)
      | EAlt(gs) ->
         let (gs,ss) = List.split (List.map fn gs) in
         (ne_alt gs, alt ss)
      | ELr(g1,k,pk,s)   ->
         let (g1,s1) = fn g1 in
         (ne_lr g1 k ?pk s, lr s1 k ?pk s)
      | ERkey _ ->
         assert false; (* handled in gn *)
      | EAppl(g1,f) ->
         let (g1,s) = fn g1 in
         (ne_appl g1 f, appl s f)
      | ERef(g) -> gn g
      | ECache(g1) as g ->
         assert ((snd (fn g1)).d = Fail);
         (g, fail())
      | ERPos(g1) ->
         let (g1, s1) = fn g1 in
         (ne_rpos g1, rpos s1)
      | ELPos(Some _ as pk, g1) ->
         let (g1, s1) = fn g1 in
         (ne_lpos ?pk g1, lpos ?pk s1)
      | ELPos(None, g1) ->
         let pk = get_pk () in
         let (g1, s1) = fn g1 in
         (ne_lpos ?pk:None g1, lpos ?pk s1)
      | ELayout(_,g1,_) ->
         let (_, s1) = fn g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under layout change";
         (g, fail())
      | ETmp -> assert false

   and gn : type b. b grammar -> b grne * b grammar =
     fun g ->
       match g.k.eq u with
       | Assoc.Eq  ->
          (EFail, mkg (Rkey g.k))
       | Assoc.NEq ->
          if List.mem (E g.k.k) above then
            begin
              (ERef g, fail ())
            end
          else
            begin
              elim_left_rec (E u :: above) g;
              let (g',s) = fn g.ne in
              factor_empty s;
              assert(s.e = None);
              s.phase <- LeftRecEliminated;
              if s.ne = EFail then (ERef g, fail()) else (g', s)
            end
   in
   assert (g.phase>=CacheFactored);
   if g.phase = CacheFactored then
     begin
       g.phase <- LeftRecEliminated;
       let (g1,s) = fn g.ne in
       factor_empty s;
       s.e <- None;
       s.phase <- LeftRecEliminated;
       let pk = !pk in
       if s.ne <> EFail then g.ne <- ne_lr g1 g.k ?pk s;
     end

(** compute the characters set accepted at the beginning of the input *)
let first_charset : type a. a grne -> Charset.t = fun g ->

  let rec fn : type a. a grne -> bool * Charset.t = fun g ->
    match g with
    | EFail -> (false, Charset.empty)
    | ETerm(c) -> (false, c.c)
    | EAlt(gs) ->
       List.fold_left (fun (shift,s) g ->
           let (shift',s') = fn g in
           (shift || shift', Charset.union s s')) (false, Charset.empty) gs
    | ESeq(g,g2) ->
       let (shift, s as r) = fn g in
       if shift then
         begin
           let (shift, s') = fn g2.ne in
           assert (not shift);
           (false, Charset.union s s')
         end
       else r
    | EDSeq(g,cs,_) ->
       let (shift, _ as r) = fn g in if shift then (true, cs) else r
    | EAppl(g,_) -> fn g
    | ELr(g,_,_,_) -> fn g
    | ERkey _ -> (true, Charset.empty)
    | ERef g -> gn g
    | ERPos g -> fn g
    | ELPos (_,g) -> fn g
    | ECache g -> fn g
    | ELayout(_,g,cfg) -> if cfg.old_blanks_before
                             && not cfg.old_blanks_after
                          then fn g else (false, Charset.full)
    | ETmp -> assert false

  and gn : type a. a grammar -> bool * Charset.t = fun g ->
    assert g.recursive;
    match g.charset with
    | Some c -> (false, c)
    | None ->
       g.charset <- Some Charset.empty;
       let (shift, r) = fn g.ne in
       assert (not shift);
       g.charset <- Some r;
       (shift, r)
  in snd (fn g)

let split_list l =
  let rec fn acc1 acc2 =
    function [] -> acc1, acc2
           | x::l -> fn (x::acc2) acc1 l
  in
  fn [] [] l

(** compilation of a grammar to combinators *)
let rec compile_ne : type a. bool -> a grne -> a Comb.t = fun direct g ->
  let open Comb in
  match g with
  | EFail -> fail
  | ETerm(c) -> if direct then direct_lexeme c.f else lexeme c.f
  | EAlt(gs) -> compile_alt gs
  | ESeq(g1,g2) -> seq (compile_ne direct g1) (compile false true g2)
  | EDSeq(g1,_,g2) -> dseq (compile_ne direct g1)
                      (fun x -> compile false true (g2 x))
  | EAppl(g1,f) -> app (compile_ne direct g1) f
  | ELr(g,k,None,s) -> lr (compile_ne direct g) k (compile_ne false s.ne)
  | ELr(g,k,Some pk,s) -> lr_pos (compile_ne direct g) k pk
                            (compile_ne false s.ne)
  | ERkey k -> read_tbl k
  | ERef g -> compile true direct g
  | ERPos(g) -> right_pos (compile_ne direct g)
  | ELPos(None,g) -> left_pos (compile_ne direct g)
  | ELPos(Some pk,g) -> read_pos pk (compile_ne direct g)
  | ECache(g) -> cache (compile_ne false g)
  | ELayout(b,g,cfg) -> change_layout ~config:cfg b (compile_ne direct g)
  | ETmp -> assert false

 and compile_alt : type a. a grne list -> a Comb.t = fun gs ->
   let rec fn = function
     | [] -> (Charset.empty, Comb.fail)
     | [g] -> (first_charset g, compile_ne false g)
     | l -> let (l1,l2) = split_list l in
            let (cs1,c1) = fn l1 in
            let (cs2,c2) = fn l2 in
            (Charset.union cs1 cs2, Comb.alt cs1 c1 cs2 c2)
   in snd (fn gs)

 and compile : type a. bool -> bool -> a grammar -> a Comb.t =
  fun ne direct g ->
    factor_empty g;
    remove_cache g;
    elim_left_rec [] g;
    assert (g.phase >= LeftRecEliminated);
    (* NOTE: g.recursive is not enough here, with mutual recursion; after
           left rec elimination, the loop may be detected at other position
           in the tree *)
    let get g =
      if g.recursive || g.phase = Compiling then Comb.deref g.compiled
      else !(g.compiled)
    in
    let cg =
      match g.phase with
      | Compiled | Compiling -> get g
      | _ ->
         g.phase <- Compiling;
         let cg = compile_ne direct g.ne in
         let cg = if g.cache then Comb.cache cg else cg in
         g.compiled := cg;
         g.phase <- Compiled;
         get g
    in
    let e = if ne then None else g.e in
    match e with
    | Some x ->
       if g.ne = EFail then Comb.empty x
       else Comb.option x (first_charset g.ne) cg
    | None ->
       if g.ne = EFail then Comb.fail else cg

let compile g = compile true true g

let grammar_name g = g.n

(** functions to actually use the parser *)
let add_eof g = seq g (term (eof (fun x -> x)))

(* NOTE: cs with blank_after = false makes no sense ? *)
let partial_parse_buffer
    : type a. a t -> Lex.blank -> ?blank_after:bool
                  -> Lex.buf -> int -> a * Lex.buf * int =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let g = compile g in
    Comb.partial_parse_buffer g blank_fun ~blank_after buf0 col0

let parse_buffer : type a. a t -> Lex.blank -> Lex.buf -> int -> a =
  fun g blank_fun buf col ->
    let g = add_eof g in
    let (v,_,_) = partial_parse_buffer g blank_fun buf col in v

let parse_string : type a. a t -> Lex.blank -> string -> a =
  fun g b s -> parse_buffer g b (Input.from_string s) 0

let parse_channel : type a. a t -> Lex.blank -> in_channel -> a =
  fun g b ic -> parse_buffer g b (Input.from_channel ic) 0

let parse_all_buffer : type a. a t -> Lex.blank -> Lex.buf -> int -> a list =
  fun g blank_fun buf0 col0 ->
    let g = compile (add_eof g) in
    Comb.parse_all_buffer g blank_fun buf0 col0

let lpos g = lpos ?pk:None g
