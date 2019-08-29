open Lex

(** Type for a grammar *)
type 'a grammar =
  { mutable d : 'a grdf   (** the definition of the grammar *)
  ; n : string            (** name of the grammar *)
  ; k : 'a Comb.key       (** a key used mainly to detect recursion *)
  ; recursive : bool      (** really means declared first and defined after,
                              using declare/set_grammar *)
  ; mutable phase : phase (** which transformation phase reached for that
                              grammar *)
  ; mutable e: 'a list    (** not []   if the grammar accepts Empty.
                              valid from phase Empty Removed *)
  ; mutable ne : 'a grne  (** the part of the grammar that does not accept empty
                              valid from phase Empty Removed, but transformed
                              at LeftRecEliminated *)
  ; mutable compiled : 'a Comb.t ref
  (** the combinator for the grammar. One needs a ref for recursion.  valid from
                              phase Compiled *)
  ; mutable charset : Charset.t option
 (** cache for the first charset. Set only if used by compilation. *)
  }

 (** abreviation! *)
 and 'a t = 'a grammar

 (** The various transformation phase before until compilation to combinators *)
 and phase = Defined | EmptyComputed of int | EmptyRemoved
             | LeftRecEliminated | Compiling | Compiled

 (** Grammar constructors at definition *)
 and 'a grdf =
   | Fail : 'a grdf                        (** grammar that always fais *)
   | Err : string -> 'a grdf               (** error reporting *)
   | Empty : 'a -> 'a grdf                 (** accept only the empty input *)
   | Term : 'a Lex.t -> 'a grdf            (** terminals *)
   | Alt  : 'a t list -> 'a grdf           (** alternatives *)
   | Appl : 'a t * ('a -> 'b) -> 'b grdf   (** application *)
   | Seq  : 'a t * ('a -> 'b) t -> 'b grdf
                                           (** sequence *)
   | DSeq : ('a * 'b) t * ('a -> ('b -> 'c) t) -> 'c grdf
                                           (** dependant sequence *)
   | Lr   : 'a t * 'a Comb.key * Pos.t Assoc.key option * 'a t -> 'a grdf
                                           (** Lr(g1,g2) represents g1 g2* and
                                               is used to eliminate left
                                               recursion.  It can not be exposed
                                               as left recursion under Lr is not
                                               supported. The key is used to
                                               index the semantic of the partial
                                               parse in the lr table. *)
   | Rkey : 'a Comb.key -> 'a grdf
   | LPos : Pos.t Assoc.key option * (Pos.t -> 'a) t -> 'a grdf
                                           (** read the postion before parsing,
                                               the key is present, the position
                                               is stored in the lr table *)
   | RPos : (Pos.t -> 'a) t -> 'a grdf     (** read the postion after parsing *)
   | Layout : blank * 'a t * layout_config -> 'a grdf
                                           (** changes the blank function *)
   | Cache : ('a -> 'a -> 'a) option * 'a t -> 'a grdf
                                           (** caches the grammar *)
   | Test : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool) * bool *
              'a t -> 'a grdf              (** test, before (true)  or after *)
   | Tmp  : 'a grdf                        (** used as initial value for
                                               recursive grammar. *)

 (** Type after elimination of empty  and for later phase.  same constructors as
     above prefixed  by E,  The left branch  does not go  trough the  'a grammar
     record except for recursion.  *)
 and 'a grne =
   | EFail : 'a grne
   | EErr  : string -> 'a grne
   | ETerm : 'a terminal -> 'a grne
   | EAlt  : 'a grne list -> 'a grne
   | EAppl : 'a grne * ('a -> 'b) -> 'b grne
   | ESeq  : 'a grne * ('a -> 'b) t -> 'b grne
   | EDSeq : ('a * 'b) grne * ('a -> ('b -> 'c) t) -> 'c grne
   | ELr   : 'a grne * 'a Comb.key * Pos.t Assoc.key option * 'a grammar
               -> 'a grne
   | ERkey : 'a Comb.key -> 'a grne
   | ERef  : 'a t -> 'a grne
   | ELPos : Pos.t Assoc.key option * (Pos.t -> 'a) grne -> 'a grne
   | ERPos : (Pos.t -> 'a) grne -> 'a grne
   | ELayout : blank * 'a grne * layout_config -> 'a grne
   | ECache : ('a -> 'a -> 'a) option * 'a grne -> 'a grne
   | ETest : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool) * bool *
              'a grne -> 'a grne
   | ETmp  : 'a grne

(** grammar renaming *)
let give_name n g = { g with n }

(** helper to construct the initial ['a grammar] record *)
let mkg : ?name:string -> ?recursive:bool -> 'a grdf -> 'a grammar =
  fun ?(name="...") ?(recursive=false) d ->
    let k = Assoc.new_key () in
    { e = []; d; n = name; k; recursive; compiled = ref Comb.assert_false
    ; charset = None; phase = Defined; ne = ETmp }

(** A type to store list of grammar keys *)
type ety = E : 'a Assoc.token -> ety [@@unboxed]

(** printing functions, usable for debugging, not yet for documentation
    of your code. *)
let prl pr sep ch l =
  let rec fn ch = function
    | [] -> ()
    | [x] -> pr ch x
    | x::l -> Printf.fprintf ch "%a%s%a" pr x sep fn l
  in
  fn ch l

type prio = Atom | Seq | Alt
type any_grammar = G : 'a grammar -> any_grammar

let print_grammar ?(def=true) ch s =
  let adone = ref [] in
  let todo = ref [] in
  let do_def g =
    g.recursive ||
      (g.n <> "..." && match g.d with Term _ -> false | _ -> true)
  in
  let rec print_grne : type a. prio -> out_channel -> a grne -> unit =
  fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_grne x in
    let pv x = print_negr x in
    let rec is_empty : type a.a grne -> bool = function
      | ERkey _        -> true
      | EAppl(g,_)     -> is_empty g
      | ERPos(g)       -> is_empty g
      | ELPos(_,g)     -> is_empty g
      | ECache(_,g)    -> is_empty g
      | ETest(_,_,g)   -> is_empty g
      | ELayout(_,g,_) -> is_empty g
      | _              -> false
    in
    match g with
    | EFail         -> pr "0"
    | EErr m        -> pr "0(%s)" m
    | ETerm t       -> pr "%s" t.n
    | EAlt(gs)      -> pr (if prio < Alt then "(%a)" else "%a")
                          (prl (pg Seq) " | ") gs
    | ESeq(ERkey _, g) -> pv prio ch g
    | ESeq(g1,g2)   -> if is_empty g1 then
                         pg prio ch g1
                       else if is_empty g2.ne then
                         pv prio ch g2
                       else
                         pr (if prio < Seq then "(%a %a)" else "%a %a")
                           (pg Atom) g1 (pv Seq) g2
    | EDSeq(g1,_)   -> pr (if prio < Seq then "(%a ...)" else "%a ...")
                         (pg Atom) g1
    | ELr(g,_,_,s)  -> pr (if prio < Seq then "(%a %a*)" else "%a %a*")
                         (pg Atom) g (pv Atom) s
    | ERkey _       -> ()
    | EAppl(g,_)    -> pg prio ch g
    | ERef(g)       -> pv prio ch g
    | ERPos(g)      -> pg prio ch g
    | ELPos(_,g)    -> pg prio ch g
    | ECache(_,g)   -> pg prio ch g
    | ETest(_,_,g)  -> pg prio ch g
    | ELayout(_,g,_) -> pg prio ch g
    | ETmp          -> pr "TMP"

  and print_grdf : type a. prio -> out_channel -> a grdf -> unit =
  fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    let pg x = print_dfgr x in
    let rec is_empty : type a. a grdf -> bool = function
      | Rkey _ | Empty _ -> true
      | Appl(g,_)        -> is_empty g.d
      | RPos(g)          -> is_empty g.d
      | LPos(_,g)        -> is_empty g.d
      | Cache(_,g)       -> is_empty g.d
      | Test(_,_,g)      -> is_empty g.d
      | Layout(_,g,_)    -> is_empty g.d
      | _                -> false
    in
    match g with
    | Fail         -> pr "0"
    | Err m        -> pr "0(%s)" m
    | Empty _      -> pr "()"
    | Term t       -> pr "%s" t.n
    | Alt(gs)      -> pr (if prio < Alt then "(%a)" else "%a")
                        (prl (pg Seq) " | ") gs
    | Seq(g1,g2)   -> if is_empty g1.d then
                        pg prio ch g2
                      else if is_empty g2.d then
                        pg prio ch g1
                      else
                        pr (if prio < Seq then "(%a %a)" else "%a %a")
                          (pg Atom) g1 (pg Seq) g2
    | DSeq(g1,_)   -> pr (if prio < Seq then "(%a ...)" else "%a ...")
                         (pg Atom) g1
    | Lr(g,_,_,s)  -> pr (if prio < Seq then "(%a %a*)" else "%a %a*")
                         (pg Atom) g (pg Atom) s
    | Rkey _       -> ()
    | Appl(g,_)    -> pg prio ch g
    | RPos(g)      -> pg prio ch g
    | LPos(_,g)    -> pg prio ch g
    | Cache(_,g)   -> pg prio ch g
    | Test(_,_,g)  -> pg prio ch g
    | Layout(_,g,_)-> pg prio ch g
    | Tmp          -> pr "TMP"

  and print_negr : type a. prio -> out_channel -> a grammar -> unit =
  fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    if List.mem (E g.k.tok) !adone then Printf.fprintf ch "%s" g.n
    else if do_def g then
      begin
        adone := E g.k.tok :: !adone;
        todo := G g :: !todo;
        pr "%s" g.n
      end
    else
      begin
        let pg x = print_grne x in
        match g.e with
        | [] -> pr "%a" (pg prio) g.ne
        | _  -> pr "(() | %a)" (pg Alt) g.ne
      end

  and print_dfgr : type a. prio -> out_channel -> a grammar -> unit =
  fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    if List.mem (E g.k.tok) !adone then Printf.fprintf ch "%s" g.n
    else if do_def g then
      begin
        adone := E g.k.tok :: !adone;
        todo := G g :: !todo;
        pr "%s" g.n
      end
    else
      begin
        let pg x = print_grdf x in
        pr "%a" (pg prio) g.d
      end

  in
  todo := G s :: !todo;
  while !todo != [] do
    match !todo with
      [] -> assert false
    | G s::l ->
       todo := l;
       let pr f x = Printf.fprintf ch "%s ::= %a\n" s.n f x in
       let pne ch s =
         match s.e with
         | [] -> print_grne Alt ch s.ne
         | _  -> Printf.printf "(() | %a)" (print_grne Alt) s.ne
       in
       if def then pr (print_grdf Alt) s.d else pr pne s

  done

(** Interface to constructors.  propagate Fail because it is tested by
   elim_left_rec for the Lr suffix *)
let fail ?name () = mkg ?name Fail

let empty ?name x = mkg ?name (Empty x)

let cond ?name b = if b then empty ?name () else fail ?name ()

let term ?name (x) =
  if accept_empty x then invalid_arg "term: empty terminals";
  let name = match name with None -> x.Lex.n | Some n -> n in
  mkg ~name (Term x)

let alt ?name l =
  let l = List.filter (fun g -> g.d <> Fail) l in
  let l = List.map (function { d = Alt(ls) } -> ls | x -> [x]) l in
  let l = List.flatten l in
  match l with
  | [] -> fail ()
  | [g] -> g
  | l   -> mkg ?name (Alt(l))

let appl ?name g f = mkg ?name (if g.d = Fail then Fail else Appl(g,f))

let seq ?name g1 g2 = mkg ?name (
  match g1.d,g2.d with
  | Fail, _ -> Fail
  | _, Fail -> Fail
  | Empty x, _ -> Appl(g2, fun y -> y x)
  | _, Empty y -> Appl(g1, y)
  | _ -> Seq(g1,g2))

let dseq ?name g1 g2 =
  mkg ?name (if g1.d = Fail then Fail else DSeq(g1,g2))

let lr ?name g1 k ?(pk) g2 =
  if g2.d = Fail then g1
  else mkg ?name (if g1.d = Fail then Fail else Lr(g1,k,pk,g2))

let lpos ?name ?pk g = mkg ?name (if g.d = Fail then Fail else LPos(pk,g))

let rpos ?name g = mkg ?name (if g.d = Fail then Fail else RPos(g))

let seq1 ?name g1 g2 = seq ?name g1 (appl g2 (fun _ x -> x))

let seq2 ?name g1 g2 = seq ?name g1 (appl g2 (fun x _ -> x))

let seq_rpos ?name g1 g2 =
  seq ?name (rpos (appl g1 (fun x rpos -> (x, rpos)))) g2

let seq_lpos ?name g1 g2 =
  seq ?name (lpos (appl g1 (fun x lpos -> (lpos, x)))) g2

let seq_pos ?name g1 g2 =
  seq ?name (lpos (rpos (appl g1 (fun x rpos lpos -> (lpos, x, rpos)))))
    g2

let error ?name m = mkg ?name (Err m)

let cache ?name ?merge g =
  mkg ?name (if g.d = Fail then Fail else Cache(merge,g))

let test ?name f b g = mkg ?name (if g.d = Fail then Fail else Test(f,b,g))

let test_before ?name f g = test ?name f true g

let test_after ?name f g = test ?name f false g

let no_blank_before g =
  let fn b1 c1 b2 c2 = Input.buffer_equal b1 b2 && c1 = c2 in
  test_before ~name:"no_blank"  fn g

let layout ?name ?(config=default_layout_config) b g =
  mkg ?name (if g.d = Fail then Fail else Layout(b,g,config))

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

let dseq ?name g1 g2 = dseq ?name g1 (memo g2)

let option : ?name:string -> 'a grammar -> 'a option grammar = fun ?name g ->
  alt ?name [appl g (fun x -> Some x); empty None]

let default_option : ?name:string -> 'a -> 'a grammar -> 'a grammar =
  fun ?name d g -> alt ?name [g; empty d]

let star : ?name:string -> 'a grammar -> 'a list grammar = fun ?name g ->
  appl ?name (fixpoint (fun r ->
            alt [empty [];
                 seq r (appl g (fun x l -> x::l))])) List.rev

let plus : ?name:string -> 'a grammar -> 'a list grammar = fun ?name g ->
  appl ?name (fixpoint (fun r ->
            alt [appl g (fun x -> [x]);
                 seq r (appl g (fun x l -> x::l))])) List.rev

let plus_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar =
  fun ?name sep g ->
    appl ?name (fixpoint (fun r ->
            alt [appl g (fun x -> [x]);
                 seq r (seq sep (appl g (fun x _ l -> x::l)))])) List.rev

let star_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar =
  fun ?name sep g -> alt ?name [empty []; plus_sep sep g]

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
  | EFail, _            -> EFail
  | _, {e=[];ne=EFail}  -> EFail
  | _, {e=[y];ne=EFail} -> ne_appl g1 y
  | _, _                -> ESeq(g1,g2)

let ne_dseq g1 g2 = match g1 with
  | EFail -> EFail
  | _     -> EDSeq(g1,g2)

let ne_lr g k ?pk s =
  match (g, s) with
  | EFail, _      -> EFail
  | _, {ne=EFail} -> g
  | _             -> ELr(g,k,pk,s)

let ne_lpos ?pk g1 = match g1 with
  | EFail -> EFail
  | _     -> ELPos(pk,g1)

let ne_rpos g1 = match g1 with
  | EFail -> EFail
  | _     -> ERPos(g1)

let ne_cache merge g1 = match g1 with
  | EFail -> EFail
  | _     -> ECache(merge,g1)

let ne_test f b g1 = match g1 with
  | EFail -> EFail
  | _     -> ETest(f,b,g1)

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
        g.ne
      end
  in

  let target = ref 0 in
  let changed = ref false in

  let rec fn : type a. a grammar -> unit = fun g ->
  match g.phase with
  | Defined ->
     g.phase <- EmptyComputed !target;
     let e = kn g.d in
     if List.length e <> List.length g.e then changed := true;
     g.e  <- e;
  | EmptyComputed n when n < !target ->
     g.phase <- EmptyComputed !target;
     let e = kn g.d in
     if List.length e <> List.length g.e then changed := true;
     g.e  <- e;
  | _ -> ()

  and kn : type a. a grdf -> a list = function
    | Fail -> []
    | Err _ -> []
    | Empty x -> [x]
    | Term _     -> []
    | Alt(gs) -> List.iter fn gs;
                   let gn acc g = g.e @ acc in
                   List.fold_left gn [] gs
    | Appl(g,f) -> fn g; List.map f g.e
    | Seq(g1,g2) -> fn g1; fn g2;
                    List.fold_left (fun acc x ->
                        List.fold_left (fun acc y ->
                            try y x :: acc
                            with NoParse -> acc) acc g2.e)
                      [] g1.e
    | DSeq(g1,g2) -> fn g1;
                       List.fold_left (fun acc (x,x') ->
                           try
                             let g2 = g2 x in
                             fn g2;
                             List.fold_left (fun acc y ->
                                 try y x' :: acc
                                 with NoParse -> acc) acc g2.e
                         with NoParse -> acc)
                         [] g1.e
    | Lr(g1,_,_,g2) -> fn g1; fn g2; g1.e
    | Rkey _        -> []
    | LPos(_,g1)    -> fn g1; List.map (fun x -> x Pos.phantom) g1.e
                       (* FIXME #14: Loose position *)
    | RPos(g1)      -> fn g1; List.map (fun x -> x Pos.phantom) g1.e
                       (* FIXME #14: Loose position *)
    | Layout(_,g,_) -> fn g; g.e
    | Cache(_,g)    -> fn g; g.e
    | Test(_,_,g)   -> fn g;
                       if g.e <> [] then
                         failwith "illegal test on grammar accepting empty";
                       []
    | Tmp           -> failwith "grammar compiled before full definition"
  in
  let rec hn : type a. a grammar -> unit = fun g ->
    match g.phase with
    | EmptyComputed _ ->
       g.phase <- EmptyRemoved;
       g.ne  <- gn g.d;
    | _ -> ()

  and gn : type a. a grdf -> a grne = function
    | Fail -> EFail
    | Empty _ -> EFail
    | Err m -> EErr m
    | Term(x) -> ETerm(x)
    | Alt(gs) -> List.iter hn gs; ne_alt (List.map get gs)
    | Appl(g,f) -> hn g; ne_appl (get g) f
    | Seq(g1,g2) -> hn g1; hn g2;
                    let ga = ne_seq (get g1) g2 in
                    let gb = ne_alt (List.map (fun x ->
                                         ne_appl (get g2) (fun y -> y x)) g1.e)
                    in
                    ne_alt [ga; gb]
    | DSeq(g1,g2) -> hn g1;
                        let ga = ne_dseq (get g1) g2 in
                        let gb =
                          ne_alt (List.fold_left (fun acc (x,x') ->
                                   try
                                     let g2 = g2 x in
                                     fn g2; hn g2;
                                     ne_appl (get g2) (fun y -> y x') :: acc
                                   with NoParse -> acc) [] g1.e)
                                (* FIXME, fn called twice on g2 x *)
                       in
                       ne_alt [ga; gb]
    | Lr(g1,k,pk,g2) -> hn g1; hn g2; ne_lr (get g1) k ?pk g2
    | Rkey k    -> ERkey k
    | LPos(pk,g1) -> hn g1; ne_lpos ?pk (get g1)
    | RPos(g1) -> hn g1; ne_rpos (get g1)
    | Cache(m,g1) -> hn g1; ne_cache m (get g1)
    | Test(f,b,g1) -> hn g1; ne_test f b (get g1)
    | Layout(b,g,cfg) -> hn g; ne_layout b (get g) cfg
    | Tmp           -> failwith "grammar compiled before full definition"

  in
  let rec loop () =
    changed := false;
    fn g;
    if !changed then (incr target; loop ())
  in
  loop ();
  hn g

type cs = NoLr | HasCache | NoCache

let (|||) cs1 cs2 = match cs1, cs2 with
  | NoLr, x    | x, NoLr    -> x
  | NoCache, _ | _, NoCache -> NoCache
  | HasCache   , HasCache   -> HasCache

(** Elimination of left recursion which is not supported by combinators *)
let rec elim_left_rec : type a. ety list -> a grammar -> unit = fun above g ->
  let u = g.k.tok in
  let pk = ref None in
  let get_pk () = match !pk with
    | None -> let k = Assoc.new_key () in
              pk := Some k;
              !pk
    | _    -> !pk
  in

  let rec fn : type b. ety list -> b grne -> b grne * b grammar * cs =
    fun above g ->
      match g with
      | EFail | ETerm _ | EErr _ -> (g, fail (), NoLr)
      | ESeq(g1,g2) ->
         let (g1, s1, cs) = fn above g1 in
         (ne_seq g1 g2, seq s1 g2, cs)
      | EDSeq(g1,g2) ->
         let (g1, s1, sc) = fn above g1 in
         (ne_dseq g1 g2, dseq s1 g2, sc)
      | EAlt(gs) ->
         let (gs,ss,cs) = List.fold_left (fun (gs,ss,cs) g ->
                              let (g,s,cs') = fn above g in
                              ((g::gs), (s::ss), (cs|||cs')))
                            ([],[],NoLr) gs
         in
         (ne_alt gs, alt ss, cs)
      | ELr(g1,k,pk,s)   ->
         let (g1,s1,cs) = fn above g1 in
         (ne_lr g1 k ?pk s, lr s1 k ?pk s,cs)
      | ERkey _ ->
         assert false; (* handled in gn *)
      | EAppl(g1,f) ->
         let (g1,s,cs) = fn above g1 in
         (ne_appl g1 f, appl s f,cs)
      | ERef(g) -> gn above g
      | ECache(_,g1) as g ->
         let (_, _, cs) = fn above g1 in
         (* grammar with cache can be recursive *)
         (g, fail (), if cs = NoLr then NoLr else HasCache)
      | ERPos(g1) ->
         let (g1, s1, cs) = fn above g1 in
         (ne_rpos g1, rpos s1, cs)
      | ELPos(Some _ as pk, g1) ->
         let (g1, s1, cs) = fn above g1 in
         (ne_lpos ?pk g1, lpos ?pk s1, cs)
      | ELPos(None, g1) ->
         let pk = get_pk () in
         let (g1, s1, cs) = fn above g1 in
         (ne_lpos ?pk:None g1, lpos ?pk s1, cs)
      | ETest(f, b, g1) ->
         let (g1, s1, cs) = fn above g1 in
         (ne_test f b g1, (if b then s1 else test f b s1), cs)
      | ELayout(_,g1,_) ->
         let (_, s1, cs) = fn above g1 in
         if s1.d <> Fail then
           invalid_arg "fixpoint: left recursion under layout change";
         (g, fail(), cs)
      | ETmp -> assert false

   and gn : type b. ety list -> b grammar -> b grne * b grammar * cs =
     fun above g ->
       match g.k.eq u with
       | Assoc.Eq  ->
          (EFail, mkg (Rkey g.k), NoCache)
       | Assoc.NEq ->
          if List.mem (E g.k.tok) above then
            begin
              (ERef g, fail (), NoLr)
            end
          else
            begin
              elim_left_rec (E u :: above) g;
              let (g',s,cs) = fn (E g.k.tok :: above) g.ne in
              factor_empty s;
              assert(s.e = []);
              s.phase <- LeftRecEliminated;
              if s.ne = EFail then (ERef g, fail(),cs) else (g', s,cs)
            end

   in
   assert(g.phase >= EmptyRemoved);
   if g.phase = EmptyRemoved then
     begin
       g.phase <- LeftRecEliminated;
       let (g1,s,cs) = fn above g.ne in
       if cs = NoCache then
         begin
           assert (s.ne <> EFail);
           factor_empty s;
           s.e <- [];
           s.phase <- LeftRecEliminated;
           let pk = !pk in
           g.ne <- ne_lr g1 g.k ?pk s;
         end
     end

(** compute the characters set accepted at the beginning of the input *)
let first_charset : type a. a grne -> Charset.t = fun g ->

  let rec fn : type a. a grne -> bool * Charset.t = fun g ->
    match g with
    | EFail -> (false, Charset.empty)
    | EErr _ -> (false, Charset.full)
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
    | EDSeq(g,_) ->
       let (shift, _ as r) = fn g in if shift then (true, Charset.full) else r
    | EAppl(g,_) -> fn g
    | ELr(g,_,_,_) -> fn g
    | ERkey _ -> (true, Charset.empty)
    | ERef g -> gn g
    | ERPos g -> fn g
    | ELPos (_,g) -> fn g
    | ECache (_,g) -> fn g
    | ETest (_,_,g) -> fn g
    | ELayout(_,g,cfg) -> if cfg.old_blanks_before
                             && not cfg.new_blanks_before
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
let rec compile_ne : type a. a grne -> a Comb.t = fun g ->
  let open Comb in
  match g with
  | EFail -> fail
  | EErr m -> error m
  | ETerm(c) -> lexeme c.f
  | EAlt(gs) -> compile_alt gs
  | ESeq(g1,g2) -> seq (compile_ne g1) (compile false g2)
  | EDSeq(g1,g2) -> dseq (compile_ne g1)
                      (fun x -> compile false (g2 x))
  | EAppl(g1,f) -> app (compile_ne g1) f
  | ELr(g,k,None,s) -> lr (compile_ne g) k (compile_ne s.ne)
  | ELr(g,k,Some pk,s) -> lr_pos (compile_ne g) k pk (compile_ne s.ne)
  | ERkey k -> read_tbl k
  | ERef g -> compile true g
  | ERPos(g) -> right_pos (compile_ne g)
  | ELPos(None,g) -> left_pos (compile_ne g)
  | ELPos(Some pk,g) -> read_pos pk (compile_ne g)
  | ECache(merge,g) -> cache ?merge (compile_ne g)
  | ETest(f,true,g) -> test_before f (compile_ne g)
  | ETest(f,false,g) -> test_after f (compile_ne g)
  | ELayout(b,g,cfg) -> change_layout ~config:cfg b (compile_ne g)
  | ETmp -> assert false

 and compile_alt : type a. a grne list -> a Comb.t = fun gs ->
  let l = List.map (fun g -> (first_charset g, compile_ne g)) gs in
  alts l

 and alts : type a. (Charset.t * a Comb.t) list -> a Comb.t = fun l ->
  let rec fn = function
     | [] -> (Charset.empty, Comb.fail)
     | [(cs,g)] -> (cs, g)
     | l -> let (l1,l2) = split_list l in
            let (cs1,c1) = fn l1 in
            let (cs2,c2) = fn l2 in
            (Charset.union cs1 cs2, Comb.alt cs1 c1 cs2 c2)
   in snd (fn l)

 and compile : type a. bool -> a grammar -> a Comb.t =
  fun ne g ->
    factor_empty g;
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
         let cg = compile_ne g.ne in
         g.compiled := cg;
         g.phase <- Compiled;
         get g
    in
    let e = if ne then [] else g.e in
    match e with
    | [] ->
       if g.ne = EFail then Comb.fail else cg
    | [x] ->
       if g.ne = EFail then Comb.empty x
       else Comb.option x (first_charset g.ne) cg
    | l ->
       let ce = alts (List.map (fun x -> Charset.full, Comb.empty x) l) in
       Comb.alt Charset.full ce (first_charset g.ne) cg

let compile g = compile false g

let grammar_name g = g.n

(** functions to actually use the parser *)
let add_eof g = seq g (term (eof (fun x -> x)))

(* NOTE: cs with blank_after = false makes no sense ? *)
let partial_parse_buffer
    : type a. a t -> Lex.blank -> ?blank_after:bool
                  -> Lex.buf -> Lex.pos -> a * Lex.buf * Lex.pos =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let g = compile g in
    Comb.partial_parse_buffer g blank_fun ~blank_after buf0 col0

let parse_buffer
    : type a. a t -> Lex.blank -> Lex.buf -> Lex.pos -> a =
  fun g blank_fun buf col ->
    let g = add_eof g in
    let (v,_,_) = partial_parse_buffer g blank_fun buf col in v

let parse_all_buffer
    : type a. a t -> Lex.blank -> Lex.buf -> Lex.pos -> a list =
  fun g blank_fun buf0 col0 ->
    let g = compile (add_eof g) in
    Comb.parse_all_buffer g blank_fun buf0 col0

let parse_string
    : type a. ?utf8:bool -> ?filename:string ->
              a t -> Lex.blank -> string -> a =
  fun ?(utf8=false) ?filename g b s ->
    parse_buffer g b (Input.from_string ~utf8 ?filename s) Input.init_pos

let parse_channel
    : type a. ?utf8:bool -> ?filename:string ->
              a t -> Lex.blank -> in_channel -> a =
  fun ?(utf8=false) ?filename g b ic ->
    parse_buffer g b (Input.from_channel ~utf8 ?filename ic) Input.init_pos

let lpos ?name g = lpos ?name ?pk:None g
