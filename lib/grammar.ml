open Lex
module Uf = UnionFind

type 'a cache = NoCache | Cache of ('a -> 'a -> 'a) option

(** Type for a grammar *)
type 'a grammar =
  { mutable d : 'a grdf   (** the definition of the grammar *)
  ; n : string            (** name of the grammar *)
  ; k : 'a Comb.key       (** a key used mainly to detect recursion *)
  ; recursive : bool      (** really means declared first and defined after,
                              using declare/set_grammar *)
  ; mutable cached : 'a cache     (** is th grammar cached *)
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
  ; mutable charset : (int * Charset.t) option
  (** cache for the first charset. Set only if used by compilation.
      the int is used to reach the fixpoint as in EmptyComputed below*)
  }

 (** abreviation *)
 and 'a t = 'a grammar

 and 'a key = 'a Comb.key

 (** The various transformation phase before until compilation to combinators *)
 and phase = Defined | EmptyComputed of int | EmptyRemoved
           | LeftRecEliminated | Compiling | Compiled

 (** type for the left prefix as in Comb *)
 and mlr_left =
   LNil : mlr_left
 | LCns : 'a key * 'a grne * mlr_left -> mlr_left

 (** type for the right prefix as in Comb *)
 and mlr_right =
   RNil : mlr_right
 | RCns : 'a key * 'b key * 'b grammar * mlr_right -> mlr_right

 (** type for all information about mutuall recursive grammars *)
 and mlr = { left : mlr_left
           ; right : mlr_right
           ; lpos : Pos.t Assoc.key option
           ; names : string list
           ; keys : Assoc.any_key list }

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
   | Rkey : 'a key -> 'a grdf              (** access to the lr table *)
   | LPos : mlr Uf.t option * (Pos.t -> 'a) t -> 'a grdf
                                           (** read the postion before parsing,
                                               the key is present, the position
                                               is stored in the lr table *)
   | RPos : (Pos.t -> 'a) t -> 'a grdf     (** read the postion after parsing *)
   | Layout : Blank.t * 'a t * Blank.layout_config -> 'a grdf
                                           (** changes the blank function *)
   | Test : 'a test * 'a t -> 'a grdf      (** test, before or after *)
   | Eval : 'a t -> 'a grdf                (** force evaluation *)
   | Tmp  : 'a grdf                        (** used as initial value for
                                               recursive grammar. *)

 and 'a test =
   | Before of (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
   | After of ('a -> Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)

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
   | ERkey : 'a key -> 'a grne
   | ERef  : 'a t -> 'a grne
   | ELPos : mlr Uf.t option * (Pos.t -> 'a) grne -> 'a grne
   | ERPos : (Pos.t -> 'a) grne -> 'a grne
   | ELayout : Blank.t * 'a grne * Blank.layout_config -> 'a grne
   | ETest : 'a test * 'a grne -> 'a grne
   | EEval : 'a grne -> 'a grne
   | ETmp  : 'a grne
   (** only new constructor, introduced by elimination of left recursion.
       the key give the grammar we actually parse and the mlr information
       is stored in a union find data structure to allow merging grammar
       when we discover they are mutually left dependant *)
   | ELr   : 'a key * mlr Uf.t -> 'a grne

(** grammar renaming *)
let give_name n g = { g with n }

(** helper to construct the initial ['a grammar] record *)
let mkg : ?name:string -> ?recursive:bool -> ?cached:'a cache ->
          'a grdf -> 'a grammar =
  fun ?(name="...") ?(recursive=false) ?(cached=NoCache) d ->
    let k = Assoc.new_key () in
    { e = []; d; n = name; k; recursive; cached
    ; compiled = ref Comb.assert_false
    ; charset = None; phase = Defined; ne = ETmp }

(** cache is added as information in the grammar record because when
    a grammar is cached, elimination of left recursion is useless *)
let cache ?name ?merge g =
  let name = match name with None -> g.n | Some n -> n in
  { g with cached = Cache merge; n = name }

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

let print_grammar ?(no_other=false) ?(def=true) ch s =
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
      | ETest(_,g)     -> is_empty g
      | ELayout(_,g,_) -> is_empty g
      | EEval(g)       -> is_empty g
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
    | ELr(_,_)      -> () (* FIXME #21 *)
    | ERkey _       -> () (* FIXME #21 *)
    | EAppl(g,_)    -> pg prio ch g
    | ERef(g)       -> pv prio ch g
    | ERPos(g)      -> pg prio ch g
    | ELPos(_,g)    -> pg prio ch g
    | ETest(_,g)    -> pg prio ch g
    | ELayout(_,g,_)-> pg prio ch g
    | EEval(g)      -> pg prio ch g
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
      | Test(_,g)        -> is_empty g.d
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
    | Rkey _       -> ()
    | Appl(g,_)    -> pg prio ch g
    | RPos(g)      -> pg prio ch g
    | LPos(_,g)    -> pg prio ch g
    | Test(_,g)    -> pg prio ch g
    | Layout(_,g,_)-> pg prio ch g
    | Eval(g)      -> pg prio ch g
    | Tmp          -> pr "TMP"

  and print_negr : type a. prio -> out_channel -> a grammar -> unit =
  fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    if List.memq (Assoc.K g.k) !adone then Printf.fprintf ch "%s" g.n
    else if do_def g then
      begin
        adone := K g.k :: !adone;
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
    if List.memq (Assoc.K g.k) !adone then Printf.fprintf ch "%s" g.n
    else if do_def g then
      begin
        adone := K g.k :: !adone;
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
       if def then pr (print_grdf Alt) s.d else pr pne s;
       if no_other then todo := []

  done

let print_grne : type a. out_channel -> a grne -> unit =
  fun ch ne ->
  let g =
    { e = []
    ; d = Tmp
    ; n = ""
    ; k = Assoc.new_key ()
    ; recursive = false
    ; cached = NoCache
    ; phase = Defined
    ; ne
    ; compiled = ref Comb.assert_false
    ; charset = None
    }
  in
  print_grammar ~no_other:true ~def:false ch g

let _ = print_grne

(** Interface to constructors.  propagate Fail because it is tested by
   elim_left_rec for the Lr suffix *)
let fail ?name () = mkg ?name Fail

let empty ?name x = mkg ?name (Empty x)

let cond ?name b = if b then empty ?name () else fail ?name ()

let term ?name (x) =
  if accept_empty x then
    invalid_arg (Printf.sprintf "term: empty terminals %s" x.n);
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

let eval ?name g =
  mkg ?name (if g.d = Fail then Fail else Eval(g))

let test ?name f g = mkg ?name (if g.d = Fail then Fail else Test(f,g))

let test_before ?name f g = test ?name (Before f) g

let test_after ?name f g = test ?name (After f) g

let no_blank_before g =
  let fn b1 c1 b2 c2 = Input.buffer_equal b1 b2 && c1 = c2 in
  test_before ~name:"no_blank"  fn g

let no_blank_after g =
  let fn _ b1 c1 b2 c2 = Input.buffer_equal b1 b2 && c1 = c2 in
  test_after ~name:"no_blank"  fn g

let layout ?name ?(config=Blank.default_layout_config) b g =
  mkg ?name (if g.d = Fail then Fail else Layout(b,g,config))

(** function to define mutually recursive grammar:
    - first one declares the grammars
    - second one set the grammars *)
let declare_grammar name =
  mkg ~name ~recursive:true Tmp

let set_grammar : type a. a grammar -> a grammar -> unit =
  fun g1 g2 ->
    if g1.d <> Tmp then
      failwith
        (Printf.sprintf
           "set_grammar: grammar %s already set or not created by set_grammar"
           g1.n)
    ;
    g1.d <- g2.d;
    g1.cached <- g2.cached

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
                        | Some f -> set_grammar g (f p);
      );
      g),
  (fun f ->
    let all_set = ref false in
    while not !all_set do
      all_set := true;
      Hashtbl_eq.iter (fun p r ->
          if r.d = Tmp then
            (all_set := false; set_grammar r (f p))) tbl;
    done;
    is_set := Some f;
    )

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

let ne_lpos ?pk g1 = match g1 with
  | EFail -> EFail
  | _     -> ELPos(pk,g1)

let ne_rpos g1 = match g1 with
  | EFail -> EFail
  | _     -> ERPos(g1)

let ne_eval g1 = match g1 with
  | EFail -> EFail
  | _     -> EEval(g1)

let ne_test f g1 = match g1 with
  | EFail -> EFail
  | _     -> ETest(f,g1)

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
    if g.recursive || g.cached <> NoCache then ERef g else
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
    | Appl(g,f) -> fn g; List.fold_left (fun acc x ->
                             try f x :: acc
                             with Lex.NoParse | Give_up _ -> acc) [] g.e
    | Seq(g1,g2) -> fn g1; fn g2;
                    List.fold_left (fun acc x ->
                        List.fold_left (fun acc y ->
                            try y x :: acc
                            with NoParse | Give_up _ -> acc) acc g2.e)
                      [] g1.e
    | DSeq(g1,g2) -> fn g1;
                       List.fold_left (fun acc (x,x') ->
                           try
                             let g2 = g2 x in
                             fn g2;
                             List.fold_left (fun acc y ->
                                 try y x' :: acc
                                 with NoParse | Give_up _ -> acc) acc g2.e
                         with NoParse | Give_up _ -> acc)
                         [] g1.e
    | Rkey _        -> []
    | LPos(_,g1)    -> fn g1; List.fold_left (fun acc x ->
                                  try x Pos.phantom :: acc
                                  with Lex.NoParse | Give_up _ -> acc) [] g1.e
                       (* FIXME #14: Loose position *)
    | RPos(g1)      -> fn g1; List.fold_left (fun acc x ->
                                  try x Pos.phantom :: acc
                                  with Lex.NoParse | Give_up _ -> acc) [] g1.e
                       (* FIXME #14: Loose position *)
    | Layout(_,g,_) -> fn g; g.e
    | Eval(g)       -> fn g; g.e
    | Test(_,g)     -> fn g;
                       if g.e <> [] then
                         failwith "illegal test on grammar accepting empty";
                       []
    | Tmp           -> failwith
                         (Printf.sprintf
                            "grammar %s compiled before full definition"
                            g.n)

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
    | Rkey k    -> ERkey k
    | LPos(pk,g1) -> hn g1; ne_lpos ?pk (get g1)
    | RPos(g1) -> hn g1; ne_rpos (get g1)
    | Eval(g1) -> hn g1; ne_eval (get g1)
    | Test(f,g1) -> hn g1; ne_test f (get g1)
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

type 'b elr =
  | ENil : 'b elr
  | ECns : 'a key * 'b * 'b elr -> 'b elr

let rec map_elr : ('a -> 'b) -> 'a elr -> 'b elr =
  fun fn -> function
  | ENil -> ENil
  | ECns(k,g,r) -> ECns(k, fn g, map_elr fn r)

let rec merge_elr : 'a grammar elr -> 'a grammar elr -> 'a grammar elr =
  fun l1 l2 -> match (l1,l2) with
  | (ENil, l) -> l
  | (l, ENil) -> l
  | (ECns(k1,g1,l1'), ECns(k2,g2,l2')) ->
     let c = Assoc.compare k1 k2 in
     if c < 0 then ECns(k1,g1,merge_elr l1' l2)
     else if c > 0 then ECns(k2,g2,merge_elr l1 l2')
     else ECns(k1,alt [g1; g2],merge_elr l1' l2')

let rec merge_elr_mlr_right
        : type a. a key -> a grammar elr -> mlr_right -> mlr_right =
  fun k l1 l2 ->
  match l1 with
  | ENil            -> l2
  | ECns(k1,g1,l1') ->
     let _ = factor_empty g1 in
     RCns(k1, k, g1, merge_elr_mlr_right k l1' l2)

(** A type to store list of grammar keys *)
type ety = E : 'a key * bool * mlr Uf.t -> ety

let rec cat_left l1 l2 = match l1 with
  | LNil -> l2
  | LCns(k,g,r) -> LCns(k,g,cat_left r l2)

let rec cat_right l1 l2 = match l1 with
  | RNil -> l2
  | RCns(k,k',g,r) -> RCns(k,k',g,cat_right r l2)

let cat_lpos p1 p2 =
  match p1, p2 with
  | (Some _, _) -> p1
  | (_     , _) -> p2

let cat_mlr {left=l1;right=r1;lpos=p1;names=n1;keys=k1}
            {left=l2;right=r2;lpos=p2;names=n2;keys=k2} =
  { left  = cat_left l1 l2
  ; right = cat_right r1 r2
  ; lpos  = cat_lpos p1 p2
  ; names = n1 @ n2
  ; keys  = k1 @ k2
  }

let find_ety : type a. a key -> ety list -> bool = fun k l ->
  let open Assoc in
  let rec fn : ety list -> mlr Uf.t = function
    | [] -> raise Not_found
    | E (k',cached,x) :: l ->
       if cached then raise Not_found;
       match k.eq k'.tok with
       | Eq  -> x
       | NEq -> if List.memq (K k) (fst (Uf.find x)).keys then x else
                  let y = fn l in
                  Uf.union cat_mlr x y;
                  y
  in
  try
    let _ = fn l in
    true
  with
    Not_found -> false

type pos_ptr = mlr Uf.t

let get_pk : ety list -> pos_ptr option =
  function [] -> None
         | E(_,_,x) :: _ ->
            let ({lpos=p} as r, x) = Uf.find x in
            begin
              match p with
              | None ->
                 Uf.set_root x { r with lpos = Some (Assoc.new_key ()) };
              | Some _ -> ()
            end;
            Some x

(** Elimination of left recursion which is not supported by combinators *)
let elim_left_rec : type a. a grammar -> unit = fun g ->
  let rec fn : type b. ety list -> b grne -> b grne * b t elr =
    fun above g ->
      match g with
      | EFail | ETerm _ | EErr _ -> (g, ENil)
      | ESeq(g1,g2) ->
         let (g1, s1) = fn above g1 in
         (ne_seq g1 g2, map_elr (fun g -> seq g g2) s1)
      | EDSeq(g1,g2) ->
         let (g1, s1) = fn above g1 in
         (ne_dseq g1 g2, map_elr (fun g -> dseq g g2) s1)
      | EAlt(gs) ->
         let (gs,ss) = List.fold_left (fun (gs,ss) g ->
                              let (g,s) = fn above g in
                              ((g::gs), merge_elr s ss))
                            ([],ENil) gs
         in
         (ne_alt gs, ss)
      | ELr(_,_)    -> assert false
      | ERkey _     -> assert false
      | EAppl(g1,f) ->
         let (g1,s) = fn above g1 in
         (ne_appl g1 f, map_elr (fun g -> appl g f) s)
      | ERef(g) -> gn above g
      | EEval(g1) ->
         let (g1,s) = fn above g1 in
         (ne_eval g1, map_elr eval s)
      | ERPos(g1) ->
         let (g1, s1) = fn above g1 in
         (ne_rpos g1, map_elr rpos s1)
      | ELPos(Some _, _) -> assert false
      | ELPos(None, g1) ->
         let (g1, s1) = fn above g1 in
         let pk = get_pk above in
         (ne_lpos g1, map_elr (lpos ?pk) s1)
      | ETest(f, g1) ->
         let (g1, s1) = fn above g1 in
         (ne_test f g1, (match f with Before _ -> s1
                                    | After  _ -> map_elr (test f) s1))
      | ELayout(_,g1,_) ->
         let (_, s1) = fn above g1 in
         if s1 <> ENil then
           invalid_arg "fixpoint: left recursion under layout change";
         (g, ENil)
      | ETmp -> assert false

   and gn : type b. ety list -> b grammar -> b grne * b grammar elr =
     fun above g ->
       assert(g.phase >= EmptyRemoved);
       if find_ety g.k above then
         begin
           assert (g.phase >= LeftRecEliminated);
           (EFail, ECns (g.k, mkg (Rkey g.k), ENil))
         end
       else if g.phase >= LeftRecEliminated then
           begin
             (ERef g, ENil)
           end
         else
           begin
             g.phase <- LeftRecEliminated;
             let ptr = Uf.root { left = LNil; right = RNil; lpos = None
                                 ; names = [g.n]; keys = [K g.k] } in
             let cached = g.cached <> NoCache in
             let (g',s) = fn (E (g.k, cached, ptr) :: above) g.ne in
             if cached then assert (s = ENil);
             begin
               match s with
               | ENil ->
                  (ERef g, ENil) (* non left recursive *)
               | _ ->
                  let ({left; right; lpos; names; keys}, x) = Uf.find ptr in
                  let left = LCns(g.k,g',left) in
                  let right = merge_elr_mlr_right g.k s right in
                  Uf.set_root x {left; right; lpos; names; keys};
                  g.ne <- ELr(g.k, x);
                  match above with
                  | [] | E(_, true, _) :: _ ->
                     (ERef g, ENil)
                  | E (_, false, y) :: _ ->
                     let (_, x) = Uf.find ptr in
                     let (_, y) = Uf.find y in
                     if x != y then
                       begin
                         (ERef g, ENil)
                       end
                     else
                       begin
                         (EFail, ECns (g.k, mkg (Rkey g.k), ENil))
                       end

             end;
           end

   in
   if g.phase < LeftRecEliminated then ignore (gn [] g)

(** compute the characters set accepted at the beginning of the input *)
let first_charset : type a. a grne -> Charset.t = fun g ->

  let target = ref 0 in
  let changed = ref true in

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
    | ELr(_,x) ->
       let rec gn = function
         | LNil -> (false, Charset.empty)
         | LCns(_,g,l') ->
            let shift, s = fn g in
            let shift', s' = gn l' in
            (shift || shift', Charset.union s s')
       in
       gn (fst (Uf.find x)).left
    | ERkey _ -> (true, Charset.empty)
    | ERef g -> gn g
    | ERPos g -> fn g
    | ELPos (_,g) -> fn g
    | EEval(g) -> fn g
    | ETest (_,g) -> fn g
    | ELayout(_,g,cfg) -> if cfg.old_blanks_before
                             && not cfg.new_blanks_before
                          then fn g else (false, Charset.full)
    | ETmp -> assert false

  and gn : type a. a grammar -> bool * Charset.t = fun g ->
    assert (g.recursive || g.cached <> NoCache);
    match g.charset with
    | Some (n,c) when n >= !target -> (false, c)
    | _ ->
       let old = match g.charset with
         | None -> Charset.empty
         | Some (_, c) -> c
       in
       g.charset <- Some (!target, old);
       let (shift, r) = fn g.ne in
       assert (not shift);
       g.charset <- Some (!target, r);
       if r <> old then changed := true;
       (shift, r)
  in
  let res = ref Charset.empty in
  while !changed do
    incr target;
    changed := false;
    res := snd (fn g)
  done;
  !res

(** compilation of a grammar to combinators *)
let rec compile_ne : type a. a grne -> a Comb.t = fun g ->
  match g with
  | EFail -> Comb.fail
  | EErr m -> Comb.error m
  | ETerm(c) -> Comb.lexeme c.f
  | EAlt(gs) -> compile_alt gs
  | ESeq(g1,g2) -> Comb.seq (compile_ne g1) (compile false g2)
  | EDSeq(g1,g2) -> Comb.dseq (compile_ne g1)
                      (fun x -> compile false (g2 x))
  | EAppl(g1,f) -> Comb.app (compile_ne g1) f
  | ELr(k,x) ->
     begin
       let (r, _) = Uf.find x in
       let rec fn : type a. a key -> a grne list -> mlr_left -> a grne list =
         fun k acc l -> match l with
         | LNil -> acc
         | LCns(k',g,l) ->
            let open Assoc in
            match k.eq k'.tok with
            | Eq  -> fn k (g::acc) l
            | NEq -> assert false
       in
       match r.right with
       | RNil-> assert false
       | RCns(k0,k',g,RNil) ->
          begin
            let open Assoc in
            assert (match k.eq k0.tok with Eq -> true | NEq -> false);
            match k.eq k'.tok with
            | NEq -> assert false
            | Eq  ->
               let gs = fn k [] r.left in
               let left = compile_alt gs in
               match r.lpos with
               | None -> Comb.lr left k (compile_ne g.ne)
               | Some pk -> Comb.lr_pos left k pk (compile_ne g.ne)
          end
       | _ ->
          let rec fn = function
            | LNil -> Comb.LNil
            | LCns(k,g,l) -> Comb.LCns(k,first_charset g,
                                       compile_ne g, fn l)
          in
          let c = ref 0 in
          let rec gn = function
            | RNil -> Comb.RNil
            | RCns(k,k',g,l) -> incr c;
                                Comb.RCns(k,k',first_charset g.ne,
                                          compile_ne g.ne, gn l)
          in
          Comb.mlr ?lpos:r.lpos (fn r.left) (gn r.right) k
     end
  | ERkey k -> Comb.read_tbl k
  | ERef g -> compile true g
  | ERPos(g) -> Comb.right_pos (compile_ne g)
  | ELPos(None,g) -> Comb.left_pos (compile_ne g)
  | ELPos(Some r,g) ->
     begin
       let (r, _) = Uf.find r in
       match r.right, r.lpos with
       | RNil, _    -> Comb.left_pos (compile_ne g)
       | _, None    -> assert false
       | _, Some pk -> Comb.read_pos pk (compile_ne g)
     end
  | EEval(g) -> Comb.eval (compile_ne g)
  | ETest(Before f,g) -> Comb.test_before f (compile_ne g)
  | ETest(After f,g) -> Comb.test_after f (compile_ne g)
  | ELayout(b,g,cfg) -> Comb.change_layout ~config:cfg b (compile_ne g)
  | ETmp -> assert false

 and compile_alt : type a. a grne list -> a Comb.t = fun gs ->
  let l = List.map (fun g -> (first_charset g, compile_ne g)) gs in
  Comb.alts l

 and compile : type a. bool -> a grammar -> a Comb.t =
  fun ne g ->
    factor_empty g;
    elim_left_rec g;
    assert (g.phase >= LeftRecEliminated);
    (* NOTE: g.recursive is not enough here, with mutual recursion; after
           left rec elimination, the loop may be detected at other position
           in the tree *)
    let get g =
      let cne = g.compiled in
        if g.recursive || g.phase = Compiling then
        Comb.deref cne
      else !cne
    in
    let cne =
      match g.phase with
      | Compiled | Compiling -> get g
      | _ ->
         g.phase <- Compiling;
         let cne = compile_ne g.ne in
         g.phase <- Compiled;
         let cne =
           match g.cached with
           | NoCache -> cne
           | Cache m -> Comb.cache ?merge:m cne
         in
         g.compiled := cne;
         get g
    in
    let c = match if ne then [] else g.e with
      | [] ->
         if g.ne = EFail then Comb.fail else cne
      | [x] ->
         if g.ne = EFail then Comb.empty x
         else Comb.option x (first_charset g.ne) cne
      | l ->
         let ce =
           Comb.alts (List.map (fun x -> Charset.full, Comb.empty x) l)
         in
         Comb.alt Charset.full ce (first_charset g.ne) cne
    in
    c

let compile g = compile false g

let grammar_name g = g.n

(** functions to actually use the parser *)
let add_eof g = seq g (term (eof (fun x -> x)))

(* NOTE: cs with blank_after = false makes no sense ? *)
let partial_parse_buffer
    : type a. a t -> Blank.t -> ?blank_after:bool
                  -> Lex.buf -> Lex.pos -> a * Lex.buf * Lex.pos =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let g = compile g in
    Comb.partial_parse_buffer g blank_fun ~blank_after buf0 col0

let parse_buffer
    : type a. a t -> Blank.t -> Lex.buf -> Lex.pos -> a =
  fun g blank_fun buf col ->
    let g = add_eof g in
    let (v,_,_) = partial_parse_buffer g blank_fun buf col in v

let parse_all_buffer
    : type a. a t -> Blank.t -> Lex.buf -> Lex.pos -> a list =
  fun g blank_fun buf0 col0 ->
    let g = compile (add_eof g) in
    Comb.parse_all_buffer g blank_fun buf0 col0

let parse_string
    : type a. ?utf8:Utf8.context -> ?filename:string ->
              a t -> Blank.t -> string -> a =
  fun ?(utf8=Utf8.ASCII) ?filename g b s ->
    parse_buffer g b (Input.from_string ~utf8 ?filename s) Input.init_pos

let parse_channel
    : type a. ?utf8:Utf8.context -> ?filename:string ->
              a t -> Blank.t -> in_channel -> a =
  fun ?(utf8=Utf8.ASCII) ?filename g b ic ->
    parse_buffer g b (Input.from_channel ~utf8 ?filename ic) Input.init_pos

let parse_file ?(utf8=Utf8.ASCII) g b filename =
    let ic = open_in filename in
    parse_channel ~utf8 ~filename g b ic

let lpos ?name g = lpos ?name ?pk:None g
