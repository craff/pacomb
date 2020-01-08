open Lex
module Uf = UnionFind

let _ = Printexc.record_backtrace true
let _ = Sys.catch_break true

type 'a cache =
  NoCache : 'a cache
| Cache   : ('a -> 'a -> 'a) option -> 'a cache

type name_kind = Created | Inherited | Given

type name = string * name_kind

let best (_,k1 as n1) (s2,k2 as n2) =
  match (k1,k2) with
  | (Given    , _        ) -> n1
  | (_        , Given    ) -> (s2, Inherited)
  | (Inherited, _        ) -> n1
  | (_        , Inherited) -> n2
  | (Created  , Created  ) -> n1

let gen_name =
  let c = ref 0 in
  (fun name ->
    match name with
    | None   -> incr c; (Printf.sprintf "?%d" !c, Created)
    | Some n -> n)

let created = function
  | None -> None
  | Some n -> Some(n,Given)

let created2 name upname = match name with
  | Some n -> Some(n,Given)
  | None -> if snd upname <> Created then Some (fst upname, Inherited)
            else None


(** Type for a grammar *)
type 'a grammar =
  { mutable d : 'a grdf   (** the definition of the grammar *)
  ; mutable n : name      (** name of the grammar *)
  ; k : 'a Comb.key      (** a key used mainly to detect recursion *)
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
           ; lpos : bool
           ; keys : (Assoc.any_key * string) list }

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
   | Lazy : 'a t -> 'a lazy_t grdf
   | Frce : 'a lazy_t t -> 'a grdf
   | UMrg : 'a list t -> 'a grdf           (** unmerge *)
   | Tmp  : 'a grdf                        (** used as initial value for
                                               recursive grammar. *)

 and 'a test =
   | Before of (Lex.buf -> Lex.idx -> Lex.buf -> Lex.idx -> bool)
   | After of ('a -> Lex.buf -> Lex.idx -> Lex.buf -> Lex.idx -> bool)

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
   | ELazy : 'a grne -> 'a lazy_t grne
   | EFrce : 'a lazy_t grne -> 'a grne
   | EUMrg : 'a list grne -> 'a grne
   | ETmp  : 'a grne
   (** only new constructor, introduced by elimination of left recursion.
       the key give the grammar we actually parse and the mlr information
       is stored in a union find data structure to allow merging grammar
       when we discover they are mutually left dependant *)
   | ELr   : 'a key * mlr Uf.t -> 'a grne

let rec eq : type a b.a grammar -> b grammar -> (a, b) Assoc.eq =
  fun g1 g2 ->
    let open Assoc in
    match g1.k.eq g2.k.tok with
    | Eq -> Eq
    | NEq ->
       begin
         match g1.d, g2.d with
         | Term t1, Term t2 -> Lex.eq t1 t2
         | Seq(g11,g12), Seq(g21,g22) ->
            begin
              match eq g11 g21, eq g12 g22 with
              | Eq, Eq -> Eq
              | _ -> NEq
            end
         | LPos(None,g1), LPos(None,g2) ->
            begin
              match eq g1 g2 with
              | Eq -> Eq | _ -> NEq
            end
         | RPos(g1), RPos(g2) ->
            begin
              match eq g1 g2 with
              | Eq -> Eq | _ -> NEq
            end
         | UMrg(g1), UMrg(g2) ->
            begin
              match eq g1 g2 with
              | Eq -> Eq | _ -> NEq
            end
         | _ -> NEq
       end

(** grammar renaming *)
let give_name n g = g.n <- (n, Given); g

(** helper to construct the initial ['a grammar] record *)
let mkg : ?name:name -> ?recursive:bool -> ?cached:'a cache ->
          'a grdf -> 'a grammar =
  fun ?name ?(recursive=false) ?(cached=NoCache) d ->
    let k = Assoc.new_key () in
    { e = []; d; n = gen_name name; k; recursive; cached
    ; compiled = ref Comb.assert_false
    ; charset = None; phase = Defined; ne = ETmp }

(** cache is added as information in the grammar record because when
    a grammar is cached, elimination of left recursion is useless *)
let cache ?name ?merge g =
  let name = gen_name (created name) in
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

module AssocLr = Assoc.Make (struct type 'a data = 'a grne list * 'a t list end)

type prio = P_Atom | P_Seq | P_Alt

type print_ast_aux =
  | PFail
  | PErr of string
  | PEmpty
  | PTerm of string
  | PAlt of print_ast list
  | PSeq of print_ast * print_ast
  | PDSeq of print_ast (* lost the second part! *)
  | PTrans of string * print_ast
  | PRkey of string
  | PLr of print_lr

and print_lr = string * (string * print_ast) list

and print_ast = { name : name
                ; mutable ast : print_ast_aux
                ; is_rec : bool  }

module AAssoc = Assoc.Make(struct type 'a data = print_ast end)

let print_ast_of_df : type a. a grammar -> print_ast = fun x ->
  let adone = ref AAssoc.empty in
  let rec fn : type a. a grdf -> print_ast_aux = function
    | Fail         -> PFail
    | Err m        -> PErr m
    | Empty _      -> PEmpty
    | Term t       -> PTerm (t.n)
    | Alt(gs)      -> PAlt (List.map gn gs)
    | Seq(g1,g2)   -> PSeq(gn g1, gn g2)
    | DSeq(g1,_)   -> PDSeq(gn g1)
    | Rkey _       -> assert false
    | Appl(g,_)    -> PTrans("appl",gn g)
    | RPos(g)      -> PTrans("rpos",gn g)
    | LPos(_,g)    -> PTrans("lpos",gn g)
    | Test(_,g)    -> PTrans("test",gn g)
    | Lazy(g)      -> PTrans("lazy",gn g)
    | Frce(g)      -> PTrans("frce",gn g)
    | Layout(_,g,_)-> PTrans("blks",gn g)
    | UMrg(g)      -> PTrans("umgrl",gn g)
    | Tmp          -> PErr "TMP in grammar"

  and gn : type a. a t -> print_ast = fun g ->
    if g.recursive then
      try AAssoc.find g.k !adone
      with Not_found ->
        let r = { name = g.n; ast = PFail; is_rec = true } in
        adone := AAssoc.add g.k r !adone;
        r.ast <- fn g.d;
        r
    else
      { name = g.n; ast = fn g.d; is_rec = false }

  in gn x

let print_ast_of_ne : type a. a grammar -> print_ast = fun x ->
  let adone = ref AAssoc.empty in
  let keys_name = ref [] in
  let get_name k = try List.assq (Assoc.K k) !keys_name with Not_found -> "?" in
  let add_name (k:Assoc.any_key) name =
    if not (List.mem_assq k !keys_name)
    then keys_name := (k, name) :: !keys_name
  in
  let rec fn : type a. a grne -> print_ast_aux = function
    | EFail         -> PFail
    | EErr m        -> PErr m
    | ETerm t       -> PTerm (t.n)
    | EAlt(gs)      -> PAlt (List.map fn' gs)
    | ESeq(g1,g2)   -> PSeq(fn' g1, gn g2)
    | EDSeq(g1,_)   -> PDSeq(fn' g1)
    | ERkey k       -> PRkey (get_name k)
    | ELr(k,uf)     -> PLr(hn k uf)
    | ERef g        -> PTrans("ref" ,gn g)
    | EAppl(g,_)    -> PTrans("appl",fn' g)
    | ERPos(g)      -> PTrans("rpos",fn' g)
    | ELPos(_,g)    -> PTrans("lpos",fn' g)
    | ETest(_,g)    -> PTrans("test",fn' g)
    | ELazy(g)      -> PTrans("lazy",fn' g)
    | EFrce(g)      -> PTrans("frce",fn' g)
    | ELayout(_,g,_)-> PTrans("blks",fn' g)
    | EUMrg(g)      -> PTrans("umgr",fn' g)
    | ETmp          -> PErr "TMP in grammar"

  and no_name ast =
    { name = gen_name None; ast; is_rec = false }

  and fn' : type a. a grne -> print_ast = fun g -> no_name (fn g)

  and gn : type a. a t -> print_ast = fun g ->
    if g.recursive then
      try AAssoc.find g.k !adone
      with Not_found ->
        let r = { name = g.n; ast = PFail; is_rec = true } in
        adone := AAssoc.add g.k r !adone;
        let ast = fn g.ne in
        let ast = if g.e <> [] then PAlt[no_name PEmpty;no_name ast] else ast in
        r.ast <- ast;
        r
    else
      let ast = fn g.ne in
      let ast = if g.e <> [] then PAlt[no_name PEmpty;no_name ast] else ast in
      { name = g.n; ast; is_rec = false }

  and hn : type a. a key -> mlr Uf.t -> print_lr =
    fun k lr ->
    let ({left; right; keys; _}, _) = Uf.find lr in
    List.iter (fun (k, name) -> add_name k (name ^ "_lr")) keys;
    let matrix  = ref AssocLr.empty in
    let get_matrix k = try AssocLr.find k !matrix with Not_found -> ([], []) in
    let add_matrix k c = matrix := AssocLr.replace k c !matrix in
    let rec collect_left = function
      | LNil -> ()
      | LCns(k,g,l) ->
         let (left,right) = get_matrix k in
         add_matrix k (g::left,right);
         collect_left l
    in
    let rec collect_right = function
      | RNil -> ()
      | RCns(_,k,g,l) ->
         let (left,right) = get_matrix k in
         add_matrix k (left,g::right);
         collect_right l
    in
    collect_left left;
    collect_right right;
    let print_one (left,right) =
      no_name (PAlt (List.map fn' left @ List.map gn right))
    in
    let print_matrix () =
      let res = ref [] in
      AssocLr.iter { f = fun k x ->
            let g = print_one x in
            res := (get_name k,g) :: !res
          } !matrix;
      !res
    in
    let name = get_name k in
    (name, print_matrix ())

  in gn x

let print_ast ?(no_other=false) ch s =
  let adone = ref [] in
  let todo = ref [] in

  let do_def g =
    g.is_rec ||
      (snd g.name = Given && match g.ast with PTerm _ -> false | _ -> true)
  in

  let rec print_ast : prio -> out_channel -> print_ast_aux -> unit =
    fun prio ch g ->
    let pr x = Printf.fprintf ch x in
    let pv x = print x in
    match g with
    | PEmpty      -> pr "()"
    | PFail       -> pr "0"
    | PErr m      -> pr "0(%s)" m
    | PTerm t     -> pr "%s" t
    | PAlt(gs)    -> pr (if prio < P_Alt then "(%a)" else "%a")
                        (prl (pv P_Alt) " | ") gs
    | PSeq(g1,g2) -> pr (if prio < P_Seq then "(%a %a)" else "%a %a")
                         (pv P_Atom) g1 (pv P_Seq) g2
    | PDSeq(g1)   -> pr (if prio < P_Seq then "(%a ...)" else "%a ...")
                       (pv P_Atom) g1
    | PLr(k,lr)   -> print_lr (k, lr) ch
    | PRkey k     -> pr "%s" k
    | PTrans(_,g) -> pv prio ch g

  and print_lr : print_lr -> out_channel -> unit =
    fun (name, m) ch ->
    let print_one ch (name,g) =
      Printf.fprintf ch "\n  %s ::= %a" name (print P_Alt) g
    in
    let print_matrix ch m =
      List.iter (print_one ch) m
    in
    Printf.fprintf ch "%s from %a" name print_matrix m

  and print : ?forced:bool -> prio -> out_channel -> print_ast -> unit =
    fun ?(forced=false) prio ch g ->
    let rec fn : name -> print_ast -> unit = fun name g ->
      let name = best name g.name in
      if snd name = Given && snd g.name = Given
      then print_aux forced name prio ch g
      else (match g.ast with
            | PTrans(_,g) -> fn name g
            | _ -> print_aux forced name prio ch g)
    in
    fn ("????",Created) g

  and print_aux : bool -> name -> prio -> out_channel -> print_ast -> unit =
    fun forced name0 prio ch g ->
    let name =
      if snd name0 <> Created then fst name0
      else fst g.name
    in
    let pr x = Printf.fprintf ch x in
    let pg x = print_ast x in
    if List.memq g !adone && not forced then Printf.fprintf ch "%s" name
    else if (snd name0 <> Created || do_def g) && not forced then
      begin
        adone := g :: !adone;
        todo := g :: !todo;
        pr "%s" name
      end
    else if not forced then
      begin
        pg prio ch g.ast
      end
    else
      begin
        let rec fn g = match g.ast with
          | PTerm _ -> ()
          | PTrans(_,g) -> fn g
          | _ -> Printf.fprintf ch "%s ::= %a\n" name (pg prio) g.ast
        in fn g
      end

  in
  todo := s :: !todo;
  adone := s :: !adone;
  while !todo != [] do
    match !todo with
      [] -> assert false
    | s::l ->
       todo := l;
       print ~forced:true P_Alt ch s;
       if no_other then todo := []
  done

let print_grammar
    : type a. ?no_other:bool -> ?def:bool -> out_channel -> a grammar -> unit =
  fun ?(no_other=false) ?(def=true) ch g ->
    let g = if def then print_ast_of_df g else print_ast_of_ne g in
    print_ast ~no_other ch g

let print_grne : type a. out_channel -> a grne -> unit =
  fun ch ne ->
  let g =
    { e = []
    ; d = Tmp
    ; n = ("TOP", Given)
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

let _ = print_grne (* to avoid warning *)

(** Interface to constructors.  propagate Fail because it is tested by
   elim_left_rec for the Lr suffix *)
let fail ?name () = let name = created name in mkg ?name Fail

let empty ?name x = let name = created name in mkg ?name (Empty x)

let cond ?name b = if b then empty ?name () else fail ?name ()

let term ?name (x) =
  if accept_empty x then
    invalid_arg (Printf.sprintf "term: empty terminals %s" x.n);
  let name = created2 name (x.Lex.n,Inherited) in
  mkg ?name (Term x)

let appl : type a b. ?name:name -> a t -> (a -> b) -> b t =
  fun ?name g f ->
  let name = gen_name name in
  let df, name =
    match g.d with
    | Fail         -> Fail, best name g.n
    | Appl(g',f')  -> Appl(g',(fun x -> f (f' x))), best name g.n
    | Empty x      -> Empty (f x), best name g.n
    (* | Seq(g1,g2)   -> Seq(g1,appl g2 (fun h x -> f (h x))) *)
    (* | LPos(None,g) -> LPos(None, appl g (fun h x -> f (h x))) *)
    (* | RPos(g)      -> RPos(appl g (fun h x -> f (h x))) *)
    | _            -> Appl(g,f), name
  in
  mkg ~name df

let seq ?name g1 g2 =
  let name = created name in
  match g1.d,g2.d with
  | Fail, _    -> mkg ?name Fail
  | _, Fail    -> mkg ?name Fail
  | Empty x, _ -> appl ?name g2 (fun y -> y x)
  | _, Empty y -> appl ?name g1 y
  | _          -> mkg ?name (Seq(g1,g2))

let dseq ?name g1 g2 =
  let name = created name in
  mkg ?name (if g1.d = Fail then Fail else DSeq(g1,g2))

let lpos ?name ?pk g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else LPos(pk,g))

let rpos ?name g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else RPos(g))

type (_,_) plist =
  | PE : ('a, 'a) plist
  | PC : 'a t * ('b, 'c) plist -> ('a -> 'b,'c) plist
  | PL : ('b, 'c) plist -> (Pos.t -> 'b,'c) plist
  | PR : ('b, 'c) plist -> (Pos.t -> 'b,'c) plist

let rec alt : type a. ?name:name -> a t list -> a t = fun ?name l ->
  let l = List.filter (fun g -> g.d <> Fail) l in
  let l = List.map (function { n = (_,(Given|Inherited))
                             ; d = Alt(ls) } -> ls | x -> [x]) l in
  let l = List.flatten l in
  match l with
  | [] -> fail ()
  | [g] -> g
  | l   -> let name = gen_name name in mkg ~name (Alt( left_factorise l))

and left_factorise : type a.a t list -> a t list = fun l ->
  let recompose : type a b.(a,b) plist -> a t -> a t -> b t =
    fun acc r1 r2 ->
      let rec fn : type a b. (a,b) plist -> a t -> b t =
        fun acc r -> match acc with
        | PE      -> r
        | PC(g,l) -> fn l (seq g r)
        | PL l    -> fn l (lpos r)
        | PR l    -> fn l (rpos r)
      in
      let rec test : type a b. (a, b) plist -> unit =
        function
        | PC _ -> ()
        | PL l -> test l
        | PR l -> test l
        | PE -> raise Not_found
      in
      test acc;
      fn acc (alt [r1; r2])
  in
  let rec common_prefix
          : type a b. (Assoc.any_key * Assoc.any_key) list -> (a,b) plist ->
                 a t -> a t -> b t
    = fun adone acc g1 g2 ->
    if List.exists
         (fun (Assoc.K ga, Assoc.K gb) ->
           match ga.eq g1.k.tok, gb.eq g2.k.tok
           with Assoc.Eq, Assoc.Eq -> true
              | _ -> false)
         adone
    then raise Not_found;
    let adone = (Assoc.K g1.k,Assoc.K g2.k) :: adone in
    let common_prefix acc g1 g2 = common_prefix adone acc g1 g2 in
    match (g1.d,g2.d) with
    | LPos(None,g1), LPos(None,g2) -> common_prefix (PL acc) g1 g2
    | RPos(g1), RPos(g2) -> common_prefix (PR acc) g1 g2
    | LPos(None,g1), _ -> common_prefix (PL acc) g1 (appl g2 (fun f _ -> f))
    | RPos(g1), _ -> common_prefix (PR acc) g1 (appl g2 (fun f _ -> f))
    | _, LPos(None,g2) -> common_prefix (PL acc) (appl g1 (fun f _ -> f)) g2
    | _, RPos(g2) -> common_prefix (PR acc) (appl g1 (fun f _ -> f)) g2
    | Seq(l1, r1), Seq(l2,r2) ->
       begin
         match eq l1 l2 with
         | Assoc.Eq -> common_prefix (PC(l1,acc)) r1 r2
         | _  -> recompose acc g1 g2
       end
    | Seq(l1, r1), Appl(g2',f) ->
       begin
         match eq l1 g2' with
         | Assoc.Eq -> recompose (PC(l1,acc)) r1 (empty f)
         | _  -> recompose acc g1 g2
       end
    | Seq(l1, r1), _ ->
       begin
         match eq l1 g2 with
         | Assoc.Eq -> recompose (PC(l1,acc)) r1 (empty (fun x -> x))
         | _  -> recompose acc g1 g2
       end
    | Appl(g1',f), Seq(l2, r2) ->
       begin
         match eq l2 g1' with
         | Assoc.Eq -> recompose (PC(l2,acc)) (empty f) r2
         | _  -> recompose acc g1 g2
       end
    | _, Seq(l2, r2) ->
       begin
         match eq l2 g1 with
         | Assoc.Eq -> recompose (PC(l2,acc)) (empty (fun x -> x)) r2
         | _  -> recompose acc g1 g2
       end
    | _ -> recompose acc g1 g2
  in
  let rec factor l = match l with
    | [] -> l
    | g::l ->
       let rec fn acc' acc = match acc with
         | [] -> List.rev (g :: acc')
         | g'::acc ->
            try
              let g = common_prefix [] PE g g' in
              List.rev_append acc' (g :: acc)
            with
              Not_found -> fn (g'::acc') acc
       in
       fn [] (factor l)
  in
  factor l

let seq1 ?name g1 g2 = seq ?name g1 (appl g2 (fun _ x -> x))

let seq2 ?name g1 g2 = seq ?name g1 (appl g2 (fun x _ -> x))

let seq_rpos ?name g1 g2 =
  seq ?name (rpos (appl g1 (fun x rpos -> (x, rpos)))) g2

let seq_lpos ?name g1 g2 =
  seq ?name (lpos (appl g1 (fun x lpos -> (lpos, x)))) g2

let seq_pos ?name g1 g2 =
  seq ?name (lpos (rpos (appl g1 (fun x rpos lpos -> (lpos, x, rpos)))))
    g2

let error ?name m =
  let name = created name in
  mkg ?name (Err m)

let unmerge ?name g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else UMrg(g))

let test ?name f g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else Test(f,g))

let test_before ?name f g = test ?name (Before f) g

let test_after ?name f g = test ?name (After f) g

let no_blank_before ?name g =
  let fn b1 c1 b2 c2 = Input.buffer_equal b1 b2 && c1 = c2 in
  test_before ?name  fn g

let no_blank_after ?name g =
  let fn _ b1 c1 b2 c2 = Input.buffer_equal b1 b2 && c1 = c2 in
  test_after ?name  fn g

let lazy_ ?name g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else Lazy(g))

let force ?name g =
  let name = created2 name g.n in
  mkg ?name (if g.d = Fail then Fail else Frce(g))

let layout ?name ?(config=Blank.default_layout_config) b g =
  let name = created2 name g.n in
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
           (fst g1.n))
    ;
    g1.d <- g2.d;
    g1.cached <- g2.cached

let fixpoint : type a. ?name:string -> (a grammar -> a grammar) -> a grammar =
  fun ?name g ->
    let g0 = declare_grammar (gen_name (created name)) in
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
  let name = created name in
  alt ?name [appl g (fun x -> Some x); empty None]

let default_option : ?name:string -> 'a -> 'a grammar -> 'a grammar =
  fun ?name d g ->
    let name = created name in
    alt ?name [g; empty d]

let star : ?name:string -> 'a grammar -> 'a list grammar = fun ?name g ->
  let name = created2 name (fst g.n ^ "*", snd g.n) in
  appl ?name (fixpoint (fun r ->
            alt [empty [];
                 seq r (appl g (fun x l -> x::l))])) List.rev

let plus : ?name:string -> 'a grammar -> 'a list grammar = fun ?name g ->
  let name = created2 name (fst g.n ^ "+", snd g.n) in
  appl ?name (fixpoint (fun r ->
            alt [appl g (fun x -> [x]);
                 seq r (appl g (fun x l -> x::l))])) List.rev

let plus_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar =
  fun ?name sep g ->
    let name = created2 name (fst g.n ^ "+" ^ fst sep.n, snd g.n) in
    appl ?name
      (fixpoint (fun r ->
           alt [appl g (fun x -> [x]);
                seq r (seq sep (appl g (fun x _ l -> x::l)))])) List.rev

let star_sep : ?name:string -> 'b grammar -> 'a grammar -> 'a list grammar =
  fun ?name sep g ->
    let name = created2 name (fst g.n ^ "*" ^ fst sep.n, snd g.n) in
    alt ?name [empty []; plus_sep sep g]

(** a function to defined idxed grammars *)
let grammar_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = Hashtbl_eq.create 8 in
  let is_set = ref None in
  (fun p ->
    try Hashtbl_eq.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p, Given) in
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
  | l   -> EAlt l

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

let ne_unmerge g1 = match g1 with
  | EFail -> EFail
  | _     -> EUMrg(g1)

let ne_test f g1 = match g1 with
  | EFail -> EFail
  | _     -> ETest(f,g1)

let ne_lazy g1 = match g1 with
  | EFail -> EFail
  | _     -> ELazy(g1)

let ne_force g1 = match g1 with
  | EFail -> EFail
  | _     -> EFrce(g1)

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
                                  try x Input.phantom_spos :: acc
                                  with Lex.NoParse | Give_up _ -> acc) [] g1.e
                       (* FIXME #14: Loose position *)
    | RPos(g1)      -> fn g1; List.fold_left (fun acc x ->
                                  try x Input.phantom_spos :: acc
                                  with Lex.NoParse | Give_up _ -> acc) [] g1.e
                       (* FIXME #14: Loose position *)
    | Layout(_,g,_) -> fn g; g.e
    | UMrg(g)       -> fn g; List.flatten g.e
    | Lazy(g)       -> fn g; List.map Lazy.from_val g.e
    | Frce(g)       -> fn g; List.fold_left (fun acc x ->
                                 try Lazy.force x :: acc
                                 with Lex.NoParse | Give_up _ -> acc) [] g.e
    | Test(_,g)     -> fn g;
                       if g.e <> [] then
                         failwith "illegal test on grammar accepting empty";
                       []
    | Tmp           -> failwith
                         (Printf.sprintf
                            "grammar %s compiled before full definition"
                            (fst g.n))

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
    | UMrg(g1) -> hn g1; ne_unmerge (get g1)
    | Lazy(g1) -> hn g1; ne_lazy (get g1)
    | Frce(g1) -> hn g1; ne_force (get g1)
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

let cat_mlr {left=l1;right=r1;lpos=p1;keys=k1}
            {left=l2;right=r2;lpos=p2;keys=k2} =
  { left  = cat_left l1 l2
  ; right = cat_right r1 r2
  ; lpos  = p1 || p2
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
       | NEq -> if List.mem_assq (K k) (fst (Uf.find x)).keys then x else
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
              | false ->
                 Uf.set_root x { r with lpos = true };
              | true -> ()
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
      | EUMrg(g1) ->
         let (g1,s) = fn above g1 in
         (ne_unmerge g1, map_elr unmerge s)
      | ELazy(g1) ->
         let (g1,s) = fn above g1 in
         (ne_lazy g1, map_elr lazy_ s)
      | EFrce(g1) ->
         let (g1,s) = fn above g1 in
         (ne_force g1, map_elr force s)
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
             let ptr = Uf.root { left = LNil; right = RNil; lpos = false
                                 ; keys = [(K g.k,fst g.n)] } in
             let cached = g.cached <> NoCache in
             let (g',s) = fn (E (g.k, cached, ptr) :: above) g.ne in
             if cached then assert (s = ENil);
             begin
               match s with
               | ENil ->
                  (ERef g, ENil) (* non left recursive *)
               | _ ->
                  let ({left; right; lpos; keys}, x) = Uf.find ptr in
                  let left = LCns(g.k,g',left) in
                  let right = merge_elr_mlr_right g.k s right in
                  Uf.set_root x {left; right; lpos; keys};
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
    | EErr _ -> (false, Charset.empty)
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
    | ERkey _ -> (true, Charset.full)
    | ERef g -> gn g
    | ERPos g -> fn g
    | ELPos (_,g) -> fn g
    | EUMrg(g) -> fn g
    | ELazy(g) -> fn g
    | EFrce(g) -> fn g
    | ETest (_,g) -> fn g
    | ELayout(_,g,cfg) -> if cfg.old_blanks_before
                             && not cfg.new_blanks_before
                          then fn g else (false, Charset.full)
    | ETmp -> assert false

  and gn : type a. a grammar -> bool * Charset.t = fun g ->
    assert (g.phase >= EmptyRemoved);
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
       if r <> old then
         begin
           g.charset <- Some (!target, r);
           changed := true;
         end;
       (shift, r)
  in
  let res = ref Charset.empty in
  while !changed do
    incr target;
    changed := false;
    let (_, cs) = fn g in
    res := cs
  done;
  !res

(** compilation of a grammar to combinators *)
let rec compile_ne : type a. a grne -> a Comb.t = fun g ->
  match g with
  | EFail -> Comb.fail
  | EErr m -> Comb.error m
  | ETerm(c) -> Comb.lexeme c.f
  | EAlt(gs) -> compile_alt gs
  | ESeq(g1,g2) -> Comb.seq (compile_ne g1)
                     (if g2.e = [] then first_charset g2.ne else Charset.full)
                     (compile false g2)
  | EDSeq(g1,g2) ->
     let memo = Hashtbl_eq.create 16 in
     Comb.dseq (compile_ne g1)
       (fun x -> try Hashtbl_eq.find memo x with Not_found ->
                   let cg = compile false (g2 x) in
                   Hashtbl_eq.add memo x cg; cg)
  | EAppl(g1,f) -> Comb.appl (compile_ne g1) f
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
               let ne = compile_ne g.ne in
               let cs = first_charset g.ne in
               match r.lpos with
               | false -> Comb.lr left k cs ne
               | true  -> Comb.lr_pos left k cs ne
          end
       | _ ->
          let rec fn = function
            | LNil -> Comb.LNil
            | LCns(k,g,l) ->
               let ne = compile_ne g in
               let cs = first_charset g in
               Comb.LCns(k, cs, ne, fn l)
          in
          let c = ref 0 in
          let rec gn = function
            | RNil -> Comb.RNil
            | RCns(k,k',g,l) -> incr c;
                                let ne = compile_ne g.ne in
                                let cs = first_charset g.ne in
                                Comb.RCns(k,k',cs,ne,gn l)
          in
          Comb.mlr ~lpos:r.lpos (fn r.left) (gn r.right) k
     end
  | ERkey k -> Comb.read_tbl k
  | ERef g -> compile true g
  | ERPos(g) -> Comb.right_pos (compile_ne g)
  | ELPos(None,g) -> Comb.left_pos (compile_ne g)
  | ELPos(Some r,g) ->
     begin
       let (r, _) = Uf.find r in
       match r.right, r.lpos with
       | RNil, _  -> Comb.left_pos (compile_ne g)
       | _, false -> assert false
       | _, true  -> Comb.read_pos (compile_ne g)
     end
  | EUMrg(g) -> Comb.unmerge (compile_ne g)
  | ELazy(g) -> Comb.lazy_ (compile_ne g)
  | EFrce(g) -> Comb.force (compile_ne g)
  | ETest(Before f,g) -> Comb.test_before f (compile_ne g)
  | ETest(After f,g) -> Comb.test_after f (compile_ne g)
  | ELayout(b,g,cfg) -> Comb.change_layout ~config:cfg b (compile_ne g)
  | ETmp -> assert false

 and compile_alt : type a. a grne list -> a Comb.t = fun gs ->
   let l = List.map (fun g -> (first_charset g, compile_ne g)) gs in
   Comb.alt l

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
      | l ->
         let acc = (first_charset g.ne, cne) in
         let (_, c) =
           List.fold_left (fun (cs, g) e ->
               (Charset.full, Comb.option e cs g)) acc l
         in
         c
    in
    c

let compile g = compile false g

let grammar_name g = fst g.n

(** functions to actually use the parser *)
let add_eof g = seq g (appl (term (eof ())) (fun _ x -> x))

(* NOTE: cs with blank_after = false makes no sense ? *)
let partial_parse_buffer
    : type a. a t -> Blank.t -> ?blank_after:bool
                  -> Lex.buf -> Lex.idx -> a * Lex.buf * Lex.idx =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let g = compile g in
    Comb.partial_parse_buffer g blank_fun ~blank_after buf0 col0

let parse_buffer
    : type a. a t -> Blank.t -> Lex.buf -> Lex.idx -> a =
  fun g blank_fun buf col ->
    let g = add_eof g in
    let (v,_,_) = partial_parse_buffer g blank_fun buf col in v

let parse_all_buffer
    : type a. a t -> Blank.t -> Lex.buf -> Lex.idx -> a list =
  fun g blank_fun buf0 col0 ->
    let g = compile (add_eof g) in
    Comb.parse_all_buffer g blank_fun buf0 col0

let parse_string
    : type a. ?utf8:Utf8.context -> ?filename:string ->
              a t -> Blank.t -> string -> a =
  fun ?(utf8=Utf8.ASCII) ?filename g b s ->
    parse_buffer g b (Input.from_string ~utf8 ?filename s) Input.init_idx

let parse_channel
    : type a. ?utf8:Utf8.context -> ?filename:string -> ?rescan:bool ->
              a t -> Blank.t -> in_channel -> a =
  fun ?(utf8=Utf8.ASCII) ?filename ?(rescan=true) g b ic ->
    parse_buffer g b (Input.from_channel ~utf8 ?filename ~rescan ic)
      Input.init_idx

let parse_fd
    : type a. ?utf8:Utf8.context -> ?filename:string -> ?rescan:bool ->
              a t -> Blank.t -> Unix.file_descr -> a =
  fun ?(utf8=Utf8.ASCII) ?filename ?(rescan=true) g b ic ->
    let buf = Input.from_fd ~utf8 ?filename ~rescan ic in
    parse_buffer g b buf Input.init_idx

let parse_file ?(utf8=Utf8.ASCII) g b filename =
    let ic = Unix.(openfile filename [O_RDONLY] 0) in
    parse_fd ~utf8 ~filename g b ic

let lpos ?name g = lpos ?name ?pk:None g
let appl ?name = appl ?name:(created name)
let alt ?name = alt ?name:(created name)

let declare_grammar name = declare_grammar (name, Given)

(*
let print_grammar ?(no_other=false) ?(def=true) ch s =
  let _ = compile s in
  print_grammar ~no_other ~def ch s
 *)
