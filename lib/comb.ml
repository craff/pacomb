(** Parser combinator library *)

(** Combinators are a standard approach  to parsing in functional language.  The
    major advantage of  combinators is that they allow  manipulating grammars as
    first class values.  However, they generally suffer from  two major defects.

    - Incomplete semantics.  A grammar  "(a|b)c" may fail  to backtrack  and try
      "bc" if parsing  for "ac" fails in "c". This  is traditionally solved with
      continuation: combinators must be given the  function that will be used to
      parse the remaining input.

    - Exponential semantics.  The parsing problem for  context-free grammars can
      be solved  in polynomial time  (O(n³) implementation are  often proposed).
      As combinator  backtrack, they usually  lead to an  exponential behaviour.
      This is solved here by a [cache] combinator, that avoids parsing twice the
      same part of the input with the same grammar.

    - backtracking is also a problem, because we need to go back in the input to
      try other  alternatives. This means  that the  whole input must  remain in
      memory.  This is  solved  by terminals  returning  immediately instead  of
      calling the continuation and a "scheduler" will store the continuation and
      call the  error function  (we use continuations  and errors).  This forces
      parsing  all terminals  in parallel.  This also  gives a  very nice  cache
      combinator.

    - A last problem arise in many technics that cover ambiguous grammars: right
      recursive grammar will  try to compute the action for  all accepted prefix
      of the input,  often leading to quadratic parsing time.  This is solved by
      delaying the  evaluation of the  semantics, but not  too much so  that the
      user can call the [give_up] function to reject some parses from the action
      code.  *)

(** Environment holding information require for parsing. *)
type env =
  { blank_fun         : Lex.blank
  (** Function used to ignore blanks. *)
  ; max_pos           : (int * Lex.buf * Lex.pos * string list ref) ref
  (** Maximum position reached by the parser (for error reporting). *)
  ; current_buf       : Lex.buf
  (** Current input buffer (or input stream). *)
  ; current_pos       : Lex.pos
  (** Current column number in buffer [current_buf]. *)
  ; buf_before_blanks : Lex.buf
  (** Input buffer before reading the blanks. *)
  ; pos_before_blanks : Lex.pos
  (** Column number in [buf_before_blanks] before reading the blanks. *)
  ; lr                : Assoc.t
  (** Association table for the lr combinator *)
  ; merge_depth       : int * int
  }

(** Type of a function called in case of error. *)
type err = unit -> res

(**  type  of result  used  by  the scheduler  to  progress  in the  parsing  in
    parallel *)
 and res =
   | Cont : env * 'a cont * err * 'a Lazy.t -> res
   (** returned by lexeme instead of calling the continuation, contains all
       information to continue parsing. *)

 (** Type  of a  parsing continuation. A  value of type  ['a cont]  represents a
    function waiting for a parsing environment, an error function and a value of
    type ['a] to continue parsing. To avoid quadratic behavior with mainly right
    recursion, this is splitted in two:

    - a transformer  of type [('a,'b) trans]  represents a function from ['a] to
      ['b]

    - continuation expect a lzay value, evaluation is retarded to the parsing of
      the next lexeme.
*)
 and 'a cont =
   | C : (env -> err -> 'b Lazy.t -> res) * ('a,'b) trans -> 'a cont
   | P : (env -> err -> 'b Lazy.t -> res) * ('a,'b) trans * Pos.t ref -> 'a cont
 (** [P] is used when the position when calling the continuation (right position
       of some grammar) is needed. *)

 (** [('a,'b) args]  is the type of a transformer from a value of type ['a] to a
    value of type ['b]. *)
 and (_,_) trans =
   | Idt : ('a,'a) trans
   (** Identity transformer *)
   | Arg : ('b,'c) trans * 'a -> ('a -> 'b,'c) trans
   (** [Arg(tr,x)] tranform a value of type ['a -> 'b] into a value of
       type ['c] by applying it to [x] and then applying the transformer [tr] *)
   | Lrg : ('b,'c) trans * 'a Lazy.t -> ('a -> 'b,'c) trans
   (** Same as above but [x] will results of the application of a transformer.
       [Lrg] means lazy arg *)
   | Pos : ('b,'c) trans * Pos.t ref -> (Pos.t -> 'b,'c) trans
   (** Same  as arg, but [x]  is a position that  will be stored in  a reference
       when calling the continuation *)
   | App : ('b,'c) trans * ('a -> 'b) -> ('a,'c) trans
   (** [App(tr,f) transform  a value of type  ['a] into a value of  type ['c] by
        passing it to a [f] and then using [tr] *)

 (** Type of a parser combinator with a semantic action of type ['a]. the return
    type [res] will be used by the scheduler function below to drive the
    parsing. *)
and 'a t = env -> 'a cont -> err -> res

(** continuations and trans functions *)

(** construction of a continuation with an identity transformer *)
let ink f = C(f,Idt)

(** evaluation function for the [app] type *)
let rec eval : type a b. a -> (a,b) trans -> b = fun x tr ->
    match tr with
    | Idt        -> x
    | Arg(tr,y)  -> eval (x y) tr
    | Lrg(tr,y)  -> eval (x (Lazy.force y)) tr
    | Pos(tr,p)  -> eval (x !p) tr
    | App(tr,f)  -> eval (f x) tr

(** function calling a  continuation. It does not evaluate any  action. It is of
    crucial importane that this function be in O(1). *)
let call : type a.a cont -> env -> err -> a Lazy.t -> res =
  fun k env err x ->
    match k with
    | C(k,Idt)    -> k env err x
    | C(k,tr)     -> k env err (lazy (eval (Lazy.force x) tr))
    | P(k,Idt,rp) ->
       rp := Pos.get_pos env.buf_before_blanks env.pos_before_blanks;
       k env err x
    | P(k,tr,rp)  ->
       rp := Pos.get_pos env.buf_before_blanks env.pos_before_blanks;
       k env err (lazy (eval (Lazy.force x) tr))

(** access to transformer constructor inside the continuation constructor *)
let arg : type a b. b cont -> a -> (a -> b) cont = fun k x ->
    match k with
    | C(k,tr)    -> C(k,Arg(tr,x))
    | P(k,tr,rp) -> P(k,Arg(tr,x),rp)

let larg : type a b. b cont -> a Lazy.t -> (a -> b) cont = fun k x ->
    match k with
    | C(k,tr)    -> C(k,Lrg(tr,x))
    | P(k,tr,rp) -> P(k,Lrg(tr,x),rp)

let app : type a b. b cont -> (a -> b) -> a cont = fun k f ->
    match k with
    | C(k,tr)    -> C(k,App(tr,f))
    | P(k,tr,rp) -> P(k,App(tr,f),rp)

(** transforsms [Lrg] into [Arg] inside a continuation *)
let eval_lrgs : type a. a cont -> a cont = fun k ->
  let rec fn : type a b. (a,b) trans -> (a,b) trans = function
    | Idt       -> Idt
    | Arg(tr,x) -> Arg(fn tr, x)
    | Lrg(tr,x) -> Arg(fn tr, Lazy.force x)
    | Pos(tr,p) -> Pos(fn tr,p)
    | App(tr,x) -> App(fn tr,x)
  in
  match k with
  | C(k,tr)    -> C(k,fn tr)
  | P(k,tr,rp) -> P(k,fn tr,rp)

(** [next env  err] updates the current maximum position  [env.max_pos] and then
    calls the [err] function. *)

let record_pos env =
  let (pos_max, _, _, _) = !(env.max_pos) in
  let pos = Input.byte_pos env.current_buf env.current_pos in
  if pos > pos_max  then
    env.max_pos := (pos, env.current_buf, env.current_pos, ref [])

let next : env -> err -> res  = fun env err -> record_pos env; err ()

let record_pos_msg msg env =                                                                   let (pos_max, _, _, msgs) = !(env.max_pos) in
  let pos = Input.byte_pos env.current_buf env.current_pos in
  if pos > pos_max then
    env.max_pos := (pos, env.current_buf, env.current_pos, ref [msg])
  else if pos = pos_max then msgs := msg :: !msgs

let next_msg : string -> env -> err -> res  = fun msg env err ->
  record_pos_msg msg env; err ()

(** the scheduler stores what remains to do in a list sorted by position in the
    buffer, and vptr key list (see cache below) here are the comparison function
    used for this sorting *)
let before r1 r2 =
  match (r1,r2) with
  | (Cont(env1,_,_,_), Cont(env2,_,_,_)) ->
     let p1 = Input.byte_pos env1.current_buf env1.current_pos in
     let p2 = Input.byte_pos env2.current_buf env2.current_pos in
     (p1 < p2) || (p1 = p2 && env1.merge_depth > env2.merge_depth)

(* Here we implement priority queus using a heap, to have a logarithmic
   complexity to choose the next action in the scheduler *)
type heap =
  E | N of res * heap * heap * int

let size = function E -> 0 | N(_,_,_,n) -> n

(** insert in a list at the correct position *)
let rec insert : res -> heap -> heap = fun r h ->
  match h with
  | E -> N(r, E, E, 1)
  | N(r',h1,h2,s) ->
     let (r,r') = if before r' r then (r',r) else (r,r') in
     if size h1 > size h2 then
       N(r, h1, insert r' h2, s+1)
     else
       N(r, insert r' h1, h2, s+1)

let extract : heap -> res * heap = fun h ->
  match h with
  | E -> raise Not_found
  | N(r,h1,h2,s) ->
     let rec fusion h1 h2 =
       match h1, h2 with
       | E, E -> E
       | _, E -> h1
       | E, _ -> h2
       | N(r1,h11,h12,_), N(r2,h21,h22,_) ->
          if before r1 r2 then
            N(r1,fusion h11 h12,h2,s-1)
          else
            N(r2,h1,fusion h21 h22,s-1)
     in
     (r, fusion h1 h2)

(** [scheduler  env g] drives  the parsing, it calls  the combinator [g]  in the
    given environment and when lexeme returns to the scheduler, it continues the
    parsing,  but  trying the error case too,  this way all  parsing progress in
    parallel in the input. *)
let scheduler : env -> 'a t -> ('a * env) list = fun env g ->
    (* a reference holding the final result *)
    let res = ref [] in
    (* the final continuation evaluating and storing the result,
       continue parsing if [all] is [true] *)
    let k env err x =
      (try
         res := (x,env)::!res;
         err
       with Lex.NoParse   -> fun () -> next env err
          | Lex.Give_up m -> fun () -> next_msg m env err) ();
    in
    try
      (* calls to the initial grammar and initialise the table *)
      let r = g env (ink k) (fun _ -> raise Exit) in
      let tbl = ref (N(r, E, E, 1)) in  (* to do at further position *)
      while true do
        let (Cont(env,k,err,x)),t = extract !tbl in
        tbl := t;
        (* calling the error and the continuation, storing the result in
              tbl1. *)
        (try
           let r = err () in
           tbl := insert r !tbl
         with Exit -> ());
        (try
           let k = eval_lrgs k in
           let r = call k env (fun _ -> raise Exit) x in
           tbl := insert r !tbl
         with
         | Exit -> ()
         | Lex.NoParse -> record_pos env
         | Lex.Give_up m -> record_pos_msg m env)
      done;
      assert false
    with Not_found | Exit -> List.map (fun (x,env) -> (Lazy.force x, env)) !res

(** Combinator that always fails. *)
let fail : 'a t = fun env _ err -> next env err

(** Fails and report an error *)
let error : string -> 'a t = fun msg env _ err -> next_msg msg env err

(** Combinator used as default fied before compilation *)
let assert_false : 'a t = fun _ _ _ -> assert false

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x env kf err -> call kf env err (lazy x)

(** Combinator accepting the given lexeme (or terminal). *)
let lexeme : 'a Lex.lexeme -> 'a t = fun lex env k err ->
    try
      let (v, buf_before_blanks, pos_before_blanks) =
        lex env.current_buf env.current_pos
      in
      let (current_buf, current_pos) =
        env.blank_fun buf_before_blanks pos_before_blanks
      in
      let env =
        { env with buf_before_blanks ; pos_before_blanks
                   ; current_buf ; current_pos; lr = Assoc.empty }
      in
      Cont(env,k, err, lazy v)
    with Lex.NoParse -> next env err
       | Lex.Give_up m -> next_msg m env err

(** Sequence combinator. *)
let seq : 'a t -> ('a -> 'b) t -> 'b t = fun g1 g2 env k err ->
    g1 env (ink (fun env err x -> g2 env (larg k x) err)) err

(** Dependant sequence combinator. *)
let dseq : ('a * 'b) t -> ('a -> ('b -> 'c) t) -> 'c t =
  fun g1 g2 env k err ->
    g1 env (ink(fun env err vs ->
        (try
           let (v1,v2) = Lazy.force vs in
           (* This forces the evaluation of v2 ... no consequence
              on right recursion *)
           let g = g2 v1 in
           fun () -> g env (arg k v2) err
         with Lex.NoParse -> fun () -> next env err
            | Lex.Give_up m -> fun () -> next_msg m env err) ())) err

(** [test cs env] returns [true] if and only if the next character to parse in
    the environment [env] is in the character set [cs]. *)
let test cs e = Charset.mem cs (Input.get e.current_buf e.current_pos)

(** option combinator,  contrary to [alt] apply to [empty],  it uses the charset
    of the  continuation for prediction. Therefore  it is preferable not  to use
    empty in [alt] and use [option] instead.*)
let option: 'a -> Charset.t -> 'a t -> 'a t = fun x cs1 g1 ->
  fun env k err ->
    if test cs1 env then call k env (fun () -> g1 env k err) (lazy x)
    else call k env err (lazy x)

(** Alternatives combinator. *)
let alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t = fun cs1 g1 cs2 g2 ->
  fun env k err ->
    match (test cs1 env, test cs2 env) with
    | (false, false) -> next env err
    | (true , false) -> g1 env k err
    | (false, true ) -> g2 env k err
    | (true , true ) -> g2 env k (fun () -> g1 env k err)

(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn env k err ->
    g env (app k fn) err

(** test combinator before a grammar *)
let test_before : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t =
  fun test g env k err ->
    match test env.buf_before_blanks env.pos_before_blanks
            env.current_buf env.current_pos
    with false -> next env err
       | true -> g env k err

(** test combinator after a grammar *)
let test_after : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t =
  fun test g env k err ->
    let k = ink (fun env err x ->
      match test env.buf_before_blanks env.pos_before_blanks
             env.current_buf env.current_pos
      with false -> err ()
         | true -> call k env err x)
    in
    g env k err

(** Read the position after parsing. *)
let right_pos : type a.(Pos.t -> a) t -> a t = fun g env k err ->
    let k = match k with
      | C(k,tr)    -> let rp = ref Pos.phantom in P(k,Pos(tr,rp),rp)
      | P(k,tr,rp) -> P(k,Pos(tr,rp),rp)
    in
    g env k err

(** Read the position before parsing. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g  env k err ->
    let pos = Pos.get_pos env.current_buf env.current_pos in
    g env (arg k pos) err

(** Read lpos from the lr table. *)
let read_pos : Pos.t Assoc.key -> (Pos.t -> 'a) t -> 'a t =
  fun key g env k err ->
    let pos = try Assoc.find key env.lr with Not_found -> assert false in
    g env (arg k pos) err

(** key used by lr below *)
type 'a key = 'a Lazy.t Assoc.key

(** [lr g  gf] is the combinator used to  eliminate left recursion. Intuitively,
    it parses using  the "grammar" [g gf*].  An equivalent  combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)].
    NOTE: left recusion forces evaluation and this is good!
*)
let lr : 'a t -> 'a key -> 'a t -> 'a t = fun g key gf env k err ->
    let rec klr env err v =
      let err () =
        let lr = Assoc.add key v env.lr in
        let env0 = { env with lr } in
        gf env0 (ink klr) err
      in
      call k env err v
    in
    g env (ink klr) err

let lr_pos : 'a t -> 'a key -> Pos.t Assoc.key -> 'a t -> 'a t =
  fun g key pkey gf env k err ->
    let pos = Pos.get_pos env.current_buf env.current_pos in
    let rec klr env err v =
      let err () =
        let lr = Assoc.add key v env.lr in
        let lr = Assoc.add pkey pos lr in
        let env0 = { env with lr } in
        gf env0 (ink klr) err
      in
      call k env err v
    in
    g env (ink klr) err

(** combinator to access the value stored by lr*)
let read_tbl : 'a key -> 'a t = fun key env k err ->
    let v = try Assoc.find key env.lr with Not_found -> assert false in
    call k env err v


(** Combinator under a refrerence used to implement recursive grammars. *)
let deref : 'a t ref -> 'a t = fun gref env k err -> !gref env k err

(** Combinator changing the "blank function". *)
let change_layout : ?config:Lex.layout_config -> Lex.blank -> 'a t -> 'a t =
    fun ?(config=Lex.default_layout_config) blank_fun g env k err ->
    let (s, n) as buf =
      if config.old_blanks_before then (env.current_buf, env.current_pos)
      else (env.buf_before_blanks, env.pos_before_blanks)
    in
    let (s, n) =
      if config.new_blanks_before then blank_fun s n
      else buf
    in
    let old_blank_fun = env.blank_fun in
    let env = { env with blank_fun ; current_buf = s ; current_pos = n } in
    g env (ink (fun env err v ->
      let (s, n) as buf =
        if config.new_blanks_after then (env.current_buf, env.current_pos)
        else (env.buf_before_blanks, env.pos_before_blanks)
      in
      let (s, n) =
        if config.old_blanks_after then old_blank_fun s n
        else buf
      in
      let env =
        { env with blank_fun = old_blank_fun
        ; current_buf = s ; current_pos = n }
      in
      Cont(env,k,err,v))) err

(** Combinator for caching a grammar, to avoid exponential behavior.
    very bad performance with a non ambiguous right recursive grammar. *)
let cache : type a. ?merge:(a -> a -> a) -> a t -> a t = fun ?merge g ->
  let cache = Input.Tbl.create () in
  fun env0 k err ->
  let {current_buf = buf0; current_pos = col0} = env0 in
  try
    let (ptr, too_late) = Input.Tbl.find cache buf0 col0 in
    assert (not !too_late);
    ptr := (k, env0.merge_depth) :: !ptr;
    err ()
  with Not_found ->
    (* size of ptr: O(N) for each position,
         it comes from a rule using that grammar (nb of rule constant),
         and the start position of this rule (thks to cache, only one each)  *)
    let ptr = ref [(k,env0.merge_depth)] in
    let too_late = ref false in
    Input.Tbl.add cache buf0 col0 (ptr, too_late);
    (* we create a merge table for all continuation to call merge on all
         semantics before proceding. To do so, we need to complete all parsing
         of this grammar at this position before calling the continuation.
         [vnum] will be used to ensure this, see below. *)
    let merge_tbl = Input.Tbl.create () in
    let vnum = 2 * Input.byte_pos buf0 col0 in
    assert(vnum >= fst env0.merge_depth);
    let merge_depth = if fst env0.merge_depth = vnum then (vnum, snd env0.merge_depth + 1)
                      else (vnum, 0)
    in
    (* we push vnum in the merge_depth environment, ensuring the correct order
         in the scheduler, a continuation k1 with a merge_depth which is a
         strict prefix of the merge_depth of k2, must be called after (k1 after
         k2) *)
    let env = { env0 with merge_depth } in
    let k0 env err v =
      let {current_buf = buf; current_pos = col} = env in
      try
        if merge = None then raise Not_found;
        (* The current environment is waiting for the value identified by
          vnum *)
        assert (merge_depth = env.merge_depth);
        (* We get the vptr to share this value, if any *)
        let (vptr,too_late_v) = Input.Tbl.find merge_tbl buf col in
        (* and it is not too late to add a semantics for this action *)
        assert (not !too_late_v);
        (* size of vptr: O(N) at each position: number of ways to
             end parsing of this grammar. Beware, vptr are distinct
             for distinct start position of parsing. So total number
             of vptr is O(N^2). It can be more if you are not using
             enough cache.  *)
        vptr := v :: !vptr;
        (* No need to continue parsing, we try other branches *)
        err ()
      with Not_found ->
        let v = match merge with
          | None -> v (* No merge, no sharing *)
          | Some merge ->
             (* create vptr and register in merge_tbl *)
             let vptr = ref [] in
             let too_late_v = ref false in
             Input.Tbl.add merge_tbl buf col (vptr,too_late_v);
             (* the semantics merge together all value in vptr,
                  Once force, too_late ensure an assertion failure if
                  we try to extend vptr *)
             lazy (
                 too_late_v := true;
                 let r = ref [] in
                 (* do not forget to deal with give_up *)
                 let force x = try Some (Lazy.force x)
                               with Lex.NoParse -> None
                                  | Lex.Give_up m -> r := m :: !r; None
                 in
                 let merge x v =
                   try let y = Lazy.force v in
                       match x with None -> Some y
                                  | Some x -> Some (merge x y)
                   with Lex.NoParse -> x
                      | Lex.Give_up m -> r := m :: !r; x
                 in
                 match List.fold_left merge (force v) !vptr with
                 | None -> (match !r with [] -> raise Lex.NoParse
                                        | m::_ -> Lex.give_up ~msg:m ())
                 | Some x -> x)
        in
        (* Now we call all continuation stored in !ptr *)
        let l0 = !ptr in
        too_late := true;
        let rec fn l =
          match l with
          | [] -> err ()
          | (k,merge_depth) :: l ->
             (* we pop vnum from merge_depth to ensure this continuation
                  is called after all extensions of vptr *)
             let env = { env with merge_depth } in
             Cont(env,k,(fun () -> fn l),v)
        in
        fn l0
    in
    g env (ink k0) err (* safe to call g, env had vnum pushed so it is a minimum *)

(** function doing the parsing *)
let gen_parse_buffer
    : type a. a t -> Lex.blank -> ?blank_after:bool
                  -> Lex.buf -> Lex.pos -> (a * Lex.buf * Lex.pos) list =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let p0 = Input.char_pos buf0 col0 in
    let max_pos = ref (p0, buf0, col0, ref []) in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; pos_before_blanks = col0
        ; current_buf = buf ; current_pos = col; lr = Assoc.empty
        ; max_pos ; blank_fun ; merge_depth = (-1,0)}
    in
    let r = scheduler env g in
    match r with
    | [] ->
       let (_, buf, col, msgs) = !max_pos in
       let msgs = List.sort_uniq compare !msgs in
       raise (Pos.Parse_error(buf, col, msgs))
    | _ -> List.map (fun (v,env) ->
               if blank_after then (v, env.current_buf, env.current_pos)
               else (v, env.buf_before_blanks, env.pos_before_blanks)) r

(** the two main variation of the above *)
let partial_parse_buffer : type a. a t -> Lex.blank -> ?blank_after:bool
                                -> Lex.buf -> Lex.pos -> a * Lex.buf * Lex.pos =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let l = gen_parse_buffer g blank_fun ~blank_after buf0 col0 in
    match l with [r]  -> r
               | r::_ -> Printf.eprintf "Parsing ambiguity, use merge\n%!"; r
               | []   -> assert false

let parse_all_buffer
    : type a. a t -> Lex.blank -> Lex.buf -> Lex.pos -> a list =
  fun g blank_fun buf0 col0 ->
    let l = gen_parse_buffer g blank_fun buf0 col0 in
    List.map (fun (r,_,_) -> r) l
