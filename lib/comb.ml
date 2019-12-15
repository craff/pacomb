(** {1 Parser combinator library} *)

(** Combinators are a standard approach to parsing in functional language.  The
   major advantage of combinators is that they allow manipulating grammars as
   first class values.  However, they generally suffer from two major defects.
   We start this file by a global description of the original feature of this
   library.

    - Incomplete semantics.  A grammar "(a|b)c" may fail to backtrack and try
   "bc" if parsing for "ac" fails in "c". This is traditionally solved with
   continuation: combinators must be given the function that will be used to
   parse the remaining input. In general, parsing combinator returning value of
   type ['a] with continuation will have type [env -> (env -> 'a -> bot) -> bot]
   where [env] is the type maintaining the data necessary for parsing (like the
   current input stream) and [bot] is then type of [false]. ['a cont = 'a -> env
   -> bot] is thefore the continuation type.

   - Exponential semantics.  The parsing problem for context-free grammars can
   be solved in polynomial time (O(n³) implementation are often proposed).  As
   combinator backtracks, they usually lead to an exponential behaviour.  This
   is solved here by a [cache] combinator, that avoids parsing twice the same
   part of the input with the same grammar.

   - backtracking  is also a problem, because we need to go back in the input to
   try  other alternatives.   This means  that the  whole input  must remain  in
   memory.  This is solved by terminals returning immediately instead of calling
   the continuation. A "scheduler" manages the continuations and the alternative
   branches for  parsing. This means  that instead of [bot],  we use a  type ['a
   res]  with  two  constructors,  one for  continuation,  one  for  alternative
   parsing.  A global  queue  stored  in all  environment  is  maintened by  the
   scheduler, and  the next action  is taken  from the top  of the queue  by the
   scheduler. The ordering in the queue is a lexicographic ordering on the input
   position and definition dependance (if A  calls B, the continuation of B must
   be called before the continuation of A). The former ensure that all terminals
   are parsed in parallel, and therefore that  the beginning of the input can be
   collected by  the GC.   The latter  is necessay for  the cache  combinator to
   work.

   - In many  technics that cover  ambiguous  grammars: right  recursive grammar
   will try to  compute the action for  all accepted prefix of  the input, often
   leading to quadratic parsing time.  This is solved by delaying the evaluation
   of the semantics,  but not too much  so that the user can  call the [give_up]
   function to reject some parses from the action code. More generaly, to ensure
   O(1)for most step of parsing, we use two specific type to represent the value
   returned by parsing and value transforming these

   -  Specific combinators (lr, lr_pos  and mlr) are provided  to transform left
   recursive grammar  that loops with  combinator into non left  recursive ones.

    - A  blank  fonction  is used  (ad  it can  be  changed  during parsing)  to
   compensate  the  scannerless  nature  of  combinators  and  deal  with  blank
   characteres.
*)

(** {2 main types } *)

(** Environment holding information required for parsing. *)
type env =
  { blank_fun         : Blank.t
  (** Function used to ignore blanks. *)
  ; max_pos           : (int * Lex.buf * Lex.pos * string list ref) ref
  (** Maximum position reached by the parser (for error reporting). *)
  ; current_buf       : Lex.buf
  (** Current input buffer (or input stream). *)
  ; current_pos       : Lex.pos
  (** Current position in buffer [current_buf]. *)
  ; buf_before_blanks : Lex.buf
  (** Input buffer before reading the blanks. *)
  ; pos_before_blanks : Lex.pos
  (** Position in [buf_before_blanks] before reading the blanks. *)
  ; lr                : Assoc.t
  (** Association table for the lr, lr_pos and mlr combinators *)
  ; cache_order       : int * int
  (** Information used  to order cache parsing, the first  [int] is the position
      where we started to parse the  cached grammar, the second int is increased
      when we parse a cached grammar that was called at same position by another
      cached grammar. *)
  ; queue : heap ref
  (** the  heap holding continuation  and alternative branch for  parsing. All
      environments share the same heap.  *)
  }

(** We  use priority queues  using a heap, to  have a logarithmic  complexity to
   choose the next action in the scheduler *)
and heap =
  | E                            (** empty heap *)
  | N of res * heap * heap * int (** node heap, holding the heap size *)

(**  type  of result  used  by  the scheduler  to  progress  in the  parsing  in
    parallel. *)
 and res =
   | Cont : env * 'a cont * 'a Lazy.t -> res
   (**  Cont(env,k,x)  the value  and  environment  resulting from  parsing  the
      beginning of the  input and the continuation to finish  parsing It must be
      used instead of calling immediatly  the continuation because the scheduler
      needs to order parsing. *)
   | Gram : env * 'a t * 'a cont -> res
   (** This contructor represents an alternative branch of parsing, with both a
   grammar and a continuation *)

 (** Type  of a  parsing continuation. A  value of type  ['a cont]  represents a
    function  waiting for  a parsing  environment and  a value  of type  ['a] to
    continue parsing.  To avoid quadratic behavior with right recursion, this is
    splitted in two parts:

    - a transformer of type [('a,'b) trans] representing a function from ['a] to
    ['b]

    - a continuation  that expect a  lazy value,  evaluation is retarded  to the
    parsing of the next lexeme.

    - A special continuation [P] is used when we need to store the position when
    we will call the continuation.

    With this type for continuation, we have two benefits:

    - most  combinator  can transform  the  continuation  in O(1)  time  without
    introducing a nested closure.

    - evaluation of action being retarded to the next lexeme, prefix of right
    recursive grammar also transform the continuation in O(1).  *)
 and 'a cont =
   | C : (env -> 'b Lazy.t -> res) * ('a,'b) trans -> 'a cont
   | P : (env -> 'b Lazy.t -> res) * ('a,'b) trans * Pos.t ref -> 'a cont
 (** [P] is used when the position when calling the continuation (right position
       of some grammar) is needed. *)

 (** [('a,'b) args] is the type of a transformer from a value of type ['a] to a
    value of type ['b]. To keep amortized O(1) semantics of eval_lrgs, we mark
    in the constructor the presence of Lrg below in the structure using XXX'
    constructor*)
 and (_,_) trans =
   | Idt : ('a,'a) trans
   (** Identity transformer *)
   | Arg : ('b,'c) trans * 'a -> ('a -> 'b,'c) trans
   (** [Arg(tr,x)] tranform a value of type ['a -> 'b] into a value of
       type ['c] by applying it to [x] and then applying the transformer [tr] *)
   | Lrg : ('b,'c) trans * 'a Lazy.t -> ('a -> 'b,'c) trans
   (** Same as above  but [x] is lazy ([Lrg] means "lazy arg").  This is what we
       use to delay evaluation *)
   | Pos : ('b,'c) trans * Pos.t ref -> (Pos.t -> 'b,'c) trans
   (** Same  as arg, but [x]  is a position that  will be stored in  a reference
       when calling the continuation constructed with [P] *)
   | App : ('b,'c) trans * ('a -> 'b) -> ('a,'c) trans
   (** [App(tr,f) transform  a value of type  ['a] into a value of  type ['c] by
        passing it to a [f] and then using [tr] *)
   | Arg' : ('b,'c) trans * 'a -> ('a -> 'b,'c) trans
   (** same  has above  when lrgs  appear under, to  avoid useless  traversal in
       [eval_lrgs] *)
   | Pos' : ('b,'c) trans * Pos.t ref -> (Pos.t -> 'b,'c) trans
   (** same has above when lrgs appear under *)
   | App' : ('b,'c) trans * ('a -> 'b) -> ('a,'c) trans
   (** same has above when lrgs appear under *)

 (** Type of a parser combinator with a semantic action of type ['a]. the return
    type  [res] will  be  used by  the  scheduler function  below  to drive  the
    parsing. *)
and 'a t = env -> 'a cont -> res

(** {2 continuations and trans functions} *)

(** construction of a continuation with an identity transformer.
    [ink] means "injection kontinuation" *)
let ink f = C(f,Idt)

(** tells if lrg is present *)
let has_lrg : type a b.(a,b) trans -> bool = function
  | Idt -> false
  | Arg(_,_) -> false
  | Pos(_,_) -> false
  | App(_,_) -> false
  | _        -> true

(** evaluation function for the [trans] type *)
let rec eval : type a b. a -> (a,b) trans -> b = fun x tr ->
    match tr with
    | Idt        -> x
    | Lrg (tr,y) -> eval (x (Lazy.force y)) tr
    | Arg (tr,y) -> eval (x y) tr
    | Arg'(tr,y) -> eval (x y) tr
    | Pos (tr,p) -> eval (x !p) tr
    | Pos'(tr,p) -> eval (x !p) tr
    | App (tr,f) -> eval (f x) tr
    | App'(tr,f) -> eval (f x) tr

(** transforsms [Lrg] into [Arg] inside a continuation, to trigger [give_up]
    soon enough, that is after a read *)
let eval_lrgs : type a. a cont -> a cont = fun k ->
  let rec fn : type a b. (a,b) trans -> (a,b) trans = fun t ->
    match t with
    | Lrg (tr,x) -> Arg(fn tr, Lazy.force x)
    | Arg'(tr,x) -> Arg(fn tr,x)
    | Pos'(tr,p) -> Pos(fn tr,p)
    | App'(tr,x) -> App(fn tr,x)
    | _          -> t
  in
  match k with
  | C(k,tr)    -> C(k,fn tr)
  | P(k,tr,rp) -> P(k,fn tr,rp)

(** function calling a  continuation. It does not evaluate any  action. It is of
    crucial importance that this function be in O(1) before calling [k]. *)
let call : type a.a cont -> env -> a Lazy.t -> res =
  fun k env x ->
    match k with
    | C(k,Idt)    -> k env x
    | C(k,tr)     -> k env (lazy (eval (Lazy.force x) tr))
    | P(k,Idt,rp) ->
       rp := Pos.get_pos env.buf_before_blanks env.pos_before_blanks;
       k env x
    | P(k,tr,rp)  ->
       rp := Pos.get_pos env.buf_before_blanks env.pos_before_blanks;
       k env (lazy (eval (Lazy.force x) tr))

(**  functions  to  add  [(_,_)  trans]  constructors  inside  the  continuation
    constructor *)
let arg : type a b. b cont -> a -> (a -> b) cont = fun k x ->
  let arg tr x = if has_lrg tr then Arg'(tr,x) else Arg(tr,x) in
  match k with
  | C(k,tr)    -> C(k,arg tr x)
  | P(k,tr,rp) -> P(k,arg tr x,rp)

let lrg : type a b. b cont -> a Lazy.t -> (a -> b) cont = fun k x ->
  (** NOTE: no need for lrg if [x] is a value, but adding the following line
     appears significantly slower:

  [if Lazy.is_val x then arg k (Lazy.force x) else] *)
  match k with
  | C(k,tr)    -> C(k,Lrg(tr,x))
  | P(k,tr,rp) -> P(k,Lrg(tr,x),rp)

let app : type a b. b cont -> (a -> b) -> a cont = fun k f ->
  let app tr f = if has_lrg tr then App'(tr,f) else App(tr,f) in
    match k with
    | C(k,tr)    -> C(k,app tr f)
    | P(k,tr,rp) -> P(k,app tr f,rp)

let posk : type a. a cont -> (Pos.pos -> a) cont = fun k ->
  let pos tr rp = if has_lrg tr then Pos'(tr,rp) else Pos(tr,rp) in
  match k with
  | C(k,tr)    -> let rp = ref Pos.phantom in P(k,pos tr rp,rp)
  | P(k,tr,rp) -> P(k,pos tr rp,rp)

(** {2 Error managment} *)

(** record the current position, before a parsing error *)
let record_pos env =
  let (pos_max, _, _, _) = !(env.max_pos) in
  let pos = Input.byte_pos env.current_buf env.current_pos in
  if pos > pos_max  then
    env.max_pos := (pos, env.current_buf, env.current_pos, ref [])

(** [next env] updates the current maximum position [env.max_pos] and
   raise [Exit] to return to the scheduler. *)
let next : env -> res  = fun env -> record_pos env; raise Exit

(** same as abobe, but recording error messages *)
let record_pos_msg msg env =
  let (pos_max, _, _, msgs) = !(env.max_pos) in
  let pos = Input.byte_pos env.current_buf env.current_pos in
  if pos > pos_max then
    env.max_pos := (pos, env.current_buf, env.current_pos, ref [msg])
  else if pos = pos_max then msgs := msg :: !msgs

let next_msg : string -> env -> res  = fun msg env ->
  record_pos_msg msg env; raise Exit

(** {2 Scheduler code} *)

(** the scheduler stores what remains to do in a list sorted by position in the
    buffer, and vptr key list (see cache below) here are the comparison function
    used for this sorting *)
let before r1 r2 =
  match (r1,r2) with
  | (Cont(env1,_,_)|Gram(env1,_,_)), (Cont(env2,_,_)|Gram(env2,_,_)) ->
     let p1 = Input.byte_pos env1.current_buf env1.current_pos in
     let p2 = Input.byte_pos env2.current_buf env2.current_pos in
     (p1 < p2) || (p1 = p2 && env1.cache_order > env2.cache_order)

(** Size of the heap, to choose where to insert *)
let size = function E -> 0 | N(_,_,_,n) -> n

(** insert in a heap at the correct position *)
let rec insert : res -> heap -> heap = fun r h ->
  match h with
  | E -> N(r, E, E, 1)
  | N(r',h1,h2,s) ->
     let (r,r') = if before r' r then (r',r) else (r,r') in
     if size h1 > size h2 then
       N(r, h1, insert r' h2, s+1)
     else
       N(r, insert r' h1, h2, s+1)

let add_queue env res =
  env.queue := insert res !(env.queue)

(** Extract  from the  heap. Does  not keep  balancing, but  the depth  may only
   decrease so it remains amortized logarithmic *)
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
    (** a reference holding the final result *)
    let res = ref [] in
    (** the final continuation evaluating and storing the result *)
    let k env x =
      res := (x,env)::!res; (** evaluation of x is done later *)
      raise Exit
    in
    try
      let queue = env.queue in
      (** calls to the initial grammar and initialise the table *)
      (try
        let r = g env (ink k) in
        queue := insert r !queue;  (** to do at further position *)
      with Exit -> ());
      while true do
        let (todo,t) = extract !queue in
        queue := t;
        try
          let r =
            match todo with
            | Cont(env,k,x) ->
               (** it is time to eval lazy arguments *)
               let k = eval_lrgs k in
               (** calling the continuation *)
               call k env x
            | Gram(env,g,k) ->
               g env k
          in
          queue := insert r !queue
        with
        | Exit -> ()
        | Lex.NoParse -> record_pos env
        | Lex.Give_up m -> record_pos_msg m env
      done;
      assert false
    with Not_found | Exit ->
      (** parsing is finished: we evaluate the semantics *)
      List.fold_left (fun res (x,env) ->
          (try (Lazy.force x, env) :: res
           with Lex.Give_up m -> record_pos_msg m env; res
              | Lex.NoParse -> record_pos env; res)) [] !res

(** {2 the combinators } *)

(** Combinator that always fails. *)
let fail : 'a t = fun env _ -> next env

(** Fails and report an error *)
let error : string -> 'a t = fun msg env _ -> next_msg msg env

(** Combinator used as default field before compilation *)
let assert_false : 'a t = fun _ _ -> assert false

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x env kf -> call kf env (lazy x)

(** Combinator accepting the given lexeme (or terminal). *)
let lexeme : 'a Lex.lexeme -> 'a t = fun lex env k ->
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
      (** don't call the continuation, return to the scheduler *)
      Cont(env,k, lazy v)
    with Lex.NoParse -> next env
       | Lex.Give_up m -> next_msg m env

(** Sequence combinator. *)
let seq : 'a t -> ('a -> 'b) t -> 'b t = fun g1 g2 env k ->
    g1 env (ink (fun env x -> g2 env (lrg k x)))

(** Dependant sequence combinator. *)
let dseq : ('a * 'b) t -> ('a -> ('b -> 'c) t) -> 'c t =
  fun g1 g2 env k ->
    g1 env (ink(fun env vs ->
        (try
           let (v1,v2) = Lazy.force vs in
           (** This forces the evaluation of v2 ... no consequence
              on right recursion *)
           let g = g2 v1 in
           fun () -> g env (arg k v2)
         with Lex.NoParse -> fun () -> next env
            | Lex.Give_up m -> fun () -> next_msg m env) ()))

(** [test cs env]  returns [true] if and only if the next  character to parse in
    the environment [env] is in the character set [cs]. *)
let test cs e = Charset.mem cs (Input.get e.current_buf e.current_pos)

(** option combinator,  contrary to [alt] apply to [empty],  it uses the charset
    of the  continuation for prediction. Therefore  it is preferable not  to use
    empty in [alt] and use [option] instead.*)
let option: 'a -> Charset.t -> 'a t -> 'a t = fun x cs1 g1 ->
  fun env k ->
    if test cs1 env then add_queue env (Gram(env,g1,k));
    call k env (lazy x)

(** Alternatives combinator. *)
let alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t = fun cs1 g1 cs2 g2 ->
  fun env k ->
    match (test cs1 env, test cs2 env) with
    | (false, false) -> next env
    | (true , false) -> g1 env k
    | (false, true ) -> g2 env k
    | (true , true ) -> add_queue env (Gram(env,g2,k)); g1 env k

let split_list l =
  let rec fn acc1 acc2 =
    function [] -> acc1, acc2
           | x::l -> fn (x::acc2) acc1 l
  in
  fn [] [] l

let alts : type a. (Charset.t * a t) list -> a t = fun l ->
  let rec fn = function
     | [] -> (Charset.empty, fail)
     | [(cs,g)] -> (cs, g)
     | l -> let (l1,l2) = split_list l in
            let (cs1,c1) = fn l1 in
            let (cs2,c2) = fn l2 in
            (Charset.union cs1 cs2, alt cs1 c1 cs2 c2)
  in snd (fn l)


(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn env k ->
    g env (app k fn)

(** combinator that  forces immediate evaluation of the action,  for instance if
   the semantics update a table *)
let eval : 'a t -> 'a t = fun g env k ->
  g env (ink (fun env v ->
             try ignore (Lazy.force v); call k env v
             with Lex.NoParse -> next env
                | Lex.Give_up m -> next_msg m env))

(** Combinator to test the input before parsing with a grammar *)
let test_before : (Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t =
  fun test g env k ->
    match test env.buf_before_blanks env.pos_before_blanks
            env.current_buf env.current_pos
    with false -> next env
       | true  -> g env k

(** Combinator to test the input after parsing with a grammar *)
let test_after : ('a -> Lex.buf -> Lex.pos -> Lex.buf -> Lex.pos -> bool)
                 -> 'a t -> 'a t =
  fun test g env k ->
    let k = ink (fun env x ->
      match test (Lazy.force x) env.buf_before_blanks env.pos_before_blanks
             env.current_buf env.current_pos
      with false -> next env
         | true  -> call k env x)
    in
    g env k

(** Read the position after parsing. *)
let right_pos : type a.(Pos.t -> a) t -> a t = fun g env k ->
    g env (posk k)

(** Read the position before parsing. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g  env k ->
    let pos = Pos.get_pos env.current_buf env.current_pos in
    g env (arg k pos)

(** Read left pos from the lr table. *)
let read_pos : Pos.t Assoc.key -> (Pos.t -> 'a) t -> 'a t =
  fun key g env k ->
    let pos = try Assoc.find key env.lr with Not_found -> assert false in
    g env (arg k pos)

(** key used by lr below *)
type 'a key = 'a Lazy.t Assoc.key

(** [lr g  gf] is the combinator used to  eliminate left recursion. Intuitively,
    it parses using  the "grammar" [g gf*].  An equivalent  combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)].
*)
let lr : 'a t -> 'a key -> 'a t -> 'a t = fun g key gf env k ->
    let rec klr env v =
      add_queue env (Cont(env,k,v));
      let lr = Assoc.add key v env.lr in
      let env0 = { env with lr } in
      gf env0 (ink klr)
    in
    g env (ink klr)

(** Same as above but incorporating the reading of the left position, stored
    in the lr table too. *)
let lr_pos : 'a t -> 'a key -> Pos.t Assoc.key -> 'a t -> 'a t =
  fun g key pkey gf env k ->
    let pos = Pos.get_pos env.current_buf env.current_pos in
    let rec klr env v =
      add_queue env (Cont(env,k,v));
      let lr = Assoc.add key v env.lr in
      let lr = Assoc.add pkey pos lr in
      let env0 = { env with lr } in
      gf env0 (ink klr);
    in
    g env (ink klr)

(** type to represent the left prefix of a mutually recursive grammar.
    the key represents the produced grammar for each left prefix. *)
type mlr_left =
  LNil : mlr_left
| LCns : 'a key * Charset.t * 'a t * mlr_left -> mlr_left

(** type of the suffix to be repeted in a mutually recursive grammar.
    the first key represents the grammar that parsed the input before
    the second key represents the produced grammar.

    Somehow, mlr_right is a matrix R, the two keys being the index of
    the coefficient and mlr_left is a vector L. Parsing, will somehow use
    L R^n for n large enough;
*)
type mlr_right =
  RNil : mlr_right
| RCns : 'a key * 'b key * Charset.t * 'b t * mlr_right -> mlr_right

(** select the useful right prefix to continue parsing, and return
    the result reusing the type mlr_left *)
let select : type a. mlr_right -> a key -> mlr_left = fun l k ->
  let rec fn : mlr_right -> mlr_left -> mlr_left =
    fun l acc ->
    match l with
    | RNil -> acc
    | RCns(k',kr,cs,g,r) ->
       match k.eq k'.tok with
         | Assoc.Eq  -> fn r (LCns(kr,cs,g,acc))
         | Assoc.NEq -> fn r acc
  in
  fn l LNil

(** semantic value when parsing mutually left recursive grammar.
    contain the key identifying the grammar, the semantics and
    the grammar to ocntinue parsing if possible *)
type mlr_res =
  Res : 'a key * 'a lazy_t * mlr_res t -> mlr_res

(** precompilation of mutualy left recursive grammars *)
let compile_mlr : mlr_left -> mlr_right -> mlr_res t = fun gl gr ->
  let adone = ref [] in
  let rec g1 : mlr_left -> (Charset.t * mlr_res t) list = function
    | LNil -> []
    | LCns(key,cs,g,r) ->
       let l = get key in
       let g env k =
         g env (ink(fun env v ->
                    let v = lazy (Res(key,v,Lazy.force l)) in call k env v))
       in
       (cs, g) :: g1 r

  and g0 : mlr_left -> mlr_res t = fun l -> alts (g1 l)

  and get : type a. a key -> mlr_res t Lazy.t = fun k ->
    try List.assq (Assoc.K k) !adone
    with Not_found ->
      let g = lazy (g0 (select gr k)) in
      adone := (Assoc.K k, g) :: !adone ;
      g
  in
  g0 gl

(* the main combinator for mutually left recursive grammars *)
let mlr : type a. ?lpos:Pos.t Assoc.key ->
               mlr_left -> mlr_right -> a key -> a t =
  fun ?lpos gl gr fkey ->
    let g = compile_mlr gl gr in
    fun env k ->
      let pos = match lpos with
        | None   -> Pos.phantom
        | Some _ -> Pos.get_pos env.current_buf env.current_pos
      in
      let rec klr env (lazy (Res(key,v,g'))) =
        begin
          match fkey.eq key.tok with
          | Assoc.Eq  -> add_queue env (Cont(env,k,v));
          | Assoc.NEq -> ()
        end;
        let lr = Assoc.add key v env.lr in
        let lr = match lpos with
          | None -> lr
          | Some pkey -> Assoc.add pkey pos lr
        in
        let env0 = { env with lr } in
        g' env0 (ink klr)
      in
      g env (ink klr)

(** combinator to access the value stored by lr*)
let read_tbl : 'a key -> 'a t = fun key env k ->
    let v = try Assoc.find key env.lr with Not_found -> assert false in
    call k env v

(** Combinator under a refrerence used to implement recursive grammars. *)
let deref : 'a t ref -> 'a t = fun gref env k -> !gref env k

(** Combinator changing the "blank function". *)
let change_layout : ?config:Blank.layout_config -> Blank.t -> 'a t -> 'a t =
    fun ?(config=Blank.default_layout_config) blank_fun g env k ->
    let (s, n, _) as buf=
      if config.old_blanks_before then (env.current_buf, env.current_pos, false)
      else (env.buf_before_blanks, env.pos_before_blanks, true)
    in
    let (s, n, moved) =
      if config.new_blanks_before then let (s,n) = blank_fun s n in (s,n,true)
      else buf
    in
    let old_blank_fun = env.blank_fun in
    let env = { env with blank_fun ; current_buf = s ; current_pos = n } in
    g env (ink (fun env v ->
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
      (** return to scheduler if we moved in the input *)
      if moved then Cont(env,k,v) else call k env v))

(** {2 The cache/merge combinator } *)

(** Combinator for caching a grammar, to avoid exponential behavior. *)
let cache : type a. ?merge:(a -> a -> a) -> a t -> a t = fun ?merge g ->
  (** creation of a table for the cache *)
  let cache = Input.Tbl.create () in
  fun env0 k ->
  let {current_buf = buf0; current_pos = col0} = env0 in
  try
    (** Did we start parsing the same grammar at the same position *)
    let (ptr, too_late) = Input.Tbl.find cache buf0 col0 in
    (** If yes we store the continuation in the returned pointer.
       !too_late is true if we already called the continuation stored
       in !ptr *)
    assert (not !too_late);
    ptr := (k, env0.cache_order) :: !ptr;
    (** Nothing else to do nw, try the other branch of parsing *)
    raise Exit
  with Not_found ->
    (** This is the first time we parse with this grammar at this position,
       we add an entry in the cache *)
    (** NOTE: size of ptr: O(N) for each position,
         it comes from a rule using that grammar (nb of rule constant),
         and the start position of this rule (thks to cache, only one each)  *)
    let ptr = ref [(k,env0.cache_order)] in
    let too_late = ref false in
    Input.Tbl.add cache buf0 col0 (ptr, too_late);
    (** we create a merge table for all continuation to call merge on all
         semantics before proceding. To do so, we need to complete all parsing
         of this grammar at this position before calling the continuation.
         [vnum] will be used to ensure this, see below. *)
    let merge_tbl = Input.Tbl.create () in
    (** NOTE: merge_tbl is not used if merge = None *)
    let co = Input.byte_pos buf0 col0 in
    assert(co >= fst env0.cache_order);
    let cache_order = if fst env0.cache_order = co
                      then (co, snd env0.cache_order + 1)
                      else (co, 0)
    in
    (** we update cache_order in the environment, ensuring the correct order in
         the scheduler. we must evaluate first grammar that were started later
         in the input buffer, or later in time if at the same position *)
    let env = { env0 with cache_order } in
    let k0 env v =
      (** the cache order must have been restored to its initial value *)
      assert (cache_order = env.cache_order);
      let {current_buf = buf; current_pos = col} = env in
      try
        (** we first try to merge ... if merge <> None *)
        if merge = None then raise Not_found;
        (** We get the vptr to share this value, if any *)
        let (vptr,too_late_v) = Input.Tbl.find merge_tbl buf col in
        (** and it is not too late to add a semantics for this action, that is
           the action was not evaluated *)
        assert (not !too_late_v);
        (** NOTE: size of vptr: O(N) at each position: number of ways to end
             parsing of this grammar. Beware, vptr are distinct for distinct
             start position of parsing. So total number of vptr is O(N^2). It
             can be more if you are not using enough cache.  *)
        vptr := v :: !vptr;
        (** No need to continue parsing, we try other branches *)
        raise Exit
      with Not_found ->
        (** we merge all semantics if merge <> None *)
        let v = match merge with
          | None -> v (** No merge, no sharing *)
          | Some merge ->
             (** create vptr and register in merge_tbl *)
             let vptr = ref [v] in
             let too_late_v = ref false in
             Input.Tbl.add merge_tbl buf col (vptr,too_late_v);
             (** the semantics merge together all value in vptr, Once forced,
                  too_late_v ensure an assertion failure if we try to extend
                  vptr after evaluation *)
             lazy (
                 too_late_v := true;
                 (** NOTE: we keep only one give_up message *)
                 let msg = ref None in
                 (** do not forget to deal with give_up *)
                 let merge x v =
                   try let y = Lazy.force v in
                       match x with None -> Some y
                                  | Some x -> Some (merge x y)
                   with Lex.NoParse -> x
                      | Lex.Give_up m -> msg := Some m; x
                 in
                 match List.fold_left merge None !vptr with
                 | None -> (match !msg with None   -> raise Lex.NoParse
                                          | Some m -> Lex.give_up ~msg:m ())
                 | Some x -> x)
        in
        (** Now we call all continuation stored in !ptr *)
        let l0 = !ptr in
        too_late := true;
        List.iter (fun (k,cache_order) ->
            add_queue env
              (** we pop cache_order to ensure this continuation
                  is called after all extensions of vptr *)
              (Cont({ env with cache_order },k,v))) l0;
        raise Exit
    in
    (** safe to call g, env had cache pushed so it is a minimum *)
    g env (ink k0)

(** {2 functions to do the actual parsing *)

(** function doing the parsing *)
let gen_parse_buffer
    : type a. a t -> Blank.t -> ?blank_after:bool
                  -> Lex.buf -> Lex.pos -> (a * Lex.buf * Lex.pos) list =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    (** environment initialisation *)
    let p0 = Input.char_pos buf0 col0 in
    let max_pos = ref (p0, buf0, col0, ref []) in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; pos_before_blanks = col0
        ; current_buf = buf ; current_pos = col; lr = Assoc.empty
        ; max_pos ; blank_fun ; cache_order = (-1,0)
        ; queue = ref E }
    in
    (** calling the scheduler to start parsing *)
    let r = scheduler env g in
    match r with
    | [] ->
       (** error managment *)
       let (_, buf, col, msgs) = !max_pos in
       let msgs = List.sort_uniq compare !msgs in
       raise (Pos.Parse_error(buf, col, msgs))
    | _ ->
       (** finalisation *)
       List.map (fun (v,env) ->
           if blank_after then (v, env.current_buf, env.current_pos)
           else (v, env.buf_before_blanks, env.pos_before_blanks)) r

(** parse for non ambiguous grammars, allows for continue parsing *)
let partial_parse_buffer : type a. a t -> Blank.t -> ?blank_after:bool
                                -> Lex.buf -> Lex.pos -> a * Lex.buf * Lex.pos =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let l = gen_parse_buffer g blank_fun ~blank_after buf0 col0 in
    match l with
    | [r]  -> r
    | r::_ -> Printf.eprintf "Parsing ambiguity, use cache with merge\n%!"; r
    | []   -> assert false

(** Parse for ambiguous grammar with no merge, return all values. If end of
    input is not parsed in some ways, some value may correspond to only the
    beginning of the input. You should rather use cache/merge anyway.*)
let parse_all_buffer
    : type a. a t -> Blank.t -> Lex.buf -> Lex.pos -> a list =
  fun g blank_fun buf0 col0 ->
    let l = gen_parse_buffer g blank_fun buf0 col0 in
    List.map (fun (r,_,_) -> r) l
