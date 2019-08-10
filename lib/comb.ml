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
  ; max_pos           : (Lex.buf * int * string list ref) ref
  (** Maximum position reached by the parser (for error reporting). *)
  ; current_buf       : Input.buffer
  (** Current input buffer (or input stream). *)
  ; current_col       : int
  (** Current column number in buffer [current_buf]. *)
  ; buf_before_blanks : Input.buffer
  (** Input buffer before reading the blanks. *)
  ; col_before_blanks : int
  (** Column number in [buf_before_blanks] before reading the blanks. *)
  ; lr                : Assoc.t
  (** Association table for the lr combinator *)
  }

(** Type of a function called in case of error. *)
type err = unit -> res

(**  type  of result  used  by  the scheduler  to  progress  in the  parsing  in
    parallel *)
 and res =
   | Cont : env * 'a cont * err * 'a -> res
   (** returned by lexeme instead of calling the continuation, contains all
       information to continue parsing. *)
   | Skip : res
   (** used by the cache combinator to tell there is nothing to do,
       we are already parsing the same grammar at the same position. *)

 (** Type  of a  parsing continuation. A  value of type  ['a cont]  represents a
    function waiting for a parsing environment, an error function and a value of
    type ['a] to continue parsing. To avoid quadratic behavior with mainly right
    recursion, this is splitted in two:

    - a transformer  of type [('a,'b,  [`Ct]) trans] reprseting a  function from
      ['a] to ['b]

    - the real continuation  waiting a value of type ['b  app], which represents
      a value of type ['b] obtained by an application of a transformer. *)
 and 'a cont =
   | C : (env -> err -> 'b app -> res) * ('a,'b,[`Ct]) trans
         -> 'a cont [@unboxed]
   | P : (env -> err -> 'b app -> res) * ('a,'b,[`Ct]) trans * Pos.t ref
         -> 'a cont [@unboxed]
 (** [P] is used when the position when calling the continuation (right position
       of some grammar) is needed. *)

 and 'a app = A : ('b,'a,[`At]) trans * 'b -> 'a app
 (** A(b,tr)  represent the value  of type ['a]  obtained by application  of the
     transformer [tr] to [b] *)

 (** [('a,'b,'t) args] is the type of a transformer from a value of type ['a] to
    a value of type ['b]. The third  parameter is a phantom type, either [[`Ct]]
    of [[`At]] to  control that the [Cns]  constructor is only used  in value of
    type ['a app] and not value of type ['a cont]. *)
 and (_,_,_) trans =
   | Idt : ('a,'a,'t) trans
   (** Identity transformer *)
   | Arg : ('b, 'c,'t) trans * 'a -> ('a -> 'b,'c,'t) trans
   (** [Arg(tr,x)] tranform a value of type ['a -> 'b] into a value of
       type ['c] by applying it to [x] and then applying the transformer [tr] *)
   | Lrg : ('b, 'c,'t) trans * 'a app -> ('a -> 'b,'c,'t) trans
   (** Same as above but [x] will results of the application of a transformer.
       [Lrg] means lazy arg *)
   | Pos : ('b, 'c,'t) trans * Pos.t ref -> (Pos.t -> 'b,'c,'t) trans
   (** Same  as arg, but [x]  is a position that  will be stored in  a reference
       when calling the continuation *)
   | App : ('b, 'c, 't) trans * ('a -> 'b) -> ('a,'c,'t) trans
   (** [App(tr,f) transform  a value of type  ['a] into a value of  type ['c] by
        passing it to a [f] and then using [tr] *)
   | Cns : ('b,'c,[`Ct]) trans * ('a, 'b,[`At]) trans -> ('a,'c,[`At]) trans
   (** composition of two transformers *)

 (** Type of a parser combinator with a semantic action of type ['a]. the return
    type [res] will be used by the scheuler function below to drive the
    parsing. *)
and 'a t = env -> 'a cont -> err -> res

(** continuations and trans functions *)

(** construction of a continuation with an identity transformer *)
let ink f = C(f,Idt)

(** function calling a  continuation. It does not evaluate any  action. It is of
    crucial importane that this function be in O(1). *)
let call : type a.a cont -> env -> err -> a app -> res =
  fun k env err x ->
    match (k,x) with
    | C(k,tr2)   , A(tr1,x) -> k env err (A(Cns(tr2, tr1),x))
    | P(k,tr2,rp), A(tr1,x) ->
       rp := Pos.get_pos env.buf_before_blanks env.col_before_blanks;
       k env err (A(Cns(tr2, tr1),x))

(** access to transformer constructor inside the continuation constructor *)
let arg : type a b. b cont -> a -> (a -> b) cont = fun k x ->
    match k with
    | C(k,tr)    -> C(k,Arg(tr,x))
    | P(k,tr,rp) -> P(k,Arg(tr,x),rp)

let larg : type a b. b cont -> a app -> (a -> b) cont = fun k x ->
    match k with
    | C(k,tr)    -> C(k,Lrg(tr,x))
    | P(k,tr,rp) -> P(k,Lrg(tr,x),rp)

let app : type a b. b cont -> (a -> b) -> a cont = fun k f ->
    match k with
    | C(k,tr)    -> C(k,App(tr,f))
    | P(k,tr,rp) -> P(k,App(tr,f),rp)

(** evaluation function for the [app] type *)
let rec eval : type a. a app -> a = fun (A(tr,x)) ->
  let rec fn : type a b x. a -> (a,b,x) trans -> b = fun x tr ->
    match tr with
    | Idt        -> x
    | Arg(tr,y)  -> fn (x y) tr
    | Lrg(tr,y)  -> fn (x (eval y)) tr
    | Pos(tr,p)  -> fn (x !p) tr
    | App(tr,f)  -> fn (f x) tr
    | Cns(t1,t2) -> fn (fn x t2) t1
  in
  fn x tr

(** transforsms [Lrg] into [Arg] inside a continuation *)
let eval_lrgs : type a. a cont -> a cont = fun k ->
  let rec fn : type a b. (a,b,[`Ct]) trans -> (a,b,[`Ct]) trans = function
    | Idt       -> Idt
    | Arg(tr,x) -> Arg(fn tr, x)
    | Lrg(tr,x) -> Arg(fn tr, eval x)
    | Pos(tr,p) -> Pos(fn tr,p)
    | App(tr,x) -> App(fn tr,x)
  in
  match k with
  | C(k,tr)    -> C(k,fn tr)
  | P(k,tr,rp) -> P(k,fn tr,rp)

(** [next env  err] updates the current maximum position  [env.max_pos] and then
    calls the [err] function. *)
let next : env -> err -> res  = fun env err ->
  let (buf_max, col_max, _) = !(env.max_pos) in
  let line_max = Input.line buf_max in
  let line = Input.line env.current_buf in
  if line > line_max || (line = line_max && env.current_col > col_max) then
    env.max_pos := (env.current_buf, env.current_col, ref []);
  err ()

let next_msg : string -> env -> err -> res  = fun msg env err ->
  let (buf_max, col_max, msgs) = !(env.max_pos) in
  let line_max = Input.line buf_max in
  let line = Input.line env.current_buf in
  if line > line_max || (line = line_max && env.current_col > col_max) then
    env.max_pos := (env.current_buf, env.current_col, ref [msg])
  else if line = line_max && env.current_col = col_max then
    msgs := msg :: !msgs;
  err ()

(** the scheduler stores what remains to do in a list sorted by position
    in the buffer, here are the comparison function used for this sorting *)
let before r1 r2 =
  match (r1,r2) with
  | (Cont(env1,_,_,_), Cont(env2,_,_,_)) ->
     let p1 = Input.line_offset env1.current_buf + env1.current_col in
     let p2 = Input.line_offset env2.current_buf + env2.current_col in
     p1 <= p2
  | _ -> assert false

let same r1 r2 =
  match (r1,r2) with
  | (Cont(env1,_,_,_), Cont(env2,_,_,_)) ->
    let p1 = Input.line_offset env1.current_buf + env1.current_col in
    let p2 = Input.line_offset env2.current_buf + env2.current_col in
    p1 = p2
  | _ -> assert false

(** insert in a list at the correct position *)
let insert : res -> res list -> res list = fun r l ->
  let rec fn acc = function
      | [] -> List.rev (r::acc)
      | r0::_ as l when before r r0 -> List.rev_append acc (r::l)
      | r0::l -> fn (r0::acc) l
  in
  fn [] l

(** extract all results at the first position *)
let extract : res list -> res list * res list = function
  | [] -> raise Exit
  | r :: l ->
     let rec fn acc = function
       | [] -> acc, []
       | r0::l when same r r0 -> fn (r0::acc) l
       | l -> acc, l
     in fn [r] l

(** [scheduler  env g] drives  the parsing, it calls  the combinator [g]  in the
    given environment and when lexeme returns to the scheduler, it continues the
    parsing,  but  trying the error case too,  this way all  parsing progress in
    parallel in the input. *)
let scheduler : ?all:bool -> env -> 'a t -> ('a * env) list =
  fun ?(all=false) env g ->
    (* a reference holding the final result *)
    let res = ref [] in
    (* the final continuation evaluating and storing the result,
       continue parsing if [all] is [true] *)
    let k env err x =
      (try
         res := (eval x,env)::!res;
         if all then next env err else raise Exit
       with Lex.NoParse -> next env err);
    in
    try
      (* calls to the initial grammar and initialise the table *)
      let r = g env (ink k) (fun _ -> raise Exit) in
      let tbl = ref [r] in  (* to do at further position *)
      while true do
        let t0,t1 = extract !tbl in
        tbl := t1;
        List.iter (function
            | Skip -> assert false
            | Cont(env,k,err,x) ->
               (* calling the error and the continuation, storing the result in
              tbl1. *)
               (try
                  let r = next env err in
                  if r <> Skip then tbl := insert r !tbl
                with Exit -> ());
               (try
                  let r = call k env (fun _ -> raise Exit) (A(Idt,x)) in
                  if r <> Skip then tbl := insert r !tbl
                with Exit -> ())) t0
      done;
      assert false
    with Exit -> !res

(** Combinator that always fails. *)
let fail : 'a t = fun env _ err -> next env err

(** Fails and report an error *)
let error : string -> 'a t = fun msg env _ err -> next_msg msg env err

(** Combinator used as default fied before compilation *)
let assert_false : 'a t = fun _ _ _ -> assert false

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x env kf err -> call kf env err (A(Idt,x))

(** Combinator accepting the given lexeme (or terminal). *)
let lexeme : 'a Lex.lexeme -> 'a t = fun lex env k err ->
    (try
       let (v, buf_before_blanks, col_before_blanks) =
         lex env.current_buf env.current_col
       in
       let (current_buf, current_col) =
         env.blank_fun buf_before_blanks col_before_blanks
       in
       fun () ->
         let k = eval_lrgs k in
         let env =
           { env with buf_before_blanks ; col_before_blanks
                    ; current_buf ; current_col; lr = Assoc.empty }
         in
         Cont(env,k, err, v)
     with Lex.NoParse -> fun () -> next env err) ()

(** Same as above but does not return to the scheduler, to use if
    one knows the error has not changed, so the scheduler will do
    nothing. *)
let direct_lexeme : 'a Lex.lexeme -> 'a t = fun lex env k err ->
    (try
       let (v, buf_before_blanks, col_before_blanks) =
         lex env.current_buf env.current_col
       in
       let (current_buf, current_col) =
         env.blank_fun buf_before_blanks col_before_blanks
       in
       fun () ->
         let k = eval_lrgs k in
         let env =
           { env with buf_before_blanks ; col_before_blanks
                    ; current_buf ; current_col; lr = Assoc.empty }
         in
         call k env err (A(Idt,v))
     with Lex.NoParse -> fun () -> next env err) ()

(** Sequence combinator. *)
let seq : 'a t -> ('a -> 'b) t -> 'b t = fun g1 g2 env k err ->
  g1 env (ink (fun env err x -> g2 env (larg k x) err)) err

(** Dependant sequence combinator. *)
let dseq : ('a * 'b) t -> ('a -> ('b -> 'c) t) -> 'c t =
  fun g1 g2 env k err ->
    g1 env (ink(fun env err vs ->
        (try
           let (v1,v2) = eval vs in
           (* This forces the evaluation of v2 ... no consequence
              on right recursion *)
           let g = g2 v1 in
           fun () -> g env (arg k v2) err
         with Lex.NoParse -> fun () -> next env err) ())) err

(** [test cs env] returns [true] if and only if the next character to parse in
    the environment [env] is in the character set [cs]. *)
let test cs e = Charset.mem cs (Input.get e.current_buf e.current_col)

(** option combinator,  contrary to [alt] apply to [empty],  it uses the charset
    of the  continuation for prediction. Therefore  it is preferable not  to use
    empty in [alt] and use [option] instead.*)
let option: 'a -> Charset.t -> 'a t -> 'a t = fun x cs1 g1 ->
  fun env k err ->
    if test cs1 env then g1 env k (fun () -> call k env err (A(Idt,x)))
    else call k env err (A(Idt,x))

(** Alternatives combinator. *)
let alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t = fun cs1 g1 cs2 g2 ->
  fun env k err ->
    match (test cs1 env, test cs2 env) with
    | (false, false) -> next env err
    | (true , false) -> g1 env k err
    | (false, true ) -> g2 env k err
    | (true , true ) -> g1 env k (fun () -> g2 env k err)

(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn env k err ->
    g env (app k fn) err

(** Read the position after parsing. *)
let right_pos : type a.(Pos.t -> a) t -> a t = fun g env k err ->
    let k = match k with
      | C(k,tr)    -> let rp = ref Pos.phantom in P(k,Pos(tr,rp),rp)
      | P(k,tr,rp) -> P(k,Pos(tr,rp),rp)
    in
    g env k err

(** Read the position before parsing. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g  env k err ->
    let pos = Pos.get_pos env.current_buf env.current_col in
    g env (arg k pos) err

(** Read lpos from the lr table. *)
let read_pos : Pos.t Assoc.key -> (Pos.t -> 'a) t -> 'a t =
  fun key g env k err ->
    let pos = try Assoc.find key env.lr with Not_found -> assert false in
    g env (arg k pos) err

(** key used by lr below *)
type 'a key = 'a app Assoc.key

(** [lr g  gf] is the combinator used to  eliminate left recursion. Intuitively,
    it parses using  the "grammar" [g gf*].  An equivalent  combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)].
    NOTE: left recusion forces evaluation and this is good!
*)
let lr : 'a t -> 'a key -> 'a t -> 'a t = fun g key gf env k err ->
    let rec klr env err v =
      let err () =
        (try
            let lr = Assoc.add key v env.lr in
            let env0 = { env with lr } in
            fun () -> gf env0 (ink klr) err
          with
            Lex.NoParse -> err) ()
        in
        call k env err v
    in
    g env (ink klr) err

let lr_pos : 'a t -> 'a key -> Pos.t Assoc.key -> 'a t -> 'a t =
  fun g key pkey gf env k err ->
    let pos = Pos.get_pos env.current_buf env.current_col in
    let rec klr env err v =
      let err () =
        (try
            let lr = Assoc.add key v env.lr in
            let lr = Assoc.add pkey pos lr in
            let env0 = { env with lr } in
            fun () -> gf env0 (ink klr) err
          with
            Lex.NoParse -> err) ()
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

type layout_config =
  { old_blanks_before : bool
  (** Ignoring blanks with the old blank function before parsing? *)
  ; new_blanks_before : bool
  (** Then ignore blanks with the new blank function (before parsing)? *)
  ; new_blanks_after  : bool
  (** Use the new blank function one last time before resuming old layout? *)
  ; old_blanks_after  : bool
  (** Use then the old blank function one last time as well? *) }

let default_layout_config : layout_config =
  { old_blanks_before = true
  ; new_blanks_before = false
  ; new_blanks_after  = false
  ; old_blanks_after  = true }

(** Combinator changing the "blank function". *)
let change_layout : ?config:layout_config -> Lex.blank -> 'a t -> 'a t =
    fun ?(config=default_layout_config) blank_fun g env k err ->
    let (s, n) as buf =
      if config.old_blanks_before then (env.current_buf, env.current_col)
      else (env.buf_before_blanks, env.col_before_blanks)
    in
    let (s, n) =
      if config.new_blanks_before then blank_fun s n
      else buf
    in
    let old_blank_fun = env.blank_fun in
    let env = { env with blank_fun ; current_buf = s ; current_col = n } in
    g env (ink (fun env err v ->
      let (s, n) as buf =
        if config.new_blanks_after then (env.current_buf, env.current_col)
        else (env.buf_before_blanks, env.col_before_blanks)
      in
      let (s, n) =
        if config.old_blanks_after then old_blank_fun s n
        else buf
      in
      let env =
        { env with blank_fun = old_blank_fun
        ; current_buf = s ; current_col = n }
      in
      call k env err v)) err (* NOTE: here, ok to call call *)

(** Combinator for caching a grammar, to avoid exponential behavior.
    very bad performance with a non ambiguous right recursive grammar. *)
let cache : type a. a t -> a t = fun g ->
  let cache = Input.Tbl.create () in
  fun env0 k err ->
    let {current_buf = buf0; current_col = col0; _} = env0 in
    try
      let ptr = Input.Tbl.find cache buf0 col0 in
      begin
        match !ptr with
        | (_, false) -> assert false (* Too late ?*)
        | (l, _    ) -> ptr := (k :: l, true)
      end;
      Skip
    with Not_found ->
      let ptr = ref ([k], true) in
      Input.Tbl.add cache buf0 col0 ptr;
      let k0 env err v =
        if snd !ptr then ptr := (fst !ptr, false);
        let rec fn = function
          | [] -> next env err
          | k :: l -> call k env (fun () -> fn l) v
        in
        fn (fst !ptr)
      in
      g env0 (ink k0) err

(** function doing the parsing *)
let gen_parse_buffer
    : type a. a t -> ?all:bool -> Lex.blank -> ?blank_after:bool
                  -> Lex.buf -> int -> (a * Lex.buf * int) list =
  fun g ?(all=false) blank_fun ?(blank_after=false) buf0 col0 ->
    let max_pos = ref (buf0, col0, ref []) in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; col_before_blanks = col0
        ; current_buf = buf ; current_col = col; lr = Assoc.empty
        ; max_pos ; blank_fun ; }
    in
    let r = scheduler ~all env g in
    match r with
    | [] ->
       let (buf, col, msgs) = !max_pos in
       let msgs = List.sort_uniq compare !msgs in
       raise (Pos.Parse_error(buf, col, msgs))
    | _ -> List.map (fun (v,env) ->
               if blank_after then (v, env.current_buf, env.current_col)
               else (v, env.buf_before_blanks, env.col_before_blanks)) r

(** the two main variation of the above *)
let partial_parse_buffer : type a. a t -> Lex.blank -> ?blank_after:bool
                                -> Lex.buf -> int -> a * Lex.buf * int =
  fun g blank_fun ?(blank_after=false) buf0 col0 ->
    let l = gen_parse_buffer g blank_fun ~blank_after buf0 col0 in
    match l with [r] -> r
               | _ -> assert false

let parse_all_buffer : type a. a t -> Lex.blank -> Lex.buf -> int -> a list =
  fun g blank_fun buf0 col0 ->
    let l = gen_parse_buffer g ~all:true blank_fun buf0 col0 in
    List.map (fun (r,_,_) -> r) l
