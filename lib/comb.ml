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
      memory. This is solved by  terminals returning immediately instead of call
      the continuation  and a "scheduler"  will store the continuation  and call
      the error function (we use  continuations and errors). This forces parsing
      all terminals in parallel. This also gives a very nice cache combinator.
*)

(** Environment holding information require for parsing. *)
type env =
  { blank_fun         : Lex.blank
  (** Function used to ignore blanks. *)
  ; left_pos_stack    : Pos.t list
  (** Used to get correct position when eliminating left recursion *)
  ; max_pos           : (Lex.buf * int) ref
  (** Maximum position reached by the parser (for error reporting). *)
  ; current_buf       : Input.buffer
  (** Current input buffer (or input stream). *)
  ; current_col       : int
  (** Current column number in buffer [current_buf]. *)
  ; buf_before_blanks : Input.buffer
  (** Input buffer before reading the blanks. *)
  ; col_before_blanks : int
  (** Column number in [buf_before_blanks] before reading the blanks. *)
  ; lr                : Assoc.t }

(** Type of a function called in case of error. *)
type err = unit -> res

(**  type  of result  used  by  the scheduler  to  progress  in the  parsing  in
    parallel *)
 and res =
   | Term : env * 'a cont * err option * 'a -> res
   | Skip : res (* used by the cache combinator to tell there is nothing to do,
                   we are already parsing the same grammar at the same position. *)

(** Type of a parsing continuation. *)
and 'a cont = env -> err -> 'a -> res

(** Type of a  parser combinator with a semantic action of  type ['a]. They type
    ['r] represents the  type of the continuation, it  is universally quantified
    in the the definition of the type ['a t] below. *)
and 'a t = env -> 'a cont -> err -> res

(** [next env  err] updates the current maximum position  [env.max_pos] and then
    calls the [err] function. *)
let next : env -> err -> res  = fun env err ->
  let (buf_max, col_max) = !(env.max_pos) in
  let line_max = Input.line buf_max in
  let line = Input.line env.current_buf in
  if line > line_max || (line = line_max && env.current_col > col_max) then
    env.max_pos := (env.current_buf, env.current_col);
  err ()

let before r1 r2 =
  match (r1,r2) with
  | (Term(env1,_,_,_), Term(env2,_,_,_)) ->
     let p1 = Input.line_offset env1.current_buf + env1.current_col in
     let p2 = Input.line_offset env2.current_buf + env2.current_col in
     p1 <= p2
  | _ -> assert false

let same r1 r2 =
  match (r1,r2) with
  | (Term(env1,_,_,_), Term(env2,_,_,_)) ->
    let p1 = Input.line_offset env1.current_buf + env1.current_col in
    let p2 = Input.line_offset env2.current_buf + env2.current_col in
    p1 = p2
  | _ -> assert false

let insert : res -> res list -> res list = fun r l ->
  let rec fn acc = function
      | [] -> List.rev (r::acc)
      | r0::_ as l when before r r0 -> List.rev_append acc (r::l)
      | r0::l -> fn (r0::acc) l
  in
  fn [] l

let extract : res list -> res list * res list = function
  | [] -> [], []
  | r :: l ->
     let rec fn acc = function
       | [] -> acc, []
       | r0::l when same r r0 -> fn (r0::acc) l
       | l -> acc, l
     in fn [r] l

let scheduler : ?all:bool -> env -> 'a t -> ('a * env) list = fun ?(all=false) env g ->
  let res = ref [] in
  let k env err x =
    (try
       res := (x,env)::!res;
       if all then err () else raise Exit
    with Lex.NoParse -> err ());
  in
  try
    let r = g env k (fun _ -> raise Not_found) in
    let tbl0 = ref [r] in
    let tbl1 = ref [] in
    while true do
      match !tbl0 with
      | [] -> (match !tbl1 with
               | [] -> raise Exit
               | _  -> let t0,t1 = extract !tbl1 in tbl0 := t0; tbl1 := t1)
      | Skip :: _ -> assert false
      | (Term(env,k,Some err,x)) :: l ->
                                 tbl0 := Term(env,k,None,x)::l;
                                 (try
                                   let r = err () in
                                   if r <> Skip then tbl1 := insert r !tbl1
                                 with Not_found -> ())
      | (Term(env,k,None,x) as r0) :: l -> tbl0 := l;
                                  (try
                                     let r = k env (fun _ -> raise Not_found) x in
                                     if r <> Skip then
                                       begin
                                         if same r r0 then
                                           tbl0 := r :: !tbl0
                                         else
                                           tbl1 := insert r !tbl1
                                       end
                                  with Not_found -> ())
    done;
    assert false
  with
  | Exit | Not_found -> !res

(** Combinator that always fails. *)
let fail : 'a t = fun env _ err -> next env err

(** Combinator used as default fied before compilation *)
let assert_false : 'a t = fun _ _ _ -> assert false

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x  env kf err -> kf env err x

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
         let env =
           { env with buf_before_blanks ; col_before_blanks
                    ; current_buf ; current_col; lr = Assoc.empty }
         in
         Term (env,k, Some err, v)
     with Lex.NoParse -> fun () -> next env err) ()

(** Sequence combinator. *)
let seq : 'a t -> ('a -> 'b) t -> 'b t = fun g1 g2 env kf err ->
  g1 env (
      fun env err v1 ->
      g2 env (
          fun env err v2 ->
          (try fun () -> kf env err (v2 v1)
           with Lex.NoParse -> fun () -> next env err) ()) err) err

(** Dependant sequence combinator. *)
let dseq : ('a * 'b) t -> ('a -> ('b -> 'c) t) -> 'c t =
  fun g1 g2 env k err ->
    g1 env (fun env err (v1,v2) ->
        (try
           let g = g2 v1 in
           fun () -> g env (fun env err v3 ->
             (try
                fun () -> k env err (v3 v2)
              with Lex.NoParse -> fun () -> next env err) ()) err
         with Lex.NoParse -> fun () -> next env err) ()) err

(** option combinator,  contrary to [alt] apply to [empty],  it uses the charset
    of the  continuation for prediction. Therefore  it is preferable not  to use
    empty in [alt] and use [option] instead.*)
let option: 'a -> 'a t -> 'a t = fun x g1 ->
  let f1 = ref 0 and f2 = ref 0 in
  fun env k err ->
       (* one wants to  avoid incrementing f2 in err or f1 in  err. This way, if
          f1 parses 50% and f2 the other 50%, we have f1 ~ f2 *)
       if !f1 < !f2 then (g1 env k (fun () -> incr f1; k env err x))
       else (k env (fun () -> incr f2; g1 env k err) x)

(** Alternatives combinator. *)
let alt : type a. a t -> a t -> a t = fun g1 g2 ->
  let f1 = ref 0 and f2 = ref 0 in
  fun env k err ->
    (* one wants to avoid incrementing f2 in err or f1 in err. This way, if f1
       parses 50% and f2 the other 50%, we have f1 ~ f2 *)
    if !f1 < !f2 then (g1 env k (fun () -> incr f1; g2 env k err))
    else (g2 env k (fun () -> incr f2; g1 env k err))

(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn env k err ->
    g env (fun env err v ->
      (try let v = fn v in fun () -> k env err v
       with Lex.NoParse -> fun () -> next env err) ()) err

(** functions to access the left position stacks *)
let head_pos n env =
  let rec fn n stack =
    match stack with
    | []                     -> assert false
    | p :: _     when n <= 0 -> p
    | _ :: stack             -> fn (n - 1) stack
  in
  fn n env.left_pos_stack

let tail_pos env =
  match env.left_pos_stack with
  | []         -> assert false
  | _ :: stack -> stack

(** Pushes the current position to the (left position) stack. *)
let push : 'a t -> 'a t = fun g  env k err ->
    let pos = Pos.get_pos env.current_buf env.current_col in
    let env = { env with left_pos_stack = pos :: env.left_pos_stack } in
    let k env err v = k { env with left_pos_stack = tail_pos env } err v in
    g env k err

(** Read the n-th position from the (left position) stack. *)
let read : int -> (Pos.t -> 'a) t -> 'a t = fun n g env k err ->
    let pos = head_pos n env in
    let k env err fn =
      (try let v = fn pos in (fun () -> k env err v)
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g env k err

(** Read the position at the left of the input to be parsed. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g -> push (read 0 g)

(** Read the position after parsing. *)
let right_pos : (Pos.t -> 'a) t -> 'a t = fun g env k err ->
    let k env err fn =
      (try
         let pos = Pos.get_pos env.buf_before_blanks env.col_before_blanks in
         let v = fn pos in fun () -> k env err v
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g env k err

(** [lr g  gf] is the combinator used to  eliminate left recursion. Intuitively,
    it parses using  the "grammar" [g gf*].  An equivalent  combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)]. *)
let lr : 'a t -> 'a Assoc.key -> 'a t -> 'a t = fun g key gf env k err ->
    let rec klr env err v =
      let err () =
          let lr = Assoc.add key v env.lr in
          let env0 = { env with lr } in
          gf env0 klr err
        in
        k env err v
    in
    g env klr err

(** combinator to access the value stored by lr*)
let read_tbl : 'a Assoc.key -> 'a t = fun key env k err ->
    let v = try Assoc.find key env.lr with Not_found -> assert false in
    k env err v


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
    g env (fun env err v ->
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
      k env err v) err

(** Combinator for caching a grammar, to avoid exponential behavior. *)
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
          | [] -> err ()
          | k :: l -> k env (fun () -> fn l) v
        in
        fn (fst !ptr)
      in
      g env0 k0 err

(* NOTE: cs with blank_after = false makes no sense ? *)
let gen_parse_buffer
    : type a. a t -> ?all:bool -> Lex.blank -> ?blank_after:bool
                  -> Lex.buf -> int -> (a * Lex.buf * int) list =
  fun g ?(all=false) blank_fun ?(blank_after=false) buf0 col0 ->
    let max_pos = ref (buf0, col0) in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; col_before_blanks = col0
        ; current_buf = buf ; current_col = col; lr = Assoc.empty
        ; max_pos ; left_pos_stack = [] ; blank_fun ; }
    in
    let r = scheduler ~all env g in
    match r with
    | [] ->
       let (buf, col) = !max_pos in
       raise (Pos.Parse_error(buf, col))
    | _ -> List.map (fun (v,env) ->
               if blank_after then (v, env.current_buf, env.current_col)
               else (v, env.buf_before_blanks, env.col_before_blanks)) r

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
