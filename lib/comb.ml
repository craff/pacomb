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
      same part of the input with the same grammar. *)

(** Environment holding information require for parsing. *)
type 'a env =
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
  ; key               : 'a Assoc.key
  (** Type repr. for keeping the type of the global result after parsing. *)
  ; lr                : Assoc.t }

(** Type of a function called in case of error. *)
type 'b err = unit -> 'b

(** Type of a parsing continuation. *)
type ('a, 'b) cont = Charset.t * ('b env -> 'b err -> 'a -> 'b)

(** Type of a  parser combinator with a semantic action of  type ['a]. They type
    ['r] represents the  type of the continuation, it  is universally quantified
    in the the definition of the type ['a t] below. *)
type ('a, 'r) comb = 'r env -> ('a, 'r) cont -> 'r err -> 'r

(** Type of  a parser combinaton, with universally  quantified continuation type
    parameter. *)
type 'a t = { comb : 'r. ('a, 'r) comb } [@@unboxed]

(** [next env  err] updates the current maximum position  [env.max_pos] and then
    calls the [err] function. *)
let next : 'b env -> 'a err -> 'a = fun env err ->
  let (buf_max, col_max) = !(env.max_pos) in
  let line_max = Input.line buf_max in
  let line = Input.line env.current_buf in
  if line > line_max || (line = line_max && env.current_col > col_max) then
    env.max_pos := (env.current_buf, env.current_col);
  err ()

(** [test cs env] returns [true] if and only if the next character to parse in
    the environment [env] is in the character set [cs]. *)
let test cs e = Charset.mem cs (Input.get e.current_buf e.current_col)

(** Combinator that always fails. *)
let fail : 'a t =
  { comb = fun env _ err -> next env err }

(** Combinator used as default fied before compilation *)
let assert_false : 'a t =
  { comb = fun _ _ _ -> assert false }

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x ->
  { comb = fun env k err -> snd k env err x }

(** Combinator accepting the given lexeme (or terminal). *)
let lexeme : 'a Lex.lexeme -> 'a t = fun lex ->
  let comb : type r. ('a, r) comb = fun env k err ->
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
         snd k env err v
     with Lex.NoParse -> fun () -> next env err) ()
  in
  { comb }

(** Sequence combinator. *)
let seq : 'a t -> ?cs:Charset.t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun g1 ?(cs=Charset.full) g2 fn ->
    let comb : type r. ('c, r) comb = fun env k err ->
      g1.comb env (cs,
        fun env err v1 ->
          g2.comb env (fst k,
            fun env err v2 ->
              (try fun () -> snd k env err (fn v1 v2)
               with Lex.NoParse -> fun () -> next env err) ()) err) err
  in
  { comb }

(** Dependant sequence combinator. *)
let dep_seq : ('a * 'b) t -> ?cs:Charset.t -> ('a -> 'c t)
              -> ('a -> 'b -> 'c -> 'd) -> 'd t =
  fun g1 ?(cs=Charset.full) g2 fn ->
    let comb : type r. ('d, r) comb = fun env k err ->
      g1.comb env (cs, fun env err (v1,v2) ->
        (try
           let g = g2 v1 in
           fun () -> g.comb env (fst k,
             fun env err v3 ->
             (try
                let v = fn v1 v2 v3 in
                fun () -> snd k env err v
              with Lex.NoParse -> fun () -> next env err) ()) err
         with Lex.NoParse -> fun () -> next env err) ()) err
  in
  { comb }

(** option combinator,  contrary to [alt] apply to [empty],  it uses the charset
    of the  continuation for prediction. Therefore  it is preferable not  to use
    empty in [alt] and use [option] instead.*)
let option: 'a -> Charset.t -> 'a t -> 'a t = fun x cs1 g1 ->
  let f1 = ref 0 and f2 = ref 0 in
  let comb : type r. ('a, r) comb = fun env k err ->
    match (test cs1 env, test (fst k) env) with
    | (false, false) -> next env err
    | (true , false) -> g1.comb env k err
    | (false, true ) -> snd k env err x
    | (true , true ) ->
       (* one wants to  avoid incrementing f2 in err or f1 in  err. This way, if
          f1 parses 50% and f2 the other 50%, we have f1 ~ f2 *)
       if !f1 < !f2 then (g1.comb env k (fun () -> incr f1; snd k env err x))
       else (snd k env (fun () -> incr f2; g1.comb env k err) x)
  in
  { comb }


(** Alternatives combinator. *)
let alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t =
  fun cs1 g1 cs2 g2 ->
  let f1 = ref 0 and f2 = ref 0 in
  let comb : type r. ('a, r) comb = fun env k err ->
    match (test cs1 env, test cs2 env) with
    | (false, false) -> next env err
    | (true , false) -> g1.comb env k err
    | (false, true ) -> g2.comb env k err
    | (true , true ) ->
       (* one wants to avoid incrementing f2 in err or f1 in err. This way, if
          f1 parses 50% and f2 the other 50%, we have f1 ~ f2 *)
       if !f1 < !f2 then (g1.comb env k (fun () -> incr f1; g2.comb env k err))
       else (g2.comb env k (fun () -> incr f2; g1.comb env k err))
  in
  { comb }

(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn ->
  let comb : type r. ('b, r) comb = fun env k err ->
    g.comb env (fst k, fun env err v ->
      (try let v = fn v in fun () -> snd k env err v
       with Lex.NoParse -> fun () -> next env err) ()) err
  in
  { comb }

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
let push : 'a t -> 'a t = fun g ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let pos = Pos.get_pos env.current_buf env.current_col in
    let env = { env with left_pos_stack = pos :: env.left_pos_stack } in
    let k0 env err v = snd k { env with left_pos_stack = tail_pos env } err v in
    g.comb env (fst k, k0) err
  in
  { comb }

(** Read the n-th position from the (left position) stack. *)
let read : int -> (Pos.t -> 'a) t -> 'a t = fun n g ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let pos = head_pos n env in
    let k0 env err fn =
      (try let v = fn pos in (fun () -> snd k env err v)
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g.comb env (fst k, k0) err
  in
  { comb }

(** Read the position at the left of the input to be parsed. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g -> push (read 0 g)

(** Read the position after parsing. *)
let right_pos : (Pos.t -> 'a) t -> 'a t = fun g ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let k0 env err fn =
      (try
         let pos = Pos.get_pos env.buf_before_blanks env.col_before_blanks in
         let v = fn pos in fun () -> snd k env err v
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g.comb env (fst k, k0) err
  in
  { comb }

(** [lr g  gf] is the combinator used to  eliminate left recursion. Intuitively,
    it parses using  the "grammar" [g gf*].  An equivalent  combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)]. *)
let lr : 'a t -> Charset.t -> 'a Assoc.key -> 'a t -> 'a t = fun g cs key gf ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let csk = Charset.union cs (fst k) in
    let rec klr env err v =
      match test cs env, test (fst k) env with
      | (true, true) ->
        let err () =
          let lr = Assoc.add key v env.lr in
          let env0 = { env with lr } in
          gf.comb env0 (csk, klr) err
        in
        snd k env err v
      | false, true ->
         snd k env err v
      | true, false ->
         let lr = Assoc.add key v env.lr in
         let env0 = { env with lr } in
         gf.comb env0 (csk,klr) err
      | false, false ->
         next env err
    in
    g.comb env (csk,klr) err
  in
  { comb }

(** combinator to access the value stored by lr*)
let read_tbl : 'a Assoc.key -> 'a t = fun key ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let v = try Assoc.find key env.lr with Not_found -> assert false in
    snd k env err v
  in
  { comb }


(** Combinator under a refrerence used to implement recursive grammars. *)
let deref : 'a t ref -> 'a t = fun gref ->
  { comb = fun env k err -> !gref.comb env k err }

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
    fun ?(config=default_layout_config) blank_fun g ->
  let comb : type r. ('a, r) comb = fun env k err ->
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
    g.comb env (fst k, fun env err v ->
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
      snd k env err v) err
  in
  { comb }

type 'a result =
  | Error : 'a result
  | Value : 'c env * (unit -> 'c) * 'a -> 'a result

(** Combinator for caching a grammar, to avoid exponential behavior. *)
let cache : type a. a t -> a t = fun g ->
  let cache = Input.Tbl.create () in
  let comb : type b. (a, b) comb = fun env0 k err ->
    let {current_buf = buf0; current_col = col0; _} = env0 in
    let add_elt elt =
      let l = try Input.Tbl.find cache buf0 col0 with Not_found -> [] in
      Input.Tbl.add cache buf0 col0 (elt :: l)
    in
    let k0 env err v = add_elt (Value(env, err, v)); snd k env err v in
    let err0 () = add_elt Error; err () in
    let rec fn l =
      match l with
      | []                    -> g.comb env0 (fst k, k0) err0
      | Error            :: l -> assert (l = []); err ()
      | Value(env,err,v) :: l ->
          match env0.key.eq env.key.k with
          | Assoc.NEq -> assert false
          | Assoc.Eq  -> snd k env (if l = [] then err else (fun () -> fn l)) v
    in
    let l = try Input.Tbl.find cache buf0 col0 with Not_found -> [] in
    fn (List.rev l)
  in
  { comb }

(* NOTE: cs with blank_after = false makes no sense ? *)
let partial_parse_buffer
    : type a. a t -> Lex.blank -> ?blank_after:bool
                  -> ?cs:Charset.t -> Lex.buf -> int -> a * Lex.buf * int =
  fun g blank_fun ?(blank_after=false) ?(cs=Charset.full) buf0 col0 ->
    let max_pos = ref (buf0, col0) in
    let err () =
      let (buf, col) = !max_pos in
      raise (Pos.Parse_error(buf, col))
    in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; col_before_blanks = col0
        ; current_buf = buf ; current_col = col; lr = Assoc.empty
        ; max_pos ; left_pos_stack = [] ; blank_fun ; key = Assoc.new_key () }
    in
    let k env _ v =
      if blank_after then (v, env.current_buf, env.current_col)
      else (v, env.buf_before_blanks, env.col_before_blanks)
    in
    g.comb env (cs, k) err

let parse_all_buffer : type a. a t -> Lex.blank -> Lex.buf -> int -> a list =
  fun g blank_fun buf0 col0 ->
    let max_pos = ref (buf0, col0) in
    let err () =
      let (buf, col) = !max_pos in
      raise (Pos.Parse_error(buf, col))
    in
    let res = ref [] in
    let k _ err v = res := v :: !res; err () in
    let (buf, col) = blank_fun buf0 col0 in
    let env =
      { buf_before_blanks = buf0 ; col_before_blanks = col0
        ; current_buf = buf ; current_col = col; lr = Assoc.empty
        ; max_pos; blank_fun ; left_pos_stack = [] ; key = Assoc.new_key () }
    in
    let cs = Charset.singleton '\255' in
    try
      ignore (g.comb env (cs,k) err);
      assert false
    with Pos.Parse_error _ as e -> if !res = [] then raise e; !res
