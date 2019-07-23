(** Parser combinator library *)

(** Combinators are a standard approach to parsing in functional language. The
    major advantage of combinators is that they allow manipulating grammars as
    first class values. However, they generally suffer from two major defects.

    - Incomplete semantics. A grammar "(a|b)c" may fail to backtrack and try
      "bc" if parsing for "ac" fails in "c". This is traditionally solved with
      continuation: combinators must be given the function that will be used
      to parse the remaining input.

    - Exponential semantics. The parsing problem for context-free grammars can
      be solved in polynomial time (O(n³) implementation are often proposed).
      As combinator backtrack, they usually lead to an exponential behaviour.
      This is solved here by a [cache] combinator, that avoids parsing twice
      the same part of the input with the same grammar. *)

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
  (** Type repr. for keeping the type of the global result after parsing. *) }

(** Type of a function called in case of error. *)
type 'b err = unit -> 'b

(** Type of a parsing continuation. *)
type ('a, 'b) cont = 'b env -> 'b err -> 'a -> 'b

(** Type of a parser combinator with a semantic action of type ['a]. They type
    ['r] represents the type of the continuation, it is universally quantified
    in the the definition of the type ['a t] below. *)
type ('a, 'r) comb = 'r env -> ('a, 'r) cont -> 'r err -> 'r

(** Type of a parser combinaton, with universally quantified continuation type
    parameter. *)
type 'a t = { comb : 'r. ('a, 'r) comb } [@@unboxed]

(** [next env err] updates the current maximum position [env.max_pos] and then
    calls the [err] function. *)
let next : 'b env -> 'a err -> 'a = fun env err ->
  let (buf_max, col_max) = !(env.max_pos) in
  let line_max = Input.line buf_max in
  let line = Input.line env.current_buf in
  if line > line_max || (line = line_max && env.current_col > col_max) then
    env.max_pos := (env.current_buf, env.current_col);
  err ()

(** [give_up ()] rejects parsing from a corresponding semantic action. *)
let give_up : unit -> 'a =
  fun _ -> raise Lex.NoParse

(** [test cs env] returns [true] if and only if the next character to parse in
    the environment [env] is in the character set [cs]. *)
let test cs e = Charset.mem cs (Input.get e.current_buf e.current_col)

(** Combinator that always fails. *)
let fail : 'a t =
  { comb = fun _ _ err -> err () }

(** Combinator accepting the empty input only. *)
let empty : 'a -> 'a t = fun x ->
  { comb = fun env k err -> k env err x }

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
                    ; current_buf ; current_col }
         in
         k env err v
     with Lex.NoParse -> fun () -> next env err) ()
  in
  { comb }

(** Sequence combinator. *)
let seq : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t = fun g1 g2 fn ->
  let comb : type r. ('c, r) comb = fun env k err ->
    g1.comb env (fun env err v1 ->
      g2.comb env (fun env err v2 ->
        (try fun () -> k env err (fn v1 v2)
         with Lex.NoParse -> fun () -> next env err) ()) err) err
  in
  { comb }

(** Dependant sequence combinator. *)
let dep_seq : 'a t -> ('a -> 'b t) -> ('b -> 'c) -> 'c t = fun g1 g2 fn ->
  let comb : type r. ('c, r) comb = fun env k err ->
    g1.comb env (fun env err v1 ->
      (try
         let g = g2 v1 in
         fun () -> g.comb env (fun env err v2 ->
           (try
              let v = fn v2 in
              fun () -> k env err v
            with Lex.NoParse -> fun () -> next env err) ()) err
       with Lex.NoParse -> fun () -> next env err) ()) err
  in
  { comb }

(** Alternatives combinator. *)
let alt : Charset.t -> 'a t -> Charset.t -> 'a t -> 'a t =
    fun cs1 g1 cs2 g2 ->
  let comb : type r. ('a, r) comb = fun env k err ->
    match (test cs1 env, test cs2 env) with
    | (false, false) -> next env err
    | (true , false) -> g1.comb env k err
    | (false, true ) -> g2.comb env k err
    | (true , true ) -> g1.comb env k (fun () -> g2.comb env k err)
  in
  { comb }

(** Application of a semantic function to alter a combinator. *)
let app : 'a t -> ('a -> 'b) -> 'b t = fun g fn ->
  let comb : type r. ('b, r) comb = fun env k err ->
    g.comb env (fun env err v ->
      (try let v = fn v in fun () -> k env err v
       with Lex.NoParse -> fun () -> next env err) ()) err
  in
  { comb }

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
    let k env err v = k { env with left_pos_stack = tail_pos env } err v in
    g.comb env k err
  in
  { comb }

(** Read the n-th position from the (left position) stack. *)
let read : int -> (Pos.t -> 'a) t -> 'a t = fun n g ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let pos = head_pos n env in
    let k env err fn =
      (try let v = fn pos in fun () -> k env err v
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g.comb env k err
  in
  { comb }

(** Read the position at the left of the input to be parsed. *)
let left_pos : (Pos.t -> 'a) t -> 'a t = fun g -> push (read 0 g)

(** Read the position after parsing. *)
let right_pos : (Pos.t -> 'a) t -> 'a t = fun g ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let k env err fn =
      (try
         let pos = Pos.get_pos env.buf_before_blanks env.col_before_blanks in
         let v = fn pos in fun () -> k env err v
       with Lex.NoParse -> fun () -> next env err) ()
    in
    g.comb env k err
  in
  { comb }

(** [lr g gf] is the combinator used to eliminate left recursion. Intuitively,
    it parses using the "grammar" [g gf*].  An equivalent combinator CANNOT be
    defined as [seq Charset.full g cs (let rec r = seq cs r cs gf in r)]. *)
let lr : 'a t -> Charset.t -> ('a -> 'a) t -> 'a t = fun g cs gf ->
  let comb : type r. ('a, r) comb = fun env k err ->
    let rec lr env err v =
      if test cs env then
        let err () =
          gf.comb env (fun env err fn ->
            (try let y = fn v in fun () -> lr env err y
             with Lex.NoParse -> fun () -> next env err) ()) err
        in
        k env err v
      else k env err v
    in
    g.comb env lr err
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
    g.comb env (fun env err v ->
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
  in
  { comb }

type 'a result =
  | Error : 'a result
  | Value : 'c env * (unit -> 'c) * 'a -> 'a result

(** combinator that caches a grammar to avoid exponential behavior *)
let cache : type a.a t -> a t = fun g ->
  let open Assoc in
  let open Input.Tbl in
  let cache = create () in
  let comb : type b. (a, b) comb =
    fun e0 k f ->
      let add_value e f x =
        let l = try find cache e0.current_buf e0.current_col with Not_found -> [] in
        let l = (Value(e,f,x)) :: l in
        add cache e0.current_buf e0.current_col l
      in
      let add_error () =
        let l = try find cache e0.current_buf e0.current_col with Not_found -> [] in
        let l = Error :: l in
        add cache e0.current_buf e0.current_col l
      in
      let k0 e f x =
        add_value e f x;
        k e f x
      in
      let f0 () =
        add_error ();
        f ()
      in
      let l = List.rev (try find cache e0.current_buf e0.current_col with Not_found -> []) in
      let rec fn = function
        | [] -> g.comb e0 k0 f0
        | [Value(e,f,x)] ->
           (match e0.key.eq e.key.k with
            | Eq -> k e f x
            | NEq -> assert false)
        | [Error]        -> f ()
        | Value(e,_f,x) :: l ->
           let f () = fn l in
           (match e0.key.eq e.key.k with
            | Eq -> k e f x
            | NEq -> assert false)
        | Error :: _     -> assert false
      in
      fn l
  in { comb }

exception Parse_error of Input.buffer * int

(** A helper to hangle exceptions *)
let fail_no_parse () = exit 1

let handle_exception ?(error=fail_no_parse) f a =
  try f a with Parse_error(buf, pos) ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: file %S, line %d, character %d.\n")
      (Input.filename buf) (Input.line_num buf) (Input.utf8_col_num buf pos);
    error ()

let partial_parse_buffer : type a. a t -> Lex.blank -> ?blank_after:bool -> Lex.buf -> int -> a * Lex.buf * int =
  fun g b ?(blank_after=false) s0 n0 ->
    let max_pos = ref (s0,n0) in
    let f () =
      let (s,c) = !max_pos in
      raise (Parse_error (s,c))
    in
    let (s,n) = b s0 n0 in
    g.comb { blank_fun = b; buf_before_blanks = s0; col_before_blanks = n0; current_buf = s; current_col = n; max_pos; left_pos_stack=[]; key = Assoc.new_key ()} (fun e _f x ->
        if blank_after then (x,e.current_buf,e.current_col)
        else (x,e.buf_before_blanks,e.col_before_blanks)) f

let parse_buffer : type a. a t -> Lex.blank -> Lex.buf -> a = fun g b s0 ->
  let g = seq g (lexeme (Lex.eof ()).f) (fun x _ -> x) in
  let (x,_,_) = partial_parse_buffer g b s0 0 in
  x

let parse_string : type a. a t -> Lex.blank -> string -> a =
  fun g b s -> parse_buffer g b (Input.from_string s)

let parse_channel : type a. a t -> Lex.blank -> in_channel -> a =
  fun g b ic -> parse_buffer g b (Input.from_channel ic)

let parse_all_buffer : type a. a t -> Lex.blank -> Lex.buf -> a list =
  fun g b s0->
    let g = seq g (lexeme (Lex.eof ()).f) (fun x _ -> x) in
    let n0 = 0 in
    let max_pos = ref (s0,n0) in
    let f () =
      let (s,c) = !max_pos in
      raise (Parse_error (s,c))
    in
    let res = ref [] in
    let k _e f x =
      res := x :: !res;
      f ()
    in
    let (s,n) = b s0 n0 in
    try
      ignore (g.comb { blank_fun = b; buf_before_blanks = s0; col_before_blanks = n0; current_buf = s; current_col = n; max_pos; left_pos_stack=[];
                    key = Assoc.new_key ()} k f);
      assert false
    with Parse_error _ as e ->
      if !res = [] then raise e else !res
