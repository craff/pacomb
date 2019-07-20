open Lex
open Position

(** Combinator library *)

(** Combinators are a standard way of parsing using a functional language.
    They present two major defect:

    - incomplete semantics: a grammar (a|b)c may fail to backtrack and try bc
      if parsing for ac fails in c. This is traditionally solved using
      continuation: i.e. a parsing combinato waits for a function to
      parse the rest of the input.

    - exponential semantics. The parsing problem for context free grammar can
      be solved in polynomial time (O(N^3) implementation are often proposed.
      combinator backtracks and therefore are in exponential time. This is
      solved here by a [cache] combinator to avoid parsing twice the same part of
      the input with the same grammar.

 *)

(** type of parsing combinator with continuation. *)

(** the parsing environment holding the information needed to parse *)
type 'a env =
  { b : blank              (** the blank function to parse spaces etc ... *)
  ; lpos : pos list        (** stack of left position needed for correct
                               position with the elimination of left recursion *)
  ; maxp : (buf * int) ref (** a reference to store the maximum position reached
                               by the parser to report an error *)
  ; s  : buf; n : int      (** current position (buffer + column) *)
  ; s0 : buf; n0: int      (** position (buffer + column) before the blank *)
  ; k : 'a Assoc.key       (** a type representation to keep in the environment
                               the type of the global result after parsing *)
  }

(** type of a combinator. it is polymorphic in the type of the global result *)
type 'b error = unit -> 'b
type ('a,'b) continuation = 'b env -> 'b error -> 'a -> 'b
type 'a combinator =
  { c : 'b. 'b env -> ('a,'b) continuation -> 'b error -> 'b } [@@unboxed]
type 'a t = 'a combinator

(** function that record the position beffor calling the error function *)
let next : 'b env -> 'a error -> 'a = fun e f ->
  let (b0,n0) = !(e.maxp) in
  let l = Input.line e.s in
  let l0 = Input.line b0 in
  if l > l0 || (l = l0 && e.n > n0) then e.maxp := (e.s,e.n);
  f ()

(** a function to reject parsing from the "action" code producing the semantics *)
let give_up () = raise NoParse

let test cs e =
  let (c,_,_) = Input.read e.s e.n in Charset.mem cs c

(** The combinators *)

(** combinator that always fails *)
let cfail : ('a) t = { c = fun _e _k f -> f () }

(** combinator accepting the empty input *)
let cempty : 'a -> 'a t =
  fun x -> { c = fun e k f -> k e f x }

(** combinator accepting a given terminal *)
let cterm : 'a fterm -> 'a t =
  fun t ->
    { c = fun e k f ->
          (try
            let (x,s0,n0) = t e.s e.n in
            let (s,n) = e.b s0 n0 in
            fun () -> k { e with s0; n0; s; n } f x
          with NoParse -> fun () -> next e f) () }

(** sequence *)
let cseq : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun g1 g2 fn ->
    { c = fun e k f -> g1.c e
             (fun e f x -> g2.c e
                (fun e f y ->
                  (try
                     let z = fn x y in
                     fun () -> k e f z
                   with NoParse ->
                     fun () -> next e f) ()) f) f }

(** dependant sequence *)
let cdep_seq: 'a t -> ('a -> 'b t) -> ('b -> 'c) -> 'c t =
    fun g1 g2 fn ->
    { c = fun e k f -> g1.c e
            (fun e f x ->
              (try let g = g2 x in
                   fun () -> g.c e
                               (fun e f y ->
                                 (try
                                    let z = fn y in
                                    fun () -> k e f z
                                  with NoParse ->
                                    fun () -> next e f) ()) f
               with NoParse ->
                 fun () -> next e f) ()) f }

(** alternatives *)
let calt : ?cs1:Charset.t -> ?cs2:Charset.t -> 'a t -> 'a t -> 'a t =
  fun ?(cs1=Charset.full) ?(cs2=Charset.full) g1 g2 ->
    { c = fun e k f ->
          match test cs1 e, test cs2 e with
          | false, false -> next e f
          | true, false  -> g1.c e k f
          | false, true  -> g2.c e k f
          | true, true   ->
             g1.c e k (fun () -> g2.c e k f)
    }

(** application *)
let capp : 'a t -> ('a -> 'b) -> 'b t =
  fun g1 fn -> { c = fun e k f ->
                    g1.c e (fun e f x ->
                        (try
                          let y = fn x in
                          fun () -> k e f y
                         with NoParse ->
                           fun () -> next e f) ()) f }

let head_pos n e =
  let rec fn n = function
  | [] -> assert false
  | x::l -> if n <= 0 then x else fn (n-1) l
  in fn n e.lpos

let tail_pos e = match e.lpos with
  | [] -> assert false
  | _::l -> l

(** push the current position to a stack *)
let cpush : 'a t -> 'a t =
  fun g ->
  { c = fun e k f ->
        let pos = get_pos e.s e.n in
        let e = { e with lpos = pos::e.lpos } in
        g.c e ( fun e f x -> k { e with lpos = tail_pos e} f x) f }

(** read the position from the stack *)
let cread : int -> (pos -> 'a) t -> 'a t =
  fun n g ->
  { c = fun e k f ->
        let pos = head_pos n e in
        g.c e (fun e f fn ->
            (try
               let x = fn pos in
               fun () -> k e f x
             with
               NoParse -> fun () -> next e f) ()) f }

(** read the position at the left of the input to be parsed *)
let clpos : (pos -> 'a) t -> 'a t = fun g -> cpush (cread 0 g)

(** read the position after parsing *)
let crpos : (pos -> 'a) t -> 'a t =
  fun g ->
    { c = fun e k f ->
          g.c e (fun e f fn ->
              (try
                 let pos = get_pos e.s0 e.n0 in
                 let x = fn pos in
                 fun () -> k e f x
               with NoParse -> fun () -> next e f) ()) f }

(** [cls c1 c2] is an optimized version of [let rec r = seq c1 (seq r c2)] *)
let clr : ?cs2:Charset.t -> 'a t -> ('a -> 'a) t -> 'a t =
  fun ?(cs2=Charset.full) g0 gf -> { c = fun e k f ->
    let rec clr e f x =
      if test cs2 e then
        k e
          (fun () ->
            gf.c e (fun e f fn ->
                (try
                  let y = fn x in
                  fun () -> clr e f y
                with NoParse ->
                  fun () -> next e f) ()) f)
          x
      else k e f x
    in
    g0.c e clr f}

(** combinator under a ref to implement recursive grammars *)
let cref : 'a t ref -> 'a t = fun ptr -> { c = fun e k f -> !ptr.c e k f }

(** changes the blank function *)
let clayout
    : ?old_before:bool -> ?new_before:bool -> ?new_after:bool -> ?old_after:bool
      -> 'a t -> blank -> 'a t =
  fun ?(old_before=true) ?(new_before=false) ?(new_after=false) ?(old_after=true)
      g b ->
  { c = fun e k f ->
        let (s,n) = if old_before then (e.s,e.n) else (e.s0,e.n0) in
        let (s,n) = if new_before then b s n else (s,n) in
        let b0 = e.b in
        let e = { e with b; s; n } in
        g.c e (fun e f x ->
              let (s,n) = if new_after then (e.s,e.n) else (e.s0,e.n0) in
              let (s,n) = if old_after then b0 s n else (s,n) in
              let e = { e with b=b0; s; n } in
              k e f x) f }

type 'a result =
  | Error : 'a result
  | Value : 'c env * (unit -> 'c) * 'a -> 'a result

(** combinator that caches a grammar to avoid exponential behavior *)
let ccache : type a.a t -> a t = fun g ->
  let open Assoc in
  let open Input.Tbl in
  let cache = create () in
  let c : type b. b env -> (b env -> (unit -> b) -> a -> b) -> (unit -> b) -> b  =
    fun e0 k f ->
      let add_value e f x =
        let l = try find cache e0.s e0.n with Not_found -> [] in
        let l = (Value(e,f,x)) :: l in
        add cache e0.s e0.n l
      in
      let add_error () =
        let l = try find cache e0.s e0.n with Not_found -> [] in
        let l = Error :: l in
        add cache e0.s e0.n l
      in
      let k0 e f x =
        add_value e f x;
        k e f x
      in
      let f0 () =
        add_error ();
        f ()
      in
      let l = List.rev (try find cache e0.s e0.n with Not_found -> []) in
      let rec fn = function
        | [] -> g.c e0 k0 f0
        | [Value(e,f,x)] ->
           (match e0.k.eq e.k.k with
            | Eq -> k e f x
            | NEq -> assert false)
        | [Error]        -> f ()
        | Value(e,_f,x) :: l ->
           let f () = fn l in
           (match e0.k.eq e.k.k with
            | Eq -> k e f x
            | NEq -> assert false)
        | Error :: _     -> assert false
      in
      fn l
  in { c }

exception Parse_error of Input.buffer * int

(** A helper to hangle exceptions *)
let fail_no_parse () = exit 1

let handle_exception ?(error=fail_no_parse) f a =
  try f a with Parse_error(buf, pos) ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: file %S, line %d, character %d.\n")
      (Input.filename buf) (Input.line_num buf) (Input.utf8_col_num buf pos);
    error ()

let partial_parse_buffer : type a. a t -> blank -> ?blank_after:bool -> buf -> int -> a * buf * int =
  fun g b ?(blank_after=false) s0 n0 ->
    let maxp = ref (s0,n0) in
    let f () =
      let (s,c) = !maxp in
      raise (Parse_error (s,c))
    in
    let (s,n) = b s0 n0 in
    g.c { b; s0; n0; s; n; maxp; lpos=[]; k = Assoc.new_key ()} (fun e _f x ->
        if blank_after then (x,e.s,e.n) else (x,e.s0,e.n0)) f

let parse_buffer : type a. a t -> blank -> buf -> a = fun g b s0 ->
  let g = cseq g (cterm (eof ()).f) (fun x _ -> x) in
  let (x,_,_) = partial_parse_buffer g b s0 0 in
  x

let parse_string : type a. a t -> blank -> string -> a = fun g b s ->
  let s = Input.from_string s in
  parse_buffer g b s

let parse_channel : type a. a t -> blank -> in_channel -> a = fun g b s ->
  let s = Input.from_channel s in
  parse_buffer g b s
