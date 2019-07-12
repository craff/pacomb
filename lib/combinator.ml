open Lex

(** Combinator library *)

(* type xof parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type env = { b : blank; s0 : buf; n0: int; s : buf; n : int }
type 'a t = { c : 'b. env -> (env -> 'a -> 'b) -> 'b } [@@unboxed]

let give_up () = raise NoParse

let test cs e =
  let (c,_,_) = Input.read e.s e.n in Charset.mem cs c

(* the usual combinators *)
let cfail : ('a) t =
  { c = fun _e _k -> raise NoParse }

let cempty : 'a -> 'a t =
  fun x -> { c = fun e k -> k e x }

let cterm : 'a fterm -> 'a t =
  fun t ->
    { c = fun e k ->
          (*Printf.printf "\r %d    %!" e.n;*)
          let (x,s0,n0) = t e.s e.n in
          let (s,n) = e.b s0 n0 in
          k { e with s0; n0; s; n } x }

let cseq : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun g1 g2 f ->
    { c = fun e k -> g1.c e
             (fun e x -> g2.c e
                (fun e y -> k e (f x y))) }

let calt : ?cs1:Charset.t -> ?cs2:Charset.t -> 'a t -> 'a t -> 'a t =
  fun ?(cs1=Charset.full) ?(cs2=Charset.full) g1 g2 ->
    { c = fun e k ->
          match test cs1 e, test cs2 e with
          | false, false -> raise NoParse
          | true, false  -> g1.c e k
          | false, true  -> g2.c e k
          | true, true   ->
             try g1.c e k with NoParse -> g2.c e k
    }

let capp : 'a t -> ('a -> 'b) -> 'b t =
  fun g1 f -> { c = fun e k -> g1.c e (fun e x -> k e (f x)) }

let clpos : (pos -> 'a) t -> 'a t =
  fun g ->
    { c = fun e k ->
          let pos = get_pos e.s e.n in
          g.c e (fun e f -> k e (f pos)) }


let crpos : (pos -> 'a) t -> 'a t =
  fun g ->
    { c = fun e k ->
          g.c e (fun e f -> let pos = get_pos e.s0 e.n0 in
                            k e (f pos)) }

let clr : ?cs2:Charset.t -> 'a t -> ('a -> 'a) t -> 'a t =
  fun ?(cs2=Charset.full) g0 gf -> { c = fun e k ->
    let rec clr e x =
      if test cs2 e then
        try k e x
        with NoParse ->  gf.c e (fun e f -> clr e (f x))
      else k e x
    in
    g0.c e clr}

let cref : 'a t ref -> 'a t = fun ptr -> { c = fun e k -> !ptr.c e k }

exception ParseError of pos

let parse_buffer : type a. a t -> blank -> buf -> a = fun g b s0 ->
  let g = cseq g (cterm (eof ()).f) (fun x _ -> x) in
  try
    let (s,n) = b s0 0 in
    g.c { b; s0; n0=0; s; n} (fun _e x -> x)
  with NoParse ->
    let (l,c,c8) = Input.last_pos s0 in
    let pos = { name = Input.filename s0; line = l; col = c
                ; utf8_col = c8; phantom = false }
    in
    raise (ParseError pos)

let parse_string : type a. a t -> blank -> string -> a = fun g b s ->
  let s = Input.from_string s in
  parse_buffer g b s

let parse_channel : type a. a t -> blank -> in_channel -> a = fun g b s ->
  let s = Input.from_channel s in
  parse_buffer g b s
