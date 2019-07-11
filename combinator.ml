open Lex

(* type of parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type 'a env = buf -> int -> buf -> int -> 'a
type 'a comb = { c : 'b. blank -> (('a -> 'b) env -> 'b) env } [@@unboxed]

let give_up () = raise NoParse

let test cs s n =
  let (c,_,_) = Input.read s n in Charset.mem cs c

(* the usual combinator *)
let cfail : ('a) comb =
  { c = fun _b _s0 _n0 _s _n _k -> raise NoParse }
let cempty : 'a -> 'a comb =
  fun x -> { c = fun _b s0 n0 s n k -> k s0 n0 s n x }
let cterm : 'a fterm -> 'a comb =
  fun t ->
  { c = fun b _s0 _n0 s n k ->
        let (x,s0,n0) = t s n in
        let (s,n) = b s0 n0 in
        k s0 n0 s n x
    }
let cseq : 'a comb -> 'b comb -> ('a -> 'b -> 'c) -> 'c comb =
  fun g1 g2 f ->
    { c = fun b s0 n0 s n k -> g1.c b s0 n0 s n
             (fun s0 n0 s n x -> g2.c b s0 n0 s n
                (fun s0 n0 s n y -> k s0 n0 s n (f x y))) }

let calt : Charset.t -> 'a comb -> Charset.t -> 'a comb -> 'a comb =
  fun ch1 g1 ch2 g2 ->
    { c = fun b s0 n0 s n k ->
          match test ch1 s n, test ch2 s n with
          | false, false -> raise NoParse
          | true, false  -> g1.c b s0 n0 s n k
          | false, true  -> g2.c b s0 n0 s n k
          | true, true   ->
             try g1.c b s0 n0 s n k with NoParse -> g2.c b s0 n0 s n k
    }

let capp : 'a comb -> ('a -> 'b) -> 'b comb =
  fun g1 f -> { c = fun b s0 n0 s n k -> g1.c b s0 n0 s n (fun s0 n0 s n x -> k s0 n0 s n (f x)) }

let clr : 'a comb -> Charset.t -> ('a -> 'a) comb -> 'a comb =
  fun g0 cs gf -> { c = fun b s0 n0 s n k ->
    let rec clr s0 n0 s n x =
      if test cs s n then
        try gf.c b s0 n0 s n (fun s0 n0 s n f -> clr s0 n0 s n (f x))
        with NoParse -> k s0 n0 s n x
      else k s0 n0 s n x
    in
    g0.c b s0 n0 s n clr}

exception ParseError of pos

let parse_buffer : type a. a comb -> blank -> buf -> a = fun g b s0 ->
  let g = cseq g (cterm (eof ()).f) (fun x _ -> x) in
  try
    let (s,n) = b s0 0 in
    g.c b s0 0 s n (fun _s0 _n0 _s _n x -> x)
  with NoParse ->
    let (l,c,c8) = Input.last_pos s0 in
    let pos = { name = Input.filename s0; line = l; col = c
                ; utf8_col = c8; phantom = false }
    in
    raise (ParseError pos)

let parse_string : type a. a comb -> blank -> string -> a = fun g b s ->
  let s = Input.from_string s in
  parse_buffer g b s

let parse_channel : type a. a comb -> blank -> in_channel -> a = fun g b s ->
  let s = Input.from_channel s in
  parse_buffer g b s
