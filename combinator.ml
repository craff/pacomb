open Lex

(* type of parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type 'a cc = { cc : 'b. ('a -> buf -> int -> 'b) -> 'b } [@@unboxed]
type 'a comb = blank -> buf -> int -> 'a cc

(* the usual combinator *)
let cfail : ('a) comb =
  fun _b _s _n -> { cc = fun _k -> raise NoParse }
let cempty : 'a -> 'a comb =
  fun x _b s n -> { cc = fun k -> k x s n }
let cterm : 'a terminal -> 'a comb =
  fun t b s n ->
  { cc = fun k ->
         let (x,s,n) = t s n in
         let (s,n) = b s n in
         k x s n
    }
let cseq : 'a comb -> 'b comb -> ('a -> 'b -> 'c) -> 'c comb =
  fun g1 g2 f b s n ->
    { cc = fun k -> (g1 b s n).cc
             (fun x s n -> (g2 b s n).cc
                (fun y s n -> k (f x y) s n)) }

let calt : 'a comb -> 'a comb -> 'a comb =
  fun g1 g2 b s n -> { cc = fun k -> try (g1 b s n).cc k with NoParse -> (g2 b s n).cc k }

let capp : 'a comb -> ('a -> 'b) -> 'b comb =
  fun g1 f b s n -> { cc = fun k -> (g1 b s n).cc (fun x s n -> k (f x) s n) }

let clr : 'a comb -> ('a -> 'a) comb -> 'a comb =
  fun g0 gf b s n -> { cc = fun k ->
    let rec clr x s n =
      try k x s n
      with NoParse -> (gf b s n).cc (fun f s n -> clr (f x) s n)
    in
    (g0 b s n).cc clr}

exception ParseError of pos

let parse_buffer : type a. a comb -> blank -> buf -> a = fun g b s ->
  let g = cseq g (cterm (eof ())) (fun x _ -> x) in
  try (g b s 0).cc (fun x _s _n -> x)
  with NoParse ->
    let (l,c,c8) = Input.last_pos s in
    let pos = { name = Input.filename s; line = l; col = c
                ; utf8_col = c8; phantom = false }
    in
    raise (ParseError pos)

let parse_string : type a. a comb -> blank -> string -> a = fun g b s ->
  let s = Input.from_string s in
  parse_buffer g b s

let parse_channel : type a. a comb -> blank -> in_channel -> a = fun g b s ->
  let s = Input.from_channel s in
  parse_buffer g b s
