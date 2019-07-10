open Lex

(* type of parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type 'a env = buf -> int -> 'a
type 'a comb = { c : 'b. blank -> (('a -> 'b) env -> 'b) env } [@@unboxed]

let give_up () = raise NoParse

(* the usual combinator *)
let cfail : ('a) comb =
  { c = fun _b _s _n _k -> raise NoParse }
let cempty : 'a -> 'a comb =
  fun x -> { c = fun _b s n k -> k s n x }
let cterm : 'a terminal -> 'a comb =
  fun t ->
  { c = fun b s n k ->
        let (x,s,n) = t s n in
        let (s,n) = b s n in
        k s n x
    }
let cseq : 'a comb -> 'b comb -> ('a -> 'b -> 'c) -> 'c comb =
  fun g1 g2 f ->
    { c = fun b s n k -> g1.c b s n
             (fun s n x -> g2.c b s n
                (fun s n y -> k s n (f x y))) }

let calt : 'a comb -> 'a comb -> 'a comb =
  fun g1 g2 -> { c = fun b s n k -> try g1.c b s n k with NoParse -> g2.c b s n k }

let capp : 'a comb -> ('a -> 'b) -> 'b comb =
  fun g1 f -> { c = fun b s n k -> g1.c b s n (fun s n x -> k s n (f x)) }

let clr : 'a comb -> ('a -> 'a) comb -> 'a comb =
  fun g0 gf -> { c = fun b s n k ->
    let rec clr s n x =
      try k s n x
      with NoParse -> gf.c b s n (fun s n f -> clr s n (f x))
    in
    g0.c b s n clr}

exception ParseError of pos

let parse_buffer : type a. a comb -> blank -> buf -> a = fun g b s ->
  let g = cseq g (cterm (eof ())) (fun x _ -> x) in
  try g.c b s 0 (fun _s _n x -> x)
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
