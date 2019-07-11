open Lex

(* type of parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type env = { b : blank; s0 : buf; n0: int; s : buf; n : int }
type 'a comb = { c : 'b. env -> (env -> 'a -> 'b) -> 'b } [@@unboxed]

let give_up () = raise NoParse

let test cs e =
  let (c,_,_) = Input.read e.s e.n in Charset.mem cs c

(* the usual combinator *)
let cfail : ('a) comb =
  { c = fun _e _k -> raise NoParse }

let cempty : 'a -> 'a comb =
  fun x -> { c = fun e k -> k e x }

let cterm : 'a fterm -> 'a comb =
  fun t ->
    { c = fun e k ->
          (*Printf.printf "\r %d    %!" e.n;*)
          let (x,s0,n0) = t e.s e.n in
          let (s,n) = e.b s0 n0 in
          k { e with s0; n0; s; n } x }

let clpos : (pos -> 'a) comb -> 'a comb =
  fun g ->
    { c = fun e k ->
          let pos = get_pos e.s e.n in
          g.c e (fun e f -> k e (f pos)) }


let crpos : (pos -> 'a) comb -> 'a comb =
  fun g ->
    { c = fun e k ->
          g.c e (fun e f -> let pos = get_pos e.s0 e.n0 in
                            k e (f pos)) }

let cseq : 'a comb -> 'b comb -> ('a -> 'b -> 'c) -> 'c comb =
  fun g1 g2 f ->
    { c = fun e k -> g1.c e
             (fun e x -> g2.c e
                (fun e y -> k e (f x y))) }

let c = ref 0

let calt : Charset.t -> 'a comb -> Charset.t -> 'a comb -> 'a comb =
  fun ch1 g1 ch2 g2 ->
    { c = fun e k ->
          match test ch1 e, test ch2 e with
          | false, false -> raise NoParse
          | true, false  -> g1.c e k
          | false, true  -> g2.c e k
          | true, true   ->
             try g1.c e k with NoParse -> g2.c e k
    }

let capp : 'a comb -> ('a -> 'b) -> 'b comb =
  fun g1 f -> { c = fun e k -> g1.c e (fun e x -> k e (f x)) }

let clr : 'a comb -> Charset.t -> ('a -> 'a) comb -> 'a comb =
  fun g0 cs gf -> { c = fun e k ->
    let rec clr e x =
      if test cs e then
        try k e x
        with NoParse ->  gf.c e (fun e f -> clr e (f x))
      else k e x
    in
    g0.c e clr}

exception ParseError of pos

let parse_buffer : type a. a comb -> blank -> buf -> a = fun g b s0 ->
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

let parse_string : type a. a comb -> blank -> string -> a = fun g b s ->
  let s = Input.from_string s in
  parse_buffer g b s

let parse_channel : type a. a comb -> blank -> in_channel -> a = fun g b s ->
  let s = Input.from_channel s in
  parse_buffer g b s
