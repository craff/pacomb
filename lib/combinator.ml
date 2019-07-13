open Lex

(** Combinator library *)

(* type xof parsing combinator with continuation.
   continuation are necessary for correct semantics of
   alternative *)
type env = { b : blank; lpos : pos list; maxp : (buf * int) ref
           ; s0 : buf; n0: int; s : buf; n : int }
type 'a t = { c : 'b. env -> (env -> 'a -> 'b) -> 'b } [@@unboxed]

exception Next

let next : env -> 'a= fun e ->
  let (b0,n0) = !(e.maxp) in
  let l = Input.line e.s in
  let l0 = Input.line b0 in
  if l > l0 || (l = l0 && e.n > n0) then e.maxp := (e.s,e.n);
  raise Next

let give_up () = raise NoParse

let test cs e =
  let (c,_,_) = Input.read e.s e.n in Charset.mem cs c

(* the usual combinators *)
let cfail : ('a) t =
  { c = fun _e _k -> raise Next }

let cempty : 'a -> 'a t =
  fun x -> { c = fun e k -> k e x }

let cterm : 'a fterm -> 'a t =
  fun t ->
    { c = fun e k ->
          let (x,s0,n0) = try t e.s e.n with NoParse -> next e in
          let (s,n) = e.b s0 n0 in
          k { e with s0; n0; s; n } x }

let cseq : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun g1 g2 f ->
    { c = fun e k -> g1.c e
             (fun e x -> g2.c e
                (fun e y -> k e (try f x y with NoParse -> next e))) }

let cdep_seq: 'a t -> ('a -> 'b t) -> ('b -> 'c) -> 'c t =
    fun g1 g2 f ->
    { c = fun e k -> g1.c e
             (fun e x -> (try g2 x with NoParse -> next e).c e
                (fun e y -> k e (try f y with NoParse -> next e))) }

let calt : ?cs1:Charset.t -> ?cs2:Charset.t -> 'a t -> 'a t -> 'a t =
  fun ?(cs1=Charset.full) ?(cs2=Charset.full) g1 g2 ->
    { c = fun e k ->
          match test cs1 e, test cs2 e with
          | false, false -> next e
          | true, false  -> g1.c e k
          | false, true  -> g2.c e k
          | true, true   ->
             try g1.c e k with Next -> g2.c e k
    }

let capp : 'a t -> ('a -> 'b) -> 'b t =
  fun g1 f -> { c = fun e k -> g1.c e (fun e x -> k e (try f x with NoParse -> next e)) }

let head_pos n e =
  let rec fn n = function
  | [] -> assert false
  | x::l -> if n <= 0 then x else fn (n-1) l
  in fn n e.lpos

let tail_pos e = match e.lpos with
  | [] -> assert false
  | _::l -> l

let cpush : 'a t -> 'a t =
  fun g ->
  { c = fun e k ->
        let pos = get_pos e.s e.n in
        let e = { e with lpos = pos::e.lpos } in
        g.c e ( fun e x -> k { e with lpos = tail_pos e} x) }

let cread : int -> (pos -> 'a) t -> 'a t =
  fun n g ->
  { c = fun e k ->
        let pos = head_pos n e in
        g.c e ( fun e f -> k e (f pos) ) }

let clpos : (pos -> 'a) t -> 'a t = fun g -> cpush (cread 0 g)

let crpos : (pos -> 'a) t -> 'a t =
  fun g ->
    { c = fun e k ->
          g.c e (fun e f -> let pos = get_pos e.s0 e.n0 in
                            k e (try f pos with NoParse -> next e)) }

let clr : ?cs2:Charset.t -> 'a t -> ('a -> 'a) t -> 'a t =
  fun ?(cs2=Charset.full) g0 gf -> { c = fun e k ->
    let rec clr e x =
      if test cs2 e then
        try k e x
        with Next ->  gf.c e (fun e f -> clr e (try f x with NoParse -> next e))
      else k e x
    in
    g0.c e clr}

let cref : 'a t ref -> 'a t = fun ptr -> { c = fun e k -> !ptr.c e k }

let clayout
    : ?old_before:bool -> ?new_before:bool -> ?new_after:bool -> ?old_after:bool
      -> 'a t -> blank -> 'a t =
  fun ?(old_before=true) ?(new_before=false) ?(new_after=false) ?(old_after=true)
      g b ->
  { c = fun e k ->
        let (s,n) = if old_before then (e.s,e.n) else (e.s0,e.n0) in
        let (s,n) = if new_before then b s n else (s,n) in
        let b0 = e.b in
        let e = { e with b; s; n } in
        g.c e (fun e x ->
              let (s,n) = if new_after then (e.s,e.n) else (e.s0,e.n0) in
              let (s,n) = if old_after then b0 s n else (s,n) in
              let e = { e with b=b0; s; n } in
              k e x) }


exception ParseError of pos

let parse_buffer : type a. a t -> blank -> buf -> a = fun g b s0 ->
  let g = cseq g (cterm (eof ()).f) (fun x _ -> x) in
  let maxp = ref (s0,0) in
  try
    let (s,n) = b s0 0 in
    g.c { b; s0; n0=0; s; n; maxp; lpos=[]} (fun _e x -> x)
  with Next ->
    let (s,c) = !maxp in
    let l = Input.line_num s in
    let c8 = Input.utf8_col_num s c in
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
