type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a fterm = buf -> int -> 'a * buf * int
type 'a terminal = { n : string; f : 'a fterm; c : Charset.t }

exception NoParse

let compute_utf8_col = ref true

let sp = Printf.sprintf

let s0 = Input.from_string ""
let accept_empty : type a. a terminal -> bool = fun t ->
  try ignore(t.f s0 0); true with NoParse -> false

let eof : 'a -> 'a terminal = fun x ->
  { n = "EOF"
  ; c = Charset.singleton '\255'
  ; f = fun s n -> if Input.is_empty s n then (x, s, n) else raise NoParse }

let char : char -> 'a -> 'a terminal = fun c x ->
  { n = sp "%C" c
  ; c = Charset.singleton c
  ; f = fun s n -> let (c',s,n) = Input.read s n in
                   if c = c' then (x,s,n) else raise NoParse }

let charset : Charset.t -> char terminal = fun cs ->
  { n = sp "%s" (Charset.show cs)
  ; c = cs
  ; f = fun s n ->
        let (c,s,n) = Input.read s n in
        if Charset.mem cs c then (c, s, n)
        else raise NoParse }

let not_charset : Charset.t -> unit terminal = fun cs ->
  { n = sp "^%s" (Charset.show cs)
  ; c = Charset.complement cs
  ; f = fun s n ->
        let (c,s,n) = Input.read s n in
        if not (Charset.mem cs c) then ((), s, n)
        else raise NoParse }

let seq : 'a terminal -> 'b terminal -> ('a -> 'b -> 'c) -> 'c terminal =
  fun t1 t2 f ->
  { n = sp "%s%s" t1.n t2.n
  ; c = if accept_empty t1 then Charset.union t1.c t2.c else t1.c
  ; f = fun s n ->
        let (s1,s,n) = t1.f s n in
        let (s2,s,n) = t2.f s n in
        (f s1 s2,s,n) }

let option : 'a terminal -> 'a option terminal = fun t ->
  { n = sp "(%s)?" t.n
  ; c = Charset.full
  ; f = fun s n ->
        try let (x,s,n) = t.f s n in (Some x,s,n)
        with NoParse -> (None,s,n) }

let star : 'a terminal -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b terminal = fun t a f ->
  { n = sp "(%s)*" t.n
  ; c = t.c
  ; f = fun s n ->
        let rec fn a s n =
          try
            let (x,s,n) = t.f s n in
            fn (f a x) s n
          with NoParse -> (a,s,n)
        in
        fn (a ()) s n }

let plus : 'a terminal -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b terminal = fun t a f ->
  { n = sp "(%s)*" t.n
  ; c = t.c
  ; f = fun s n ->
        let rec fn a s n =
          try
            let (x,s,n) = t.f s n in
            fn (f a x) s n
          with NoParse -> (a,s,n)
        in
        let (x,s,n) = t.f s n in
        fn (f (a ()) x) s n }

let string : string -> unit terminal = fun k ->
  if String.length k = 0 then invalid_arg "empty keyword";
  { n = sp "%S" k
  ; c = Charset.singleton k.[0]
  ; f = fun s n ->
        let l = String.length k in
        let rec fn i s n =
          if i >= l then (s,n) else
            let c,s,n = Input.read s n in
            if c <> k.[i] then raise NoParse;
            fn (i+1) s n
        in
        let (s,n) = fn 0 s n in
        ((),s,n) }

let keyword : string -> Charset.t -> unit terminal = fun k cs ->
  seq (string k) (not_charset cs) (fun _ _ -> ())

let noblank : blank = fun s n -> (s,n)

let blank_charset : Charset.t -> blank =
  fun cs s n ->
    let rec fn s n =
      let (c,s',n') = Input.read s n in
      if Charset.mem cs c then fn s' n' else (s,n)
    in
    fn s n

let blank_terminal : 'a terminal -> blank =
  fun t s n ->
    try
      let (_,s,n) = t.f s n in
      (s,n)
    with NoParse -> (s,n)

type pos = { name : string
           ; line  : int
           ; col   : int
           ; utf8_col : int
           ; phantom : bool }

let max_pos p1 p2 =
  if p1.line > p2.line then p1
  else if p1.line < p2.line then p2
  else if p1.col < p2.col then p2
  else p1

let phantom = { name = "";
                  line = 0;
                  col  = 0;
                  utf8_col = 0;
                  phantom = true }

let get_pos : buf -> int -> pos = fun b n ->
  let open Input in
  { name = filename b;
    line = line_num b;
    col = n;
    utf8_col = if !compute_utf8_col then utf8_col_num b n else (-1);
    phantom = false
  }
