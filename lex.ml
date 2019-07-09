type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a terminal = buf -> int -> 'a * buf * int

exception NoParse

let eof : 'a -> 'a terminal =
  fun x s n -> if Input.is_empty s n then (x, s, n) else raise NoParse

let char : char -> 'a -> 'a terminal =
  fun c x s n -> let (c',s,n) = Input.read s n in
               if c = c' then (x,s,n) else raise NoParse

let charset : Charset.t -> string terminal =
  fun cs s n ->
    let b = Buffer.create 32 in
    let rec fn s n =
      let (c,s',n') = Input.read s n in
      if Charset.mem cs c then (Buffer.add_char b c; fn s' n')
      else (Buffer.contents b,s,n)
    in
    fn s n

let keyword : Charset.t -> string -> unit terminal =
  fun cs k s n ->
    let l = String.length k in
    let rec fn i s n =
      if i >= l then (s,n) else
        let c,s,n = Input.read s n in
        if c <> k.[i] then raise NoParse;
        fn (i+1) s n
    in
    let (s,n) = fn 0 s n in
    let (c,_,_) = Input.read s n in
    if Charset.mem cs c then raise NoParse;
    ((),s,n)

let noblank : blank = fun s n -> (s,n)

let blank_charset : Charset.t -> blank =
  fun cs s n ->
    let rec fn s n =
      let (c,s',n') = Input.read s n in
      if Charset.mem cs c then fn s' n' else (s,n)
    in
    fn s n
