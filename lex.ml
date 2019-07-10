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
      else if Buffer.length b = 0 then raise NoParse else (Buffer.contents b,s,n)
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
    utf8_col = utf8_col_num b n;
    phantom = false
  }
