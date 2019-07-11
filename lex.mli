type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a fterm = buf -> int -> 'a * buf * int
type 'a terminal = { n : string; f : 'a fterm; c : Charset.t }

exception NoParse

val eof : 'a -> 'a terminal

val char : char -> 'a -> 'a terminal

val keyword : Charset.t -> string -> unit terminal

val charset : Charset.t -> string terminal

val blank_charset : Charset.t -> blank

val noblank : blank

type pos = { name : string
           ; line  : int
           ; col   : int
           ; utf8_col : int
           ; phantom : bool }

val phantom : pos

val max_pos : pos -> pos -> pos

val get_pos : buf -> int -> pos
