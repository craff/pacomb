type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a terminal = buf -> int -> 'a * buf * int

exception NoParse

val eof : 'a -> 'a terminal

val char : char -> 'a -> 'a terminal

val keyword : Charset.t -> string -> unit terminal

val charset : Charset.t -> string terminal

val blank_charset : Charset.t -> blank

val noblank : blank
