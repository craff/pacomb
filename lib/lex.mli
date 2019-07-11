type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** same as blank with a value returned *)
type 'a fterm = buf -> int -> 'a * buf * int
type 'a terminal = { n : string; f : 'a fterm; c : Charset.t }

(** exception when failing,
    - can be raised (but not captured) by terminal
    - can be raised (but not captured) by action code in the grammar,
      see [Combinator.give_up]
    - will be raise and captured by [Combinator.parse_buffer] that will give
      the most advanced position
 *)
exception NoParse

(** test wether a terminal accept the empty string. Such a terminal
   are illegal in a grammar, but may be used in combinator to create
   terminals *)
val accept_empty : 'a terminal -> bool

(** Combinators to create terminals *)

(** terminal accepting then end of a buffer only. remark:
   eof is automatically added at the end of a grammar by parse_buffer. *)
val eof : 'a -> 'a terminal

(** terminal accepting a given char, remark: [char '\255'] is equivalent to [eof] *)
val char : char -> 'a -> 'a terminal

(** accept a character in the given charset *)
val charset : Charset.t -> char terminal

(** test that the input is not in the given charset but read nothing *)
val not_charset : Charset.t -> unit terminal

(** compose two terminals in sequence *)
val seq : 'a terminal -> 'b terminal -> ('a -> 'b -> 'c) -> 'c terminal

(** repetition of a given terminal 0,1 or more times.
    The type allows for ['b = Buffer.t] for efficiency*)
val star : 'a terminal -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b terminal

(** repetition of a given terminal 1 or more times
    The type allows for ['b = Buffer.t] for efficiency*)
val plus : 'a terminal -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b terminal

(** accept only the given string. raises [Invalid_argument] is the string is empty *)
val string : string -> unit terminal

(** [keyword k cs = seq (string k) (not_charset cs) (fun _ _ -> ())]
    usefull to accept a keyword only when not followed by an alpha-numeric char *)
val keyword : string -> Charset.t -> unit terminal

(** function managing blanks *)

(** use when you have no blank chars *)
val noblank : blank

(** blank from a charset *)
val blank_charset : Charset.t -> blank

(** blank from a terminal *)
val blank_terminal : 'a terminal -> blank


(** functions managing positions *)

(** type to represent position *)
type pos = { name : string
           ; line  : int
           ; col   : int
           ; utf8_col : int
           ; phantom : bool }

(** a phnatom position *)
val phantom : pos

(** the max of to position (further in the file *)
val max_pos : pos -> pos -> pos

(** get a position from a buffer and a column number *)
val get_pos : buf -> int -> pos
