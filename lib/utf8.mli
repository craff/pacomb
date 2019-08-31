
(* possible context for column numbering in file *)
type context =
  ASCII | UTF8 | CJK_UTF8

(* [width n] return the column width of the unicode cahcar*)
val width : ?context:context -> Uchar.t -> int

type grapheme_break_property =
  | Other
  | CR
  | LF
  | Prepend
  | Control
  | Extend
  | SpacingMark
  | L
  | V
  | T
  | LV
  | LVT
  | ZWJ
  | RegionalIndicator
  | ExtPict

(* Give the grapheme break property f a charactere *)
val gbp : Uchar.t -> grapheme_break_property

type previous_chars =
  EvenRegionalIndicator | ExtPictExtendStar | NoPrevious

val encode : Uchar.t -> string

val decode : string -> int -> Uchar.t * int

val look : string -> int -> Uchar.t

val next : string -> int -> int

val prev : string -> int -> int

val of_list : Uchar.t list -> string

val to_list : string -> Uchar.t list

val fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a

val length : string -> int

val sub : string -> int -> int -> string

val grapheme_break : string -> int -> bool

val grapheme_break_after : Uchar.t list -> Uchar.t -> bool

val next_grapheme : string -> int -> int

val prev_grapheme : string -> int -> int

val fold_grapheme : ('a -> string -> 'a) -> 'a -> string -> 'a
