
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

val look : string -> int -> Uchar.t

val next : string -> int -> int

val prev : string -> int -> int

val next_grapheme : string -> int -> int

val prev_grapheme : string -> int -> int

val fold_grapheme : (string -> 'a -> 'a) -> 'a -> string -> 'a
