
(* possible context for column numbering in file *)
type context =
  ASCII | UTF8 | CJK_UTF8

(* [width n] return the column width of the unicode cahcar*)
val width : ?context:context -> Uchar.t -> int
