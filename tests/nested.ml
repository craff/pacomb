open Pacomb
open Grammar
open Comb


[%%parser
  let noblank = layout Lex.noblank
  let f = (x::(noblank ('a' 'a' => 2))) => x

  module H =
    struct
      let f = 'b' (x::(noblank ('a' 'a' => 2))) => x
      let g = compile f
    end
]

let g = compile f

let blank = Lex.blank_charset (Charset.singleton ' ')

let parse_string c = Comb.parse_string c blank
let assert_fail f x =
  try ignore (f x); assert false with Comb.Parse_error _ -> ()

let _ = assert (parse_string g "aa" = 2)
let _ = assert_fail (parse_string g) "a a"

let _ = assert (parse_string H.g "b aa " = 2)
let _ = assert_fail (parse_string H.g) "b a a "
