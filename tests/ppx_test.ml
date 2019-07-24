open Pacomb
open Grammar
open Comb

let test ?(blank=Lex.noblank) g s r =
  let g = compile g in
  assert (parse_string g blank s = r)

let test_fail ?(blank=Lex.noblank) g s =
  let g = compile g in
  try
    let _ = parse_string g blank s in assert false
  with Comb.Parse_error _ -> ()

let bspace = Lex.blank_charset (Charset.singleton ' ')

[%%parser
  (*  test syntax for terminal *)
  let g : unit grammar = 'a' => ()
  let _ = test g "a" ()
  let g : unit grammar = "a" => ()
  let _ = test g "a" ()
  let g : int grammar = (x::INT) => x
  let _ = test g "42" 42
  let g : float grammar = (x::FLOAT) => x
  let _ = test g "42.42E-42" 42.42E-42
  let g : string grammar = (x::RE "\\([a-zA-Z_][a-zA-Z_0-9]*\\)") => x
  let _ = test g "toto_x3" "toto_x3"
  let g : 'a -> 'a -> float grammar = fun x y -> (x=y) (z::INT) => float z
                                               ; (x<>y) (z::FLOAT) => z
  let _ = test (g 0 0) "42" 42.0
  let _ = test (g 0 1) "42.0" 42.0


  (* test patterns in terminals *)
  let g0 : (int * int) grammar = (x::INT) => Pos.(x_lpos.col, x_rpos.col)
  let _ = test ~blank:bspace g0 " 123 " (1,4)
  let g : (int * int) grammar = ((x,y)::g0) => (y,x)
  let _ = test ~blank:bspace g " 123 " (4,1)
  let g : (int * int * int) grammar = (((x,y)=z)::g0) => Pos.(y,x,z_rpos.col)
  let _ = test ~blank:bspace g " 123 " (4,1,4)
  let g : (int * int * int) grammar = ((((x:int),(y:int))=z)::g0) => Pos.(y,x,z_rpos.col)
  let _ = test ~blank:bspace g " 123 " (4,1,4)

  (* test grammar under sub expressions or sub modules *)
  let noblank = layout Lex.noblank
  let f = (x::(noblank ('a' 'a' => 2))) => x

  module H =
    struct
      let f = 'b' (x::(noblank ('a' 'a' => 2))) => x
    end
  let _ = test ~blank:bspace f "aa" 2
  let _ = test_fail ~blank:bspace f "a a"

  let _ = test ~blank:bspace H.f "b aa " 2
  let _ = test_fail H.f "b a a "

]
