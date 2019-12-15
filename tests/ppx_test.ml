open Pacomb
open Pos
open Grammar

let bspace = Blank.from_charset (Charset.singleton ' ')

let test ?(blank=bspace) g s r =
  assert (parse_string g blank s = r)

let tests ?(blank=bspace) g l =
  List.iter (fun (s,r) -> test ~blank g s r) l

let test_fail ?(blank=bspace) g s =
  try
    let _ = parse_string g blank s in assert false
  with Parse_error _ -> ()

let tests_fail ?(blank=bspace) g l =
  List.iter (test_fail ~blank g) l

(*  test syntax for terminal *)
let%parser g : unit grammar = () => ()
let _ = test g "" ()
let%parser g : unit grammar = 'a' => ()
let _ = test g "a" ()
let%parser g : unit grammar = "a" => ()
let _ = test g "a" ()
let%parser g : int grammar = (x::INT) => x
let _ = test g "42" 42
let%parser g : float grammar = (x::FLOAT) => x
let _ = test g "42.42E-42" 42.42E-42
let%parser g : string grammar = (x::RE "\\([a-zA-Z_][a-zA-Z_0-9]*\\)") => x
let _ = test g "toto_x3" "toto_x3"
let%parser g : 'a -> 'a -> float grammar =
  fun x y -> (x=y) (z::INT) => float_of_int z
           ; (x<>y) (z::FLOAT) => z
let _ = test (g 0 0) "42" 42.0
let _ = test (g 0 1) "42.0" 42.0


(* test patterns in terminals *)
let%parser g0 : (int * int) grammar = (x::INT) => Pos.(x_lpos.col, x_rpos.col)
let _ = test g0 " 123 " (1,4)
let%parser g : (int * int) grammar = ((x,y)::g0) => (y,x)
let _ = test g " 123 " (4,1)
let%parser g : (int * int * int) grammar
  = (((x,y)=z)::g0) => Pos.(y,x,z_rpos.col)
let _ = test g " 123 " (4,1,4)
let%parser g : (int * int * int) grammar =
  ((((x:int),(y:int))=z)::g0) => Pos.(y,x,z_rpos.col)
let _ = test g " 123 " (4,1,4)

(* test rules and sequences *)
type op = Add | Sub | Mul | Div
let%parser bin = (x::INT)
                   (op::('+'=>Add ; '-'=>Sub; '*'=>Mul; '/'=>Div))
                   (y::INT) => (x,op,y)
let _ = test bin "42 + 73" (42,Add,73)
let%parser g = (x::INT) 'a' 'b' => x
             ; 'a' (x::INT) 'b' => x
             ; 'a' 'b' (x::INT) => x
let _ = tests g [("42 a b",42); ("a 42 b",42); ("a b 42",42)]

(* test positions *)
let%parser g = (x::INT) 'a' 'b' => Pos.(x_lpos.col,x,x_rpos.col)
             ; 'a' (x::INT) (b::'b') => Pos.(x_lpos.col,x,b_rpos.col)
             ; (a::'a') 'b' (x::INT) => Pos.(a_lpos.col,x,x_rpos.col)
let _ = tests g [("42 a b ",(0,42,2))
               ; ("a 42 b ",(2,42,6))
               ; ("a b 42 ",(0,42,6))]
let%parser g = (x::bin) 'a' 'b' => Pos.(x_lpos.col,x,x_rpos.col)
             ; 'a' (x::bin) (b::'b') => Pos.(x_lpos.col,x,b_rpos.col)
             ; (a::'a') 'b' (x::bin) => Pos.(a_lpos.col,x,x_rpos.col)
let _ = tests g [("42+13 a b ",(0,(42,Add,13),5))
               ; ("a 42 * 4 b ",(2,(42,Mul,4),10))
               ; ("a b 42 / 2 ",(0,(42,Div,2),10))]
let%parser g = 'a' 'b' => Pos.(_pos.start.col,_pos.end_.col)
let _ = tests g [("a b ",(0,3))]

(* test recursion *)
let%parser rec g = (y::g) (x::INT) => x+y
                 ; (x::INT) => x
let _ = tests g [("42", 42); ("1 2 3",6)]
let%parser rec g = (x::INT) (y::g) => x+y
                 ; (x::INT) => x
let _ = tests g [("42", 42); ("1 2 3",6)]
let%parser rec g = (x::INT) '+' (y::g) => x+y
                 ; (x::g) '-' (y::INT) => x-y
                 ; (x::INT) => x
(* This is ambiguous !!! *)
let _ = tests g [("42", 42); ("1 + 2 - 3",0); ("1 - 2 - 3",-4)]
let%parser rec g1 = (x::g3) 'a' 'b' => x+1
                  ; 'c' (x::g1) 'd' => x-1
                  ; 'e' 'f' (x::g2) => x
                  ; ()              => 0
and g2 = (x::g1) 'b' 'a' => x+1
       ; 'c' (x::g2) 'd' => x-1
       ; 'f' 'e' (x::g3) => x
       ; ()              => 0
and g3 = (x::g2) 'a' 'b' => x+1
       ; 'd' (x::g3) 'c' => x-1
       ; 'e' 'f' (x::g1) => x
       ; ()              => 0
let _ = tests g1 [("", 0); ("ab",1); ("cd",-1); ("ef", 0)]
let _ = tests g2 [("", 0); ("ba",1); ("cd",-1); ("fe", 0)]
let _ = tests g3 [("", 0); ("ab",1); ("dc",-1); ("ef", 0)]
let _ = tests g1 [("cdefefcfedcabd",-2)]

(* test parameters *)
let%parser rec g g0 n = (n=0) ()                 => 0
                      ; (n>0) (x::g g0 (n-1)) g0 => x+1
let%parser _ =
  for i = 0 to 10 do
    test (g ('a' => ()) i) (String.make i 'a') i;
    test_fail (g ('a' => ()) i) (String.make (i+1) 'a')
  done

let%parser rec g g0 n = (n=0) ()                 => 0
                      ; (n>0) g0 (x::g g0 (n-1)) => x+1
let%parser _ =
  for i = 0 to 10 do
    test (g ('a' => ()) i) (String.make i 'a') i;
    test_fail (g ('a' => ()) i) (String.make (i+1) 'a')
  done

let%parser rec g ~g:g0 ?(n=0) () = (n=0) ()               => 0
                      ; (n>0) g0 (x::g ~g:g0 ~n:(n-1) ()) => x+1
let%parser _ =
  test (g ~g:('a' => ()) ()) (String.make 0 'a') 0;
  test_fail (g ~g:('a' => ()) ()) (String.make 1 'a');
  for i = 0 to 10 do
    test (g ~g:('a' => ()) ~n:i ()) (String.make i 'a') i;
    test_fail (g ~g:('a' => ()) ~n:i ()) (String.make (i+1) 'a')
  done


(* test grammar under sub expressions or sub modules *)
let noblank = layout Blank.none
let%parser f = (x::(noblank ('a' 'a' => 2))) => x

module%parser H =
  struct
    let f = 'b' (x::(noblank ('a' 'a' => 2))) => x
  end
let _ = test f "aa" 2
let _ = test_fail f "a a"

let _ = test H.f "b aa " 2
let _ = test_fail H.f "b a a "

type tree = Nil of int | Bin of tree * tree | Alt of tree * tree
let rec nb_tree = function
  | Nil _ -> 1
  | Bin(t1,t2) -> nb_tree t1 * nb_tree t2
  | Alt(t1,t2) -> nb_tree t1 + nb_tree t2

let size t =
  let adone = ref [] in
  let rec fn t =
    if List.memq t !adone then 0 else
      begin
        adone := t :: !adone;
        match t with
        | Nil _ -> 0
        | Bin(t1,t2) -> fn t1 + fn t2 + 1
        | Alt(t1,t2) -> fn t1 + fn t2 + 1
      end
  in fn t

let catalan_fn =
  let memo = Hashtbl.create 128 in
  let rec fn n =
    if n = 0 then 1 else if n = 1 then 1 else
    try Hashtbl.find memo n
    with Not_found ->
      let r = ref 0 in
      for i = 0 to n-1 do
        r := fn i * fn (n - i - 1) + !r
      done;
      Hashtbl.add memo n !r;
      !r
  in
  fn
                                    (* test cache and merge attribute *)
let%parser [@merge (fun x y -> Alt(x,y))] rec catalan =
    (x::INT) => Nil x
  ; (t1::catalan) ',' (t2::catalan) => Bin(t1,t2)

let _ = nb_tree (parse_string catalan bspace "0,0,0,0") = catalan_fn 4
let _ = nb_tree (parse_string catalan bspace "0,0,0,0,0,0") = catalan_fn 6
