open Pacomb
open Lex
open Pos
open Grammar

(* factorisation ... just a test for the ppx, here left factorisation is done by
   the elimination of left recursion, to the result is the same as with
   calc_prio.ml *)

let eps = 1e-10

type assoc = RightAssoc | LeftAssoc | NonAssoc

type expr =
  | Cst of float
  | Idt of string * expr array

type func =
  | Def of (expr*string array)
  | Op0 of float
  | Op1 of (float -> float)
  | Op2 of (float -> float -> float)
  | Op3 of (float -> float -> float -> float)

let prios = ref [("^", (2.0, RightAssoc))
                ;("*", (4.0, LeftAssoc))
                ;("/", (4.0, LeftAssoc))
                ;("+", (6.0, LeftAssoc))
                ;("-", (6.0, LeftAssoc))
                ]

let env = ref [(("^",2)  , (Op2 ( ** )))
              ;(("*",2)  , (Op2 ( *. )))
              ;(("/",2)  , (Op2 ( /. )))
              ;(("+",2)  , (Op2 ( +. )))
              ;(("-",2)  , (Op2 ( -. )))
              ;(("e",0)  , (Op0 (exp 1.0)))
              ;(("exp",1), (Op1 (exp )))
              ;(("log",1), (Op1 (log )))
              ;(("pi",0) , (Op0 (acos(-1.0))))
              ;(("cos",1), (Op1 (cos )))
              ;(("sin",1), (Op1 (sin )))
              ;(("tan",1), (Op1 (tan )))
              ]

exception Unbound of string * int

let rec eval env = function
  | Cst x -> x
  | Idt(id,args) ->
     try
       let args = Array.map (eval env) args in
       let f = List.assoc (id,Array.length args) env  in
       match f with
       | Def(f,params) ->
          let env = ref env in
          let add i id = env := ((id,0),(Op0 args.(i))) :: !env in
          Array.iteri add params;
          eval !env f
       | Op0(x) -> x
       | Op1(f) -> f args.(0)
       | Op2(f) -> f args.(0) args.(1)
       | Op3(f) -> f args.(0) args.(1) args.(2)

     with Not_found -> raise (Unbound (id, Array.length args))

let%parser ident = (id::RE("[a-zA-Z][a-zA-Z0-9_]*")) => id

let%parser bin = (c::RE("[-&~^+=*/\\$!:]+\\(_[a-zA-Z0-9_]+\\)?")) => c

let%parser op pmin pmax =
  (c::bin) =>
    try let (p,a) = List.assoc c !prios in
        let good = match a with
          | NonAssoc -> pmin < p && p < pmax
          | LeftAssoc -> pmin <= p && p < pmax
          | RightAssoc -> pmin < p && p <= pmax
        in
        if not good then give_up ();
        (p,c)
    with Not_found -> give_up ()

let%parser lists sep f =
  let rec ne_slist = (x::f)                           => [x]
                   ; (l::ne_slist) (CHAR(sep)) (x::f) => x::l
  in
    () => [||]
  ; '(' (l::ne_slist) ')' => Array.of_list (List.rev l)

let%parser rec
  expr pmax = (r::dseq (expr pmax)
                  ~ae:false ~cs:(Charset.from_string "-&~^+=*/\\$!:")
                 (fun pe ->
                   dseq (op pe pmax)
                     ~ae:false ~cs:(Charset.from_string "-+0-9(a-zA-Z")
                     (fun pop -> seq (expr pop)
                                   (empty (fun (_,e2) b e1 ->
                                        (pop, Idt(b,[|e1;e2|])))))))
                                                  => r
            ; (x::FLOAT)                          => (0.0,Cst x)
            ; '(' (e::expr_top) ')'               => (0.0,e)
            ; (id::ident) (l::lists ',' expr_top) => (0.0, Idt(id,l))

and expr_top = ((__,e)::expr 1000.0) => e

let%parser assoc = "none" => NonAssoc
                 ; "left" => LeftAssoc
                 ; "right" => RightAssoc

let%parser cmd =
    (e::expr_top)
      => (fun () -> Printf.printf "%f\n%!" (eval !env e))
  ; (id::ident) (params::lists ',' ident) '=' (e::expr_top)
      => (fun () ->
          env := ((id,Array.length params),Def(e,params)) :: !env)
  ; (a1::ident) (id::bin) (a2::ident) "priority" (p::FLOAT) "assoc" (a::assoc)
                 '=' (e::expr_top)
      => (fun () ->
          let params = [|a1;a2|] in
          env := ((id,Array.length params),Def(e,params)) :: !env;
          prios := (id,(p,a)) :: !prios)

let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      let f () =
        try
          Printf.printf "=> %!";
          let line = input_line stdin in
          parse_string cmd blank line ()
        with Unbound(s,n) ->
          Printf.eprintf "unbound %s with arity %d\n%!" s n
      in handle_exception f ()
    done
  with
    End_of_file -> ()
