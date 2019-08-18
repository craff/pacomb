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
                 ; ERROR("ident")

let%parser bin =
  (c::RE("[-&~^+=*/\\$!:]+\\(_[a-zA-Z0-9_]+\\)?"))
    => (if c = "=" then give_up ~msg:"= not valid as op bin" (); c)
; (ERROR "op bin")

let%parser op pmin pmax =
  (c::bin) =>
    try let (p,a) = List.assoc c !prios in
        let good = match a with
          | NonAssoc   -> pmin < p && p < pmax
          | LeftAssoc  -> pmin <= p && p < pmax
          | RightAssoc -> pmin < p && p <= pmax
        in
        if not good then give_up ();
        let p = match a with
          | RightAssoc -> p
          | _          -> p -. 1e-10
        in
        (p,c)
    with Not_found -> give_up ~msg:("unbound op bin "^c) ()

let%parser closing = ')' => (); ERROR "closing parenthesis"

let%parser rec
 expr pmax = ((pe,e1)>:expr pmax)
               ((pop,b)>:op pe pmax)
               ((__,e2)::expr pop)
                      =>  (pop, Idt(b,[|e1;e2|]))
            ; (x::FLOAT)                          => (0.0,Cst x)
            ; '(' (e::expr_top) closing           => (0.0,e)
            ; (id::ident) '(' (l:: ~+ [","] expr_top) closing
                                                  => (0.0, Idt(id,Array.of_list l))
            ; (id::ident)
                                                  => (0.0, Idt(id,[||]))
            ; (ERROR "expression")
and expr_top = ((__,e)::expr 1000.0) => e

let%parser assoc = "none" => NonAssoc
                 ; "left" => LeftAssoc
                 ; "right" => RightAssoc
                 ; ERROR("associtivity: none | left | right")

let%parser priority = (x::FLOAT) => x
                    ; ERROR("float")

let%parser eq = '=' => () ; ERROR("=")
let %parser priority_kwd = "priority" => (); ERROR("priority keyword")
let %parser assoc_kwd = "assoc" => (); ERROR("assoc keyword")

let%parser cmd =
    (e::expr_top)
      => (fun () -> Printf.printf "%f\n%!" (eval !env e))
  ; (id::ident) eq (e::expr_top)
      => (fun () ->
          env := ((id,0),Def(e,[||])) :: !env)
  ; (id::ident) '(' (l:: ~+ [","] ident) closing eq (e::expr_top)
      => (fun () ->
          let params = Array.of_list l in
          env := ((id,Array.length params),Def(e,params)) :: !env)
  ; (a1::ident) (id::bin) (a2::ident)
       priority_kwd (p::priority)
       assoc_kwd (a::assoc)
       eq (e::expr_top)
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
      in handle_exception ~error:(fun _ -> ()) f ()
    done
  with
    End_of_file -> ()
