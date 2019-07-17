
open Grammar
open Combinator

let float = term(Lex.float)

(*
let float = term(Lex.appl float Lex.int)
 *)

(*
let float = Regexp.regexp_from_string "\\([-+]?[0-9]+\\([.][0-9]*\\)?\\([Ee][0-9]+\\)?\\)"
let float = term(Lex.appl (function x::_ -> float_of_string x | _ -> assert false)
                         (Lex.regexp ~name:"NUM" float))
 *)

(*
let float =
  let pos s =
    Lex.appl Buffer.contents
             (Lex.plus (Lex.charset (Charset.range '0' '9'))
                       (fun () -> let b = Buffer.create 16 in Buffer.add_string b s; b)
                       (fun b c -> Buffer.add_char b c; b))
  in
  let int = Lex.seq
              (Lex.option '+' (Lex.charset (Charset.from_string "+-")))
              (pos "")
              (fun c s -> if c = '-' then "-" ^ s else s)
  in
  let dec = Lex.option "" (Lex.seq
              (Lex.charset (Charset.singleton '.'))
              (pos ".") (fun _ x -> x))
  in
  let exp = Lex.option "" (Lex.seq
              (Lex.charset (Charset.from_string "Ee"))
              (pos "e") (fun _ x -> x))
  in
  term(Lex.seq int
               (Lex.seq dec exp (fun x y -> (x,y)))
               (fun x (y,z) -> float_of_string (x ^ y ^ z)))
 *)

let plus = term(Lex.char '+' ())
let moins = term(Lex.char '-' ())
let mul = term(Lex.char '*' ())
let div = term(Lex.char '/' ())
let opar = term(Lex.char '(' ())
let cpar = term(Lex.char ')' ())

let seq3 (a,b,c,f) = seq(seq(a,b,fun x _ -> x),c,f)

let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

let expr = fixpoint ~name:"expr" (fun expr ->
  let atom =
    alt(float,
        if show_sub then
          rpos(lpos(seq(seq(opar,expr,fun _ x -> x),cpar,
            fun x _ lpos rpos ->
              Printf.printf "%d=%d: %f\n%!" lpos.Lex.col rpos.Lex.col x;
              x)))
        else
          seq(seq(opar,expr,fun _ x -> x),cpar, fun x _ -> x)
       )
  in

  let pro = fixpoint ~name:"prod" (fun pro ->
                alt(atom,
                    alt(seq3(pro,mul,atom,( *. )),
                        seq3(pro,div,atom,( /. )))))
  in

  alt(pro,
      alt(seq3(expr,plus,pro,(+.)),
          seq3(expr,moins,pro,(-.)))))

let g = compile expr

let blank = Lex.blank_charset (Charset.singleton ' ')

let _ =
  try
    while true do
      try
        let line = input_line stdin in
        let n = parse_string g blank line in
        Printf.printf "%f\n%!" n
      with Parse_error (_,c) ->
        Printf.eprintf "parse error at %d\n%!" c
    done
  with
    End_of_file -> ()
