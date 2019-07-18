
open Grammar
open Combinator

let float = term(Lex.float ())
let plus = term(Lex.char '+' ())
let moins = term(Lex.char '-' ())
let mul = term(Lex.char '*' ())
let div = term(Lex.char '/' ())
let opar = term(Lex.char '(' ())
let cpar = term(Lex.char ')' ())

let seq3 a b c f = seq (seq a b (fun x _ -> x)) c f

let show_sub = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v"

let expr = fixpoint ~name:"expr" (fun expr ->
  let atom =
    alt float
        (if show_sub then
          rpos (lpos (seq (seq opar expr (fun _ x -> x)) cpar
            (fun x _ lpos rpos ->
              Printf.printf "%d=%d: %f\n%!" lpos.Lex.col rpos.Lex.col x;
              x)))
        else
          seq (seq opar expr (fun _ x -> x)) cpar (fun x _ -> x))
  in

  let pro = fixpoint ~name:"prod" (fun pro ->
                alt atom
                    (alt (seq3 pro mul atom ( *. )) (seq3 pro div atom ( /. ))))
  in

  alt pro (alt (seq3 expr plus pro (+.)) (seq3 expr moins pro (-.))))

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
