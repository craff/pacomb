open Grammar
open Grammar__Combinator
module Lex = Grammar__Lex

let char_a = Lex.char 'a' 1
let char_b = Lex.char 'b' 1

let test1 = fixpoint (fun r -> alt(Vide 0, seq(Term(char_a), r, (+))))
let test1 = compile test1

let test2 = fixpoint (fun r -> alt(Vide 0,
                                  alt(seq(Term(char_a), r, (+)),
                                      seq(Term(char_b), r, (+)))))
let test2 = compile test2

let test3 = fixpoint (fun r -> alt(Vide 0, seq(r, Term(char_a), (+))))
let test3 = compile test3

let test4 = fixpoint (fun r -> alt(alt(Vide 0,Term(char_b)), seq(r, Term(char_a), (+))))
let test4 = compile test4

let test5 = fixpoint (fun r -> alt(Vide 0,
                                  alt(seq(r, Term(char_a), (+)),
                                      seq(r, Term(char_b), (+)))))
let test5 = compile test5

let star g = fixpoint (fun r -> alt(seq(r, g, (+)), Vide 0))

let plus g sep =
  let g' = appl(g,fun x -> [x]) in
  fixpoint (fun r -> alt(seq(seq(g, sep, fun x _ -> x), r, (fun x y -> x::y)), g'))


let test6 = plus (star (Term(char_a))) (Term(Lex.char ',' ()))
let test6 = compile test6

let na n = String.make n 'a'

let parse_string c = parse_string c Lex.noblank

let p1 = assert (parse_string test1 (na 10) = 10)
let p1b = assert (parse_string test1 "" = 0)
let p2 = assert (parse_string test2 (na 10) = 10)
let p2b = assert (parse_string test2 "" = 0)
let p2c = assert (parse_string test2 "ababa" = 5)
let p3 = assert (parse_string test3 (na 10) = 10)
let p3b = assert (parse_string test3 "" = 0)
let p4 = assert (parse_string test4 (na 10) = 10)
let p4b = assert (parse_string test4 "" = 0)
let p4c = assert (parse_string test4 "baaa" = 4)
let p4d = assert (parse_string test4 "b" = 1)
let p5 = assert (parse_string test5 "aaa" = 3)
let p5b = assert (parse_string test5 "" = 0)
let p5c = assert (parse_string test5 "ababa" = 5)
let p6 = assert (parse_string test6 "a" = [1])
let p6b = assert (parse_string test6 "a,aa,aaa,aa,a," = [1;2;3;2;1;0])

let nas p =
  let rec fn p =
    if p = 0 then []
    else na p :: fn (p - 1)
  in
  String.concat "," (fn p)

let chrono_parse g s =
  let n = String.length s in
  Printf.printf "parsing %d chars in %!" n;
  let t0 = Unix.gettimeofday () in
  let r = parse_string g s in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%f seconds\n%!" (t1 -. t0);
  r

let p6c = chrono_parse test6 (nas 100)

let p6d = chrono_parse test6 (nas 1000)

let p6e = chrono_parse test6 (nas 10000)

let p6f = chrono_parse test6 (nas 20000)
