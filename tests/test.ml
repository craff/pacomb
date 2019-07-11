open Grammar
open Combinator

let parse_string c = parse_string c Lex.noblank

let char_a = term(Lex.char 'a' 1)
let char_b = term(Lex.char 'b' 1)

let na n = String.make n 'a'

let test0 = alt(char_a,char_b)
let test0 = compile test0

let test0b = seq(char_a,char_b,(+))
let test0b = compile test0b

let test1 = fixpoint (fun r -> alt(empty 0, seq(char_a, r, (+))))
let test1 = compile test1

let test2 = fixpoint (fun r -> alt(empty 0,
                                  alt(seq(char_a, r, (+)),
                                      seq(char_b, r, (+)))))
let test2 = compile test2

let test3 = fixpoint (fun r -> alt(empty 0, seq(r, char_a, (+))))
let test3 = compile test3

let test4 = fixpoint (fun r -> alt(alt(empty 0,char_b), seq(r, char_a, (+))))
let test4 = compile test4

let test5 = fixpoint (fun r -> alt(empty 0,
                                  alt(seq(r, char_a, (+)),
                                      seq(r, char_b, (+)))))
let test5 = compile test5

let star g = fixpoint (fun r -> alt(seq(r, g, (+)), empty 0))

let plus g sep =
  let g' = appl(g,fun x -> [x]) in
  fixpoint (fun r -> alt(seq(seq(g, sep, fun x _ -> x), r, (fun x y -> x::y)), g'))

let test6 = plus (star (char_a)) (term(Lex.char ',' ()))
let test6 = compile test6

let star_pos g =
  let gseq = seq in
  let open Lex in
  fixpoint
    (fun r -> rpos(lpos(alt(gseq(r, g,
                                 fun (_,x,_) y lpos rpos ->
                                 (lpos.col,x+y,rpos.col)),
                            empty (fun lpos rpos -> (lpos.col,0,rpos.col))))))

let test7 = seq (plus (star_pos (char_a)) (term(Lex.char ',' ())), char_b, fun x _ -> x)

let test7 = compile test7

let p0 = assert (parse_string test0 "a" = 1)
let p0b = assert (parse_string test0 "b" = 1)
let p0c = assert (parse_string test0b "ab" = 2)
let p1 = assert (parse_string test1 (na 1) = 1)
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
let p7 = assert (parse_string test7 "b" = [(0,0,0)])
let p7b = assert (parse_string test7 "ab" = [(0,1,1)])
let p7c = assert (parse_string test7 "a,aa,aaab" = [(0,1,1);(2,2,4);(5,3,8)])

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
