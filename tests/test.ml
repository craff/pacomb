open Grammar
open Combinator

let parse_string c = parse_string c (Lex.blank_charset (Charset.singleton ' '))

let assert_fail f =
  try ignore (f ()); assert false with ParseError _ -> ()

let char_a = term(Lex.char 'a' 1)
let char_b = term(Lex.char 'b' 1)
let char_c = term(Lex.char 'c' 1)

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

let test8 = fixpoint
              (fun r -> alt(empty 0,
                            seq(char_a,seq(r,char_b,fun x _ -> x + 1), fun _ x -> x)))
let test9 = dseq(test8,
                 (let rec fn x =
                    if x <= 0 then empty 0
                    else seq(fn (x-1),char_a, fun x _  -> x+1 )
                       in fn),
                 fun x -> x)
let test8 = compile test8
let test9 = compile test9

let test10 = seq(char_a,layout(seq(char_a,char_b,(+)),Lex.noblank),(+))
let test10 = compile test10

let test11 = fixpoint ~name:"AB" (fun ab ->
                 let gbc =
                   fixpoint ~name:"BC" (fun bc ->
                       let gac =
                         alt(empty (),
                             alt(seq(ab,seq(char_a,char_b,fun _ _ -> ()), fun _ _ -> ()),
                                 seq(bc,seq(char_b,char_c,fun _ _ -> ()), fun _ _ -> ())))
                       in
                       alt(empty (),
                           alt(seq(ab,seq(char_a,char_b,fun _ _ -> ()), fun _ _ -> ()),
                               seq(gac,seq(char_a,char_c,fun _ _ -> ()), fun _ _ -> ()))))
                 in
                 let gac =
                   alt(empty (),
                       alt(seq(ab,seq(char_a,char_b,fun _ _ -> ()), fun _ _ -> ()),
                           seq(gbc,seq(char_b,char_c,fun _ _ -> ()), fun _ _ -> ())))
                 in
                 alt(empty (),
                     alt(seq(gbc,seq(char_b,char_c,fun _ _ -> ()), fun _ _ -> ()),
                         seq(gac,seq(char_a,char_c,fun _ _ -> ()), fun _ _ -> ()))))
let test11c = compile test11
let _ = Printf.printf "test11: %a\n%!" (print_grammar ~def:false) test11
let test11 = test11c

let test12 =
  let ab = declare_grammar ~name:"AB" () in
  let ac = declare_grammar ~name:"AC" () in
  let bc = declare_grammar ~name:"BC" () in
  set_grammar ab
              (alt(empty (),
                   alt(seq(bc,seq(char_b,char_c,fun _ _ -> ()), fun _ _ -> ()),
                       seq(ac,seq(char_a,char_c,fun _ _ -> ()), fun _ _ -> ()))));
  set_grammar ac
              (alt(empty (),
                   alt(seq(ab,seq(char_a,char_b,fun _ _ -> ()), fun _ _ -> ()),
                       seq(bc,seq(char_b,char_c,fun _ _ -> ()), fun _ _ -> ()))));
  set_grammar bc
              (alt(empty (),
                   alt(seq(ab,seq(char_a,char_b,fun _ _ -> ()), fun _ _ -> ()),
                       seq(ac,seq(char_a,char_c,fun _ _ -> ()), fun _ _ -> ()))));
  ab
let test12c = compile test12
let _ = Printf.printf "test12: %a\n%!" (print_grammar ~def:false) test12
let test12 = test12c

let _ = assert (parse_string test0 "a" = 1)
let _ = assert_fail (fun () -> parse_string test0 "")
let _ = assert_fail (fun () -> parse_string test0 "c")
let _ = assert (parse_string test0 "b" = 1)
let _ = assert (parse_string test0b "ab" = 2)
let _ = assert (parse_string test0b "a b" = 2)
let _ = assert (parse_string test0b "  a  b  " = 2)

let _ = assert (parse_string test1 (na 1) = 1)
let _ = assert (parse_string test1 "" = 0)
let _ = assert (parse_string test2 (na 10) = 10)
let _ = assert (parse_string test2 "" = 0)
let _ = assert (parse_string test2 "ababa" = 5)
let _ = assert (parse_string test3 (na 10) = 10)
let _ = assert (parse_string test3 "" = 0)
let _ = assert (parse_string test4 (na 10) = 10)
let _ = assert (parse_string test4 "" = 0)
let _ = assert (parse_string test4 "baaa" = 4)
let _ = assert (parse_string test4 "b" = 1)
let _ = assert (parse_string test5 "aaa" = 3)
let _ = assert (parse_string test5 "" = 0)
let _ = assert (parse_string test5 "ababa" = 5)
let _ = assert (parse_string test6 "a" = [1])
let _ = assert (parse_string test6 "a,aa,aaa,aa,a," = [1;2;3;2;1;0])
let _ = assert (parse_string test7 "b" = [(0,0,0)])
let _ = assert (parse_string test7 "ab" = [(0,1,1)])
let _ = assert (parse_string test7 "a,aa,aaab" = [(0,1,1);(2,2,4);(5,3,8)])
let _ = assert (parse_string test8 "" = 0)
let _ = assert (parse_string test8 "ab" = 1)
let _ = assert (parse_string test8 "aaaabbbb" = 4)
let _ = assert (parse_string test9 "" = 0)
let _ = assert (parse_string test9 "aba" = 1)
let _ = assert (parse_string test9 "aaaabbbbaaaa" = 4)
let _ = assert_fail (fun () -> parse_string test9 "aaaabbbbaaa")
let _ = assert_fail (fun () -> parse_string test9 "aaaabbbbaaaaa")
let _ = assert (parse_string test10 "aab" = 3)
let _ = assert (parse_string test10 " a ab  " = 3)
let _ = assert_fail (fun () -> parse_string test10 "a a b")

let _ = assert (parse_string test11 "" = ())
let _ = assert (parse_string test11 "ac" = ())
let _ = assert (parse_string test11 "bc" = ())
let _ = assert (parse_string test11 "bcac" = ())
let _ = assert (parse_string test11 "abac" = ())
let _ = assert (parse_string test11 "abbc" = ())
let _ = assert (parse_string test11 "acbc" = ())
let _ = assert (parse_string test11 "abbcac" = ())
let _ = assert (parse_string test11 "acbcac" = ())
let _ = assert (parse_string test11 "bcabac" = ())
let _ = assert (parse_string test11 "acabac" = ())
let _ = assert (parse_string test11 "bcabbc" = ())
let _ = assert (parse_string test11 "acabbc" = ())
let _ = assert (parse_string test11 "abacbc" = ())
let _ = assert (parse_string test11 "bcacbc" = ())


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

let _ = chrono_parse test6 (nas 100)

let _ = chrono_parse test6 (nas 1000)

let _ = chrono_parse test6 (nas 10000)

let _ = chrono_parse test6 (nas 20000)
