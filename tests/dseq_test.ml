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

let%parser [@cache] rec seq (n:int) =
   (n = 0) ()                 => []
 ; (n > 0) 'a' (l::seq (n-1)) => ('a' :: l)

let%parser [@cache] seqf =
  ((n,__)>:(n::INT => (n,()))) (l::seq n) => l

let _ = test seqf "0" []
let _ = test seqf "1 a" ['a']
let _ = test seqf "2 a a" ['a';'a']
let _ = test seqf "3 a a a" ['a';'a';'a']

let _ = test_fail seqf "1"
let _ = test_fail seqf "1 a a"

let%parser infix = '+' => (2,'+')
                 ; '*' => (1,'*')

type t = L of int | N of t * char * t

let%parser [@cache] rec term =
    (n::INT) => (0, L n)
  ; '(' ((__,t) :: term) ')' => (0,t)
  ; (t::term_infix) => t

and [@cache] term_infix =
  ((pl,tl)>:term)
      ((ps,s)::(((ps,__) = c) :: infix => (if pl > ps then Lex.give_up (); c)))
      ((pr,tr)::term)
    => (if pr >= ps then Lex.give_up (); (ps,N(tl,s,tr)))

let _ = test term "1" (0, L 1)
let _ = test term "1 + 1" (2, N(L 1,'+', L 1))
let _ = test term "1 + 1 + 1" (2, N(N(L 1,'+',L 1),'+', L 1))
let _ = test term "1 * 1" (1, N(L 1,'*', L 1))
let _ = test term "1 + 1 * 1 + 1" (2, N(N(L 1,'+', N(L 1,'*',L 1)),'+',L 1))
