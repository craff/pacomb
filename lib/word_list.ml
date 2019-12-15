module Pacomb = struct
  module Lex = Lex
  module Grammar = Grammar
end

type ('a,'b) t =
  { mutable leafs : 'b list
  ; next  : ('a, ('a, 'b) t) Hashtbl.t }

let create () = { leafs = []; next = Hashtbl.create 8 }

let size {leafs; next} =
  let res = ref 0 in
  let rec fn _ {leafs; next} =
    res := !res + List.length leafs;
    Hashtbl.iter fn next
  in
  res := !res + List.length leafs;
  Hashtbl.iter fn next;
  !res

type ('a,'b) fold = ('a -> 'b -> 'a) -> 'a -> 'a

let add : bool -> ('a -> 'a) -> ('a,'b) t
          -> (('a,'b) t, 'a) fold -> 'b -> unit =
  fun repl map tbl fold v ->
    let f tbl c =
      let c = map c in
      try
        Hashtbl.find tbl.next c
      with Not_found ->
        let r = create () in
        Hashtbl.add tbl.next c r;
        r
    in
    let tbl = fold f tbl in
    tbl.leafs <- if repl then [v] else v :: tbl.leafs

let mem : ('a -> 'a) -> ('a,'b) t -> (('a,'b) t, 'a) fold -> bool =
  fun map tbl fold ->
    let f tbl c =
      Hashtbl.find tbl.next (map c)
    in
    try
      let tbl = fold f tbl in
      tbl.leafs <> []
    with
      Not_found -> false

let idt x = x

let mem_ascii : ?map:(char -> char) -> (char,'b) t -> string -> bool =
  fun ?(map=idt) tbl s ->
    let fold f a =
      let res = ref a in
      String.iter (fun c -> res := f !res c) s;
      !res
    in
    mem map tbl fold

let add_ascii : bool -> (char -> char) -> (char,'b) t -> string -> 'b -> unit =
  fun repl map tbl s v ->
    let fold f a =
      let res = ref a in
      String.iter (fun c -> res := f !res c) s;
      !res
    in
    add repl map tbl fold v

let replace_ascii ?(map=idt) tbl s v = add_ascii true map tbl s v
let add_ascii     ?(map=idt) tbl s v = add_ascii false map tbl s v

let mem_utf8 : ?map:(string -> string)
               -> (string, 'b) t -> string -> bool =
  fun ?(map=idt) tbl s ->
    mem map tbl (fun f a -> Utf8.fold_grapheme f a s)

let add_utf8 : bool -> (string -> string)
               -> (string, 'b) t -> string -> 'b -> unit =
  fun repl map tbl s v ->
    add repl map tbl (fun f a -> Utf8.fold_grapheme f a s) v

let replace_utf8 ?(map=idt) tbl s v = add_utf8 true map tbl s v
let add_utf8     ?(map=idt) tbl s v = add_utf8 false map tbl s v

let next tbl c =
  try Hashtbl.find tbl.next c with Not_found -> raise Lex.NoParse

let parse_char : (char -> char) -> (char, 'a) t -> 'a Grammar.t =
  fun map tbl ->
    let%parser rec p tbl =
      (x::Grammar.alt (List.map Grammar.empty tbl.leafs))      => x
      ; ((c,__)>:((c::CHAR) => (map c,()))) (x::p (next tbl c)) => x
    in
    p tbl

let word : ?name:string -> ?final_test:(Input.buffer -> Input.pos -> bool)
           -> ?map:(char -> char) -> (char, 'a) t -> 'a Grammar.t =
  fun ?name ?(final_test=fun _ _ -> true) ?(map=fun c -> c) tbl ->
  Grammar.(layout ?name Blank.none
             (test_after (fun _ _ _ -> final_test) (parse_char map tbl)))

let parse_utf8 : (string -> string) -> (string, 'a) t -> 'a Grammar.t =
  fun map tbl ->
    let%parser rec p tbl =
      (x::Grammar.alt (List.map Grammar.empty tbl.leafs))      => x
      ; ((c,__)>:((c::GRAPHEME) => (map c,()))) (x::p (next tbl c)) => x
    in
    p tbl

let utf8_word : ?name:string -> ?final_test:(Input.buffer -> Input.pos -> bool)
           -> ?map:(string -> string) -> (string, 'a) t -> 'a Grammar.t =
  fun ?name ?(final_test=fun _ _ -> true) ?(map=fun c -> c) tbl ->
  Grammar.(layout ?name Blank.none
             (test_after (fun _ _ _ -> final_test) (parse_utf8 map tbl)))
