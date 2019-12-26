module Pacomb = struct
  module Lex = Lex
  module Grammar = Grammar
end

type ('a,'b) data =
  { mutable leafs : 'b list
  ; next  : ('a, ('a, 'b) data) Hashtbl.t }

type ('a,'b) t =
  { data : ('a, 'b) data
  ; uniq : bool
  ; map  : 'a -> 'a
  ; cs   : Charset.t
  ; finl : Input.buffer -> Input.idx -> bool }

let create_data () = { leafs = []; next = Hashtbl.create 8 }

let idt x = x

let create ?(unique=true) ?(map=idt)
      ?(cs=Charset.full) ?(final_test=fun _ _ -> true) () =
  { data = create_data () ; uniq = unique; map; cs; finl = final_test }

let reset t = t.data.leafs <- []; Hashtbl.clear t.data.next

let size { data = {leafs; next}; _} =
  let res = ref 0 in
  let rec fn _ {leafs; next} =
    res := !res + List.length leafs;
    Hashtbl.iter fn next
  in
  res := !res + List.length leafs;
  Hashtbl.iter fn next;
  !res

type ('a,'b) fold = ('a -> 'b -> 'a) -> 'a -> 'a

exception Already_bound

let add : ('a,'b) t -> (('a,'b) data, 'a) fold -> 'b -> unit =
  fun { data; uniq; map } fold v ->
    let f data c =
      let c = map c in
      try
        Hashtbl.find data.next c
      with Not_found ->
        let r = create_data () in
        Hashtbl.add data.next c r;
        r
    in
    let data = fold f data in
    if uniq && data.leafs <> [] then raise Already_bound;
    data.leafs <- v :: data.leafs

let mem : ('a,'b) t -> (('a,'b) data, 'a) fold -> bool =
  fun { data = tbl; map; _ } fold ->
    let f tbl c =
      Hashtbl.find tbl.next (map c)
    in
    try
      let tbl = fold f tbl in
      tbl.leafs <> []
    with
      Not_found -> false

let mem_ascii : (char,'b) t -> string -> bool =
  fun tbl s ->
    let fold f a =
      let res = ref a in
      String.iter (fun c -> res := f !res c) s;
      !res
    in
    mem tbl fold

let add_ascii : (char,'b) t -> string -> 'b -> unit =
  fun tbl s v ->
    if s = "" then invalid_arg "Word_list.add: empty word";
    if not (Charset.mem tbl.cs s.[0]) then
      invalid_arg "Word_list.add: charset mismatch";
    let fold f a =
      let res = ref a in
      String.iter (fun c -> res := f !res c) s;
      !res
    in
    add tbl fold v

let mem_utf8 : (string, 'b) t -> string -> bool =
  fun tbl s ->
    mem tbl (fun f a -> Utf8.fold_grapheme f a s)

let add_utf8 : (string, 'b) t -> string -> 'b -> unit =
  fun tbl s v ->
    add tbl (fun f a -> Utf8.fold_grapheme f a s) v

let next tbl c = Hashtbl.find tbl.next c

let word : ?name:string -> (char, 'a) t -> 'a Grammar.t =
  fun ?name { data = tbl; map; cs; finl; uniq } ->
    let n = Lex.default "WORD" name in
    if uniq then
      let rec f tbl s n =
        try
          let (c,s,n) = Input.read s n in
          let c = map c in
          f (next tbl c) s n
        with
          Not_found ->
          if finl s n && tbl.leafs <> [] then (List.hd tbl.leafs, s, n)
          else raise Lex.NoParse
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.term ?name lex
    else
      let rec f tbl s n =
        try
          let (c,s,n) = Input.read s n in
          let c = map c in
          f (next tbl c) s n
        with
          Not_found ->
          if finl s n && tbl.leafs <> [] then (tbl.leafs, s, n)
        else raise Lex.NoParse
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.unmerge ?name (Grammar.term lex)

let utf8_word : ?name:string -> (string, 'a) t -> 'a Grammar.t =
  fun ?name { data = tbl; map; finl; cs; uniq } ->
    let n = Lex.default "UTF8_WORD" name in
    if uniq then
      let rec f tbl s n =
        try
          let (c,s,n) = Lex.((any_grapheme ()).f s n) in
          let c = map c in
          f (next tbl c) s n
        with
          Not_found ->
          if finl s n && tbl.leafs <> [] then(List.hd tbl.leafs, s, n)
          else raise Lex.NoParse
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.term ?name lex
    else
      let rec f tbl s n =
        try
          let (c,s,n) = Lex.((any_grapheme ()).f s n) in
          let c = map c in
          f (next tbl c) s n
        with
          Not_found ->
          if finl s n && tbl.leafs <> [] then (tbl.leafs, s, n)
          else raise Lex.NoParse
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.unmerge ?name (Grammar.term lex)
