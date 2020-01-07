module Pacomb = struct
  module Lex = Lex
  module Grammar = Grammar
end

type 'a data =
  { mutable leafs : 'a list
  ; mutable next  : 'a data option array }

type ('a,'b) t =
  { data : 'b data
  ; uniq : bool
  ; map  : 'a -> 'a
  ; cs   : Charset.t
  ; finl : Input.buffer -> Input.idx -> bool }

let create_data () = { leafs = []; next = Array.make 256 None }

let idt x = x

let create ?(unique=true) ?(map=idt)
      ?(cs=Charset.full) ?(final_test=fun _ _ -> true) () =
  { data = create_data () ; uniq = unique; map; cs; finl = final_test }

let reset t = t.data.leafs <- []; t.data.next <- Array.make 256 None

let save t = { leafs = t.data.leafs; next = t.data.next }

let save_and_reset t = let s = save t in reset t; s

let restore t s = t.data.leafs <- s.leafs; t.data.next <- s.next

let size { data = {leafs; next}; _} =
  let res = ref 0 in
  let rec fn {leafs; next} =
    res := !res + List.length leafs;
    Array.iter gn next
  and gn = function
    | None -> ()
    | Some d -> fn d
  in
  res := !res + List.length leafs;
  Array.iter gn next;
  !res

exception Already_bound

let next tbl c = tbl.next.(Char.code c)

let advance : bool -> (char -> char) -> 'b data -> string -> 'b data =
  fun add map tbl s ->
  let r = ref tbl in
  for i = 0 to String.length s - 1 do
    let c = map s.[i] in
    match !r.next.(Char.code c) with
    | Some tbl -> r := tbl
    | None ->
       if add then
         let tbl = create_data () in
         !r.next.(Char.code c) <- Some tbl;
         r := tbl
       else raise Not_found
  done;
  !r

let add_ascii : (char,'b) t -> string -> 'b -> unit =
  fun { data; uniq; map; cs } s v ->
    if s = "" then invalid_arg "Word_list.add_ascii: empty word";
    if not (Charset.mem cs s.[0]) then
      invalid_arg "Word_list.add: charset mismatch";
    let data = advance true map data s in
    if uniq && data.leafs <> [] then raise Already_bound;
    data.leafs <- v :: data.leafs

let mem_ascii : (char,'b) t -> string -> bool =
  fun { data; map; _ } s ->
    try
      let data = advance false map data s in
      data.leafs <> []
    with
      Not_found -> false

let add_utf8 : (string, 'b) t -> string -> 'b -> unit =
  fun { data; map; uniq; cs  } s v ->
    if s = "" then invalid_arg "Word_list.add_utf8: empty word";
    if not (Charset.mem cs s.[0]) then
      invalid_arg "Word_list.add: charset mismatch";
    let fn data s = advance true (fun c -> c) data (map s) in
    let data = Utf8.fold_grapheme fn data s in
    if uniq && data.leafs <> [] then raise Already_bound;
    data.leafs <- v :: data.leafs

let mem_utf8 : (string, 'b) t -> string -> bool =
  fun { data; map; _ } s ->
    try
      let fn data s = advance false (fun c -> c) data (map s) in
      let data = Utf8.fold_grapheme fn data s in
      data.leafs <> []
    with
      Not_found -> false

let word : ?name:string -> (char, 'a) t -> 'a Grammar.t =
  fun ?name { data = tbl; map; cs; finl; uniq } ->
    let n = Lex.default "WORD" name in
    if uniq then
      let rec f tbl s0 n0 =
        let (c,s,n) = Input.read s0 n0 in
        let c = map c in
        match next tbl c with
        | Some t -> f t s n
        | None ->
           if finl s0 n0 && tbl.leafs <> [] then (List.hd tbl.leafs, s0, n0)
           else (raise Lex.NoParse)
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.term ?name lex
    else
      let rec f tbl s0 n0 =
        let (c,s,n) = Input.read s0 n0 in
        let c = map c in
        match next tbl c with
        | Some t -> f t s n
        | None ->
           if finl s0 n0 && tbl.leafs <> [] then (tbl.leafs, s0, n0)
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
          let (g,s,n) = Lex.((any_grapheme ()).f s n) in
          let g = map g in
          f (advance false (fun c -> c) tbl g) s n
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
          let (g,s,n) = Lex.((any_grapheme ()).f s n) in
          let g = map g in
          f (advance false (fun c -> c) tbl g) s n
        with
          Not_found ->
          if finl s n && tbl.leafs <> [] then (tbl.leafs, s, n)
          else raise Lex.NoParse
      in
      let f = f tbl in
      let lex = Lex.{ n; f; a = Custom(f,Assoc.new_key ()); c = cs }
      in
      Grammar.unmerge ?name (Grammar.term lex)
