open Utils
open Lex
open Combinator
open Grammar

type 'a grammar = 'a Grammar.grammar
type 'a fpos = Input.buffer -> int -> Input.buffer -> int -> 'a

type blank = Lex.blank
let no_blank = Lex.noblank
let give_up = Combinator.give_up ()
let handle_exception = Combinator.handle_exception
let empty = empty
let eof x = term(Lex.eof x)
let declare_grammar = declare_grammar
let regexp ?name r = appl ?name (term ?name (Lex.regexp_grps (Regexp.from_string r)))
                                 (fun l-> Array.of_list (List.rev l))

let set_grammar = set_grammar
let char ?name c x = term ?name (Lex.char c x)
let string ?name s x = term ?name (Lex.string ?name s x)
exception Parse_error = Combinator.Parse_error
let keyword ?name k f x = term ?name (keyword k f x)
let give_name = give_name
let grammar_info = grammar_info
let grammar_family = grammar_family
let alternatives gs =
  let rec fn acc = function
    | [] -> acc
    | g1::l -> fn (alt acc g1) l
  in
  fn (fail ()) gs

let parse_buffer g bl b =
  let g = compile g in parse_buffer g bl b

let parse_string ?(filename="") grammar blank str =
  let str = Input.from_string ~filename str in
  parse_buffer grammar blank str

let parse_channel ?(filename="") grammar blank ic  =
  let str = Input.from_channel ~filename ic in
  parse_buffer grammar blank str

let parse_file grammar blank filename  =
  let str = Input.from_file filename in
  parse_buffer grammar blank str

let partial_parse_buffer g bl ?(blank_after=false) buf n =
  let g = compile g in
  partial_parse_buffer g bl ~blank_after buf n

let grammar_prio ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = EqHashtbl.create 8 in
  let is_set = ref None in
  (fun p ->
    try EqHashtbl.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p) in
      EqHashtbl.add tbl p g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f p);
      );
      g),
  (fun (gs,gp) ->
    let f = fun p ->
      alternatives (List.map snd (List.filter (fun (f,_) -> f p) gs) @ (gp p))
    in
    is_set := Some f;
    EqHashtbl.iter (fun p r ->
      set_grammar r (f p);
    ) tbl)

let grammar_prio_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = EqHashtbl.create 8 in
  let tbl2 = EqHashtbl.create 8 in
  let is_set = ref None in
  (fun args p ->
    try EqHashtbl.find tbl (args,p)
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string (args,p)) in
      EqHashtbl.add tbl (args, p) g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f args p);
      );
      g),
  (fun f ->
    let f = fun args ->
      (* NOTE: to make sure the tbl2 is filled soon enough *)
      let (gs, gp) = f args in
      try
        EqHashtbl.find tbl2 args
      with Not_found ->
        let g = fun p ->
            alternatives (List.map snd (List.filter (fun (f,_) -> f p) gs) @ gp p)
        in
        EqHashtbl.add tbl2 args g;
        g
    in
    is_set := Some f;
    EqHashtbl.iter (fun (args,p) r ->
      set_grammar r (f args p);
    ) tbl)

let accept_empty g = fst (grammar_info g)
let any = term(charset Charset.full)
let fail = fail
let apply f g = appl g f
let in_charset ?name cs = term ?name(Lex.charset cs)
let sequence = seq
let simple_dependent_sequence g1 g2 = dseq g1 g2 (fun x -> x)
let debug_lvl = ref 0
let warn_merge = ref false
let fsequence g1 g2 = seq g1 g2 (fun x f -> f x)
let fsequence_ignore g1 g2 = seq2 g1 g2
let sequence3 g1 g2 g3 f = seq g1 (seq g2 g3 (fun y z x -> f x y z)) (fun x f -> f x)
let greedy g = g (* useless ?*)

let fixpoint x g = fixpoint (fun gr -> alt (empty x) (seq gr g (fun x f -> f x)))
