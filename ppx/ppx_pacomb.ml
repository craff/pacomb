let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Longident
open Location

(* Someone above is overriding these ?*)
let (=) = Stdlib.(=)
let (<>) = Stdlib.(<>)

let cache_att =
  let open Ppxlib in
  Attribute.(declare "cache"
               Context.Value_binding
               Ast_pattern.(pstr nil)
               ())

let merge_att =
  let open Ppxlib in
  Attribute.(declare "merge"
               Context.Value_binding
               Ast_pattern.(single_expr_payload __)
               (fun x -> x))

let layout_att =
  let open Ppxlib in
  Attribute.(declare "layout"
               Context.Value_binding
               Ast_pattern.(single_expr_payload __)
               (fun x -> x))

let unit_ = let loc = none in [%expr ()]

(* make a location from two *)
let merge_loc loc1 loc2 =
  if loc2.loc_ghost then loc1
  else if loc1.loc_ghost then loc2
  else { loc1 with loc_end = loc2.loc_end }

(* an exception to issue a warning when an expression is probably a grammar
   but tranformation fails, may be for a syntax error*)
exception Warn of attribute

let warn loc msg = raise (Warn (attribute_of_warning loc msg))

let add_attribute exp att =
  { exp with pexp_attributes = att :: exp.pexp_attributes }

(* a mapper to test if an identifier occurs. return true also
   if it occuer bounded, this is correct because we only need
   that if it returns false then it does not occur *)
let has_ident id e =
  let found = ref false in
  let iter =
    object
      inherit Ast_traverse.iter as super
      method! expression exp =
        match exp.pexp_desc with
        | Pexp_ident { txt = Lident id' } when id' = id ->
           found := true;
        | _ -> super#expression exp
    end
  in
  let _ = iter#expression e in
  !found

(* transform an expression in a pattern
   - "_" does not work. use "__" instead
   - (pat = lid) is the synta for as pattern *)
let rec exp_to_pattern e =
  let loc = e.pexp_loc in
  match e with
  | {pexp_desc = Pexp_ident({txt = Lident name; loc = loc_s})} ->
     let name = mkloc name loc_s in
     (Some name, Pat.var ~loc name)
  | [%expr [%e? e] = [%e? {pexp_desc = Pexp_ident({txt = Lident name
                                                  ;loc = loc_s})}]]
     ->
     let name = mkloc name loc_s in
     let (_, pat) = exp_to_pattern e in
     (Some name, Pat.alias pat name)
  | [%expr ([%e? e] : [%t? t])] ->
     let (name, pat) = exp_to_pattern e in
     (name, [%pat? ([%p pat] : [%t t])])
  | {pexp_desc = Pexp_tuple(l)} ->
     let (_,pats) = List.split (List.map exp_to_pattern l) in
     (None, Pat.tuple ~loc pats)
  | [%expr lazy [%e? e]] ->
     let (name, pat) = exp_to_pattern e in
     (name, [%pat? lazy [%p pat]])
(* NOTE: the next line works wirh ocaml 4.08.1 and 4.09.0, but not with
   4.07.1 ???, best abandon let open in pattern, not so useful  *)
(*| [%expr let open [%m? { pmod_desc = Pmod_ident m }] in [%e? e]] ->*)
(*| { pexp_desc = Pexp_open({popen_expr = { pmod_desc = Pmod_ident m }}, e)} ->
     let (name, pat) = exp_to_pattern e in
     (name, Pat.open_ ~loc m pat)*)
  | { pexp_desc = Pexp_construct(c,a) } ->
     (None, Pat.construct c (match a with
                     | None -> None
                     | Some e -> Some(snd (exp_to_pattern e))))
  | _ -> warn loc "expression left of \"::\" does not represent a pattern"

(* transform an expression into a terminal *)
let rec exp_to_term exp =
  let loc = exp.pexp_loc in
  match exp with
  | {pexp_desc = Pexp_constant (Pconst_char _)} ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.char [%e exp] ())]
  | [%expr CHAR] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.any ())]
  | [%expr CHAR([%e? s])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.char [%e s] ())]
  | [%expr CHARSET([%e? s])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.charset [%e s] ())]
  | {pexp_desc = Pexp_constant (Pconst_string _)} ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.string [%e exp] ())]
  | [%expr STR([%e? s])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.string [%e s] ())]
  | [%expr NAT] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.nat ())]
  | [%expr INT] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.int ())]
  | [%expr FLOAT] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.float ())]
  | [%expr STRING_LIT] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.string_lit ())]
  | [%expr CHAR_LIT] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.char_lit ())]
  | [%expr UTF8] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.any_utf8 ())]
  | [%expr UTF8([%e? c])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.utf8 [%e c] ())]
  | [%expr GRAPHEME] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.any_grapheme ())]
  | [%expr GRAPHEME([%e? c])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.grapheme [%e c] ())]
  | [%expr EOF] ->
     [%expr Pacomb.Grammar.term (Pacomb.Lex.eof ())]
  | [%expr RE([%e? s])] ->
     [%expr Pacomb.Grammar.term (Pacomb.Regexp.regexp
                                  (Pacomb.Regexp.from_string [%e s]))]
  | [%expr ~? [ [%e? default] ] [%e? exp] ] ->
     [%expr Pacomb.Grammar.default_option [%e default] [%e exp_to_term exp]]
  | [%expr ~? [%e? exp]] ->
     [%expr Pacomb.Grammar.option [%e exp_to_term exp]]
  | [%expr ~* [ [%e? sep] ] [%e? exp]] ->
     [%expr Pacomb.Grammar.star_sep [%e exp_to_term sep] [%e exp_to_term exp]]
  | [%expr ~* [%e? exp]] ->
     [%expr Pacomb.Grammar.star [%e exp_to_term exp]]
  | [%expr ~+ [ [%e? sep] ] [%e? exp]] ->
     [%expr Pacomb.Grammar.plus_sep [%e exp_to_term sep] [%e exp_to_term exp]]
  | [%expr ~+ [%e? exp]] ->
     [%expr Pacomb.Grammar.plus [%e exp_to_term exp]]
  | _ -> exp

(* treat each iterm in a rule. Accept (pat::term) or term when
   - pat is an expression accepted by exp_to_pattern
   - term is an expression accepted by exp_to_term *)
let exp_to_rule_item (e, loc_e) =  match e with
  | [%expr [%e? epat] :: [%e? exp]] ->
     let (name, pat) = exp_to_pattern epat in
     (Some (name, pat), None, exp_to_term exp, loc_e)
  | [%expr ([%e? dpat], [%e? epat]) >: [%e? exp]] ->
     let (name, pat) = exp_to_pattern epat in
     let (_, dpat) = exp_to_pattern dpat in
     (Some (name, pat), Some dpat, exp_to_term exp, loc_e)
  | _ ->
     (None, None, exp_to_term e, loc_e)

type cond =
  CondMatch of expression * expression
| CondTest of expression
| CondNone

(* transform exp into rule, that is list of rule item. Accept
   - a sequence of items (that is applications of items)
   - or () to denote the empty rule *)
let rec exp_to_rule e =
  let loc_e = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_apply({ pexp_desc =
      Pexp_apply({ pexp_desc =
        Pexp_ident
          { txt = Lident("="|"<"|">"|"<="|">="|"<>"
                         |"=="|"!="|"<<="|">>="|"==="
                         |"not"|"&&"|"||"|"=|" as sym)}}
               , [(Nolabel,a0);(Nolabel,a1)])}
               as cond,
      (Nolabel,a3)::rest)  ->
     let (rule,_) =
       exp_to_rule (if rest <> [] then Exp.apply a3 rest else a3)
     in
     let cond = if sym = "=|" then CondMatch(a0,a1) else CondTest(cond) in
     (rule, cond)
  | Pexp_construct({txt = Lident "()"; loc}, None) ->
     ([None, None, [%expr Pacomb.Grammar.empty ()], loc_e], CondNone)
  | Pexp_apply(e1, args) ->
     let e1, args = match e1, args with
       | (([%expr (~* )] | [%expr (~+) ] | [%expr (~?) ])
          , ((Nolabel, ([%expr [__]] as a1))::(Nolabel, a2)::args))
         -> let loc = merge_loc e1.pexp_loc a2.pexp_loc in
            ([%expr [%e e1] [%e a1] [%e a2]], args)
       | (([%expr (~* )] | [%expr (~+) ] | [%expr (~?) ]) , (Nolabel, a1)::args)
         -> let loc = merge_loc e1.pexp_loc a1.pexp_loc in
            ([%expr [%e e1] [%e a1]], args)
       | _ -> (e1, args)
     in
     let kn (_,e') = (e',merge_loc e'.pexp_loc loc_e) in
     let l = (e1, e.pexp_loc) :: List.map kn args in
     (List.map exp_to_rule_item l, CondNone)
  | _ ->
     ([exp_to_rule_item (e, e.pexp_loc)], CondNone)

let rec base_rule acts_fn rule action =
  let (rule,cond) = exp_to_rule rule in
  let loc_a = action.pexp_loc in
  let gl_pos = mknoloc ("_pos") in
  let gl_lpos = mknoloc ("_lpos") in
  let gl_rpos = mknoloc ("_rpos") in
  let has_gl_pos = has_ident gl_pos.txt action in
  let has_gl_lpos = has_gl_pos || has_ident gl_lpos.txt action in
  let has_gl_rpos = has_gl_pos || has_ident gl_rpos.txt action in
  let acts_fn =
    if has_gl_pos then
      let pat = Pat.var gl_pos in
      let vb = [
          Vb.mk pat
            (Exp.record
               [( mknoloc (Ldot(Lident "Pos","start"))
                , Exp.ident (mknoloc (Lident gl_lpos.txt)))
               ;( mknoloc (Ldot(Lident "Pos","end_"))
                , Exp.ident (mknoloc (Lident gl_rpos.txt)))]
               None)]
      in
      (fun exp -> Exp.let_ Nonrecursive vb (acts_fn exp))
    else acts_fn
  in
  let acts_fn =
    if has_gl_lpos then
      (fun exp -> let loc = exp.pexp_loc in
                  [%expr fun _lpos -> [%e (acts_fn exp)]])
    else
      acts_fn
  in
  let acts_fn =
    if has_gl_rpos then
      (fun exp -> let loc = exp.pexp_loc in
                  [%expr fun _rpos -> [%e (acts_fn exp)]])
    else
      acts_fn
  in
  let gn (acts_fn, rule) (name, dep, item, loc_e) = match name with
    | None    ->
       let pat = Pat.any () in
       let acts_fn exp =
         Exp.fun_ ~loc:loc_a Nolabel None pat (acts_fn exp)
       in
       (acts_fn, (false, false, dep, item, loc_e) :: rule)
    | Some (None, pat) ->
       let acts_fn exp =
         Exp.fun_ ~loc:loc_a Nolabel None pat (acts_fn exp)
       in
       (acts_fn, (false,false,dep,item,loc_e) :: rule)
    | Some (Some id, pat) ->
       let id_pos = mkloc (id.txt ^ "_pos") id.loc in
       let id_lpos = mkloc (id.txt ^ "_lpos") id.loc in
       let id_rpos = mkloc (id.txt ^ "_rpos") id.loc in
       let has_id = has_ident id.txt action in
       let has_id_pos = has_ident id_pos.txt action in
       let has_id_lpos = has_id_pos || has_ident id_lpos.txt action in
       let has_id_rpos = has_id_pos || has_ident id_rpos.txt action in
       let pat =
         match has_id_lpos, has_id_rpos with
         | false, false -> pat
         | true, false -> Pat.tuple [Pat.var id_lpos; pat]
         | false, true -> Pat.tuple [pat; Pat.var id_rpos]
         | true, true -> Pat.tuple [Pat.var id_lpos; pat; Pat.var id_rpos]
       in
       let acts_fn =
         if has_id_pos then
           let pat = Pat.var id_pos in
           let vb = [
               Vb.mk pat
                 (Exp.record
                    [( mknoloc (Ldot(Lident "Pos","start"))
                     , Exp.ident (mkloc (Lident id_lpos.txt) id.loc))
                    ;( mknoloc (Ldot(Lident "Pos","end_"))
                     , Exp.ident (mkloc (Lident id_rpos.txt) id.loc))]
                    None)]
           in
           (fun exp -> Exp.let_ Nonrecursive vb (acts_fn exp))
         else acts_fn
       in
       let acts_fn exp =
         let loc = exp.pexp_loc in
         (* add ignore(id) if we only use position *)
         if not has_id && (has_id_lpos || has_id_rpos) then
           begin
             let id = Exp.ident (mkloc (Lident id.txt) id.loc) in
             [%expr fun [%p pat] -> ignore [%e id]; [%e acts_fn exp]]
           end
         else
           [%expr fun [%p pat] -> [%e acts_fn exp]]
       in
       (acts_fn, (has_id_lpos,has_id_rpos,dep,item,loc_e) :: rule)
  in
  let (acts_fn, rule) = List.fold_left gn (acts_fn, []) rule in
  let rule = List.rev rule in
  let action =
    try exp_to_grammar ~acts_fn action
    with
    | Exit ->
       let loc = action.pexp_loc in
       [%expr Pacomb.Grammar.empty [%e acts_fn action]]
    | Warn att ->
       let loc = action.pexp_loc in
       let exp = [%expr Pacomb.Grammar.empty [%e acts_fn action]] in
       add_attribute exp att
  in
  let fn (lpos,rpos,dep,item,loc_e) exp =
    let loc = merge_loc loc_e exp.pexp_loc in
    let f = match (lpos,rpos,dep) with
      | false, false, None -> [%expr Pacomb.Grammar.seq]
      | true , false, None -> [%expr Pacomb.Grammar.seq_lpos]
      | false, true , None -> [%expr Pacomb.Grammar.seq_rpos]
      | true , true , None -> [%expr Pacomb.Grammar.seq_pos]
      | false, false, Some(_) -> [%expr Pacomb.Grammar.dseq]
      | true , false, Some(_) -> [%expr Pacomb.Grammar.dseq_lpos]
      | false, true , Some(_) -> [%expr Pacomb.Grammar.dseq_rpos]
      | true , true , Some(_) -> [%expr Pacomb.Grammar.dseq_pos]
    in
    let exp = match dep with
      | None     -> exp
      | Some pat -> [%expr fun [%p pat] -> [%e exp]]
    in
    [%expr [%e f] [%e item] [%e exp]]
  in
  let rule = List.fold_right fn rule action in
  let rule =
    if has_gl_rpos then
      let loc = rule.pexp_loc in
      [%expr Pacomb.Grammar.rpos [%e rule]]
    else rule
  in
  let rule =
    if has_gl_lpos then
      let loc = rule.pexp_loc in
      [%expr Pacomb.Grammar.lpos [%e rule]]
    else rule
  in
  match cond with
  | CondNone -> rule
  | CondTest (cond) ->
     let loc = rule.pexp_loc in
     [%expr if [%e cond] then [%e rule] else Pacomb.Grammar.fail ()]
  | CondMatch(a0,a1) ->
     let (_,pat) = exp_to_pattern a1 in
     let loc = rule.pexp_loc in
     [%expr match [%e a0] with [%p pat] -> [%e rule]
                             | _ -> Pacomb.Grammar.fail ()]

(* transform an expression into a list of rules with action
   - name_param is an optional arguments for an eventual parameter name
   - fn is a function to modify the action. It adds [Exp.fun _] conctructs *)
and exp_to_rules ?name_param ?(acts_fn=(fun exp -> exp)) e =
  match e with
  (* base case [items => action] *)
  | [%expr [%e? rule] => [%e? action]] ->
     [base_rule acts_fn rule action]
  | [%expr [%e? rule] ==> [%e? action]] ->
     let rule = base_rule acts_fn rule action in
     let loc = e.pexp_loc in
     [[%expr Pacomb.Grammar.eval [%e rule]]]
  (* inheritance case [prio1 < prio2 < ... < prion] *)
  | [%expr [%e? _] < [%e? _]] when name_param <> None ->
     let rec fn exp = match exp with
       | [%expr [%e? x] < [%e? y]] -> y :: fn x
       | _ -> [exp]
     in
     let prios = fn e in
     let (name,param,_,_) =
       match name_param with None -> assert false
                           | Some x -> x
     in
     let param = mknoloc (Lident param) in
     let loc = e.pexp_loc in
     let rec gn acc l =
       match l with
       | x::(y::_ as l) ->
          let e = [%expr Pacomb.Grammar.seq2 (Pacomb.Grammar.cond
                                         ([%e Exp.ident param] = [%e x]))
                      ([%e Exp.ident name] [%e y])]
          in gn (e::acc) l
       | [] | [_] -> acc
     in
     gn [] prios
  | [%expr ERROR([%e? s])] ->
     let loc = e.pexp_loc in
     [ [%expr Pacomb.Grammar.error [%e s]] ]
  (* alternatives represented as sequence *)
  | [%expr [%e? e1]; [%e? e2]] ->
     exp_to_rules ?name_param ~acts_fn e1
     @ exp_to_rules ?name_param ~acts_fn e2
  (* not a grammar at all (no warning)! *)
  | _ -> raise Exit

(* transform an expression into grammar, by adding [alt] combinators
   to the result of exp_to_rules *)
and exp_to_grammar ?name_param ?(acts_fn=(fun exp -> exp)) exp =
  let rules = exp_to_rules ?name_param ~acts_fn exp in
  let loc = exp.pexp_loc in
  match rules with (* three cases for better location ? *)
  | [] -> [%expr Pacomb.Grammar.fail ()]
  | [x] -> x
  | _ ->
     let rec fn = function
       | [] -> [%expr []]
       | x::l ->
          let exp = fn l in
          let loc = merge_loc x.pexp_loc exp.pexp_loc in
          [%expr [%e x] :: [%e exp]]
     in
     let exp = fn rules in
     [%expr Pacomb.Grammar.alt [%e exp]]

(* remove acts_fn argument and handle exceptions *)
let exp_to_grammar ?name_param exp =
  try (true, exp_to_grammar ?name_param exp)
  with Exit     -> (false, exp)
     | Warn att -> (false, add_attribute exp att)

(* transform a list of structure_items in one *)
let flatten_str items =
  match items with
  | [x] -> x
  | _ ->
     Str.include_ { pincl_mod = Mod.structure items
                  ; pincl_loc = Location.none
                  ; pincl_attributes = [] }

let gen_id =
  let c = ref 0 in
  (fun s -> incr c; s ^(string_of_int !c))

let vb_to_parser rec_ vb =
  let gn vb =
    let loc = vb.pvb_loc in
    let rec treat_pat p = match p.ppat_desc with
      | Ppat_var s           -> (s         , false)
      | Ppat_alias(_,s)      -> (s         , false)
      | Ppat_open(_,p)       -> treat_pat p
      | Ppat_constraint(p,_) -> treat_pat p
      | _                    -> (mknoloc "", true )
    in
    let (name,do_warn) = treat_pat vb.pvb_pat in
    let (params,exp) =
      let rec fn exp =
        match exp.pexp_desc with
        | Pexp_fun (lbl, def, param, exp) when rec_ = Recursive ->
           let (params, exp) = fn exp in
           ((lbl,def,param)::params, exp)
        | _ -> ([], exp)
      in
      fn vb.pvb_expr
    in
    let (name, param) = match params with
        []  -> (name, None)
      | [(Nolabel,None,p)] -> (name, Some (p,None))
      | ps  ->
         let curry = List.map (fun (lbl,def,_) -> (lbl,def)) ps in
         let ps = List.map (fun (_,_,p) -> p) ps in
         ( mkloc (name.txt^"@uncurry") name.loc
         , Some(Pat.tuple ~loc:vb.pvb_expr.pexp_loc (ps), Some curry))
    in
    let name_param = match param with
      | None -> None
      | Some (p,curry) ->
         Some ( mkloc (Lident name.txt) name.loc
              , gen_id "@p"
              , p, curry)
    in
    let (changed,rules) = exp_to_grammar ?name_param exp in
    if changed && do_warn then
      warn vb.pvb_pat.ppat_loc
        "Pattern not allowed here for grammar parameter";
    let rules =
      match Ppxlib.Attribute.get layout_att vb with
      | Some [%expr [%e? blank] ~config:[%e? config]]  ->
        [%expr Pacomb.Grammar.layout ~config:[%e config] [%e blank] [%e rules]]
      | Some [%expr [%e? blank] ]  ->
        [%expr Pacomb.Grammar.layout [%e blank] [%e rules]]
      | None   -> rules
    in
    let rules =
      match Ppxlib.Attribute.get merge_att vb with
      | Some e ->
        [%expr Pacomb.Grammar.cache ~merge:[%e e] [%e rules]]
      | None   -> rules
    in
    let rules =
      match Ppxlib.Attribute.get cache_att vb with
      | Some _ -> [%expr Pacomb.Grammar.cache [%e rules]]
      | None   -> rules
    in
    (loc,changed,name,vb.pvb_pat,name_param,rules)
  in
  let ls = List.map gn vb in
  if not (List.exists (fun (_,changed,_,_,_,_) -> changed) ls)
  then raise Exit;
  let (gr,orig) = List.partition
                    (fun (_,changed,_,_,_,_) -> changed && rec_ = Recursive)
                    ls
  in
  let set name = "set__grammar__" ^ name.txt in
  let declarations =
    let gn (loc,changed,(name:string loc),pat,param,_) =
      assert changed;
      match param with
      | None ->
         [Vb.mk ~loc pat
           [%expr Pacomb.Grammar.declare_grammar
               [%e Exp.constant ~loc:name.loc (Const.string name.txt)]]]
      | Some(_,_,_,curry) ->
         let pat = if curry <> None then Pat.var name else pat in
         [Vb.mk ~loc (Pat.tuple [pat; Pat.var (mkloc (set name) name.loc)])
           [%expr Pacomb.Grammar.grammar_family
               [%e Exp.constant ~loc:name.loc (Const.string name.txt)]]]
    in
    let hn (loc,_,(name:string loc),pat,param,_) =
      match param with
      | Some(_,_,_,Some lbls) ->
         let args =
           List.mapi
             (fun i (lbl,def) -> (lbl,def,mknoloc ("x@"^string_of_int i)))
             lbls
         in
         let tuple =
           Exp.tuple (
               List.map (fun (_,_,v) -> Exp.ident (mknoloc (Lident v.txt))) args
             )
         in
         let exp =
           [%expr
               [%e Exp.ident (mkloc (Lident name.txt) name.loc)]
               [%e tuple]]
         in
         let exp =
           List.fold_right (fun (lbl,def,v) exp ->
               let pat = Pat.var v in
               Exp.fun_ lbl def pat exp) args exp
         in
         [Vb.mk  ~loc pat exp]
      | _ -> []
    in
    List.map gn gr @ List.map hn gr
  in
  let orig =
    let gn (loc,_,_,pat,_,rules) =
        Vb.mk ~loc pat rules
    in
    List.map gn orig
  in
  let definitions =
    let fn (loc,changed,name,_,param, rules) =
      assert changed;
      let exp =
        match param with
        | None ->
           [%expr Pacomb.Grammar.set_grammar
             [%e Exp.ident (mkloc (Lident name.txt) name.loc)]
             [%e rules]]
        | Some (_,pn,pat,_) ->
           let pat = Pat.alias pat (mknoloc pn) in
           [%expr
             [%e Exp.ident (mkloc (Lident (set name)) name.loc)]
             (fun [%p pat] -> [%e rules])]
      in
      [Vb.mk ~loc (Pat.any ()) exp]
    in
    List.map fn gr
  in
  (declarations, orig, definitions)

(* transform a list of structure item to parser definition *)
let str_to_parser items =
  let fn item =
    try match item.pstr_desc with
      | Pstr_value(rec_,ls) ->
         let declarations, orig, definitions = vb_to_parser rec_ ls in
         let fn ls =
           List.fold_right
             (fun x a -> if x = [] then a else Str.value Nonrecursive x :: a)
             ls []
         in
           fn declarations
         @ (if orig = [] then [] else [Str.value rec_ orig])
         @ fn definitions
    | _              -> [item]
    with Warn w ->
      (* NOTE: there is no place for attribute in structure_item:
         add an include for that! *)
      [Str.include_ { pincl_mod = Mod.structure items
                   ; pincl_loc = Location.none
                   ; pincl_attributes = [w] }]
       | Exit -> items
  in
  flatten_str (List.flatten (List.map fn items))

let exp_to_parser e =
  try match e.pexp_desc with
  | Pexp_let(rec_,vb,e0) ->
     let declarations, orig, definitions = vb_to_parser rec_ vb in
     let fn ls e0 =
       List.fold_right
         (fun vb e -> if vb = [] then e else Exp.let_ Nonrecursive vb e)
         ls e0
     in
     let defs = (fn definitions e0) in
     fn declarations (if orig = [] then defs else Exp.let_ rec_ orig defs)
  | _ -> snd (exp_to_grammar e)
  with Exit     -> e
     | Warn att -> add_attribute e att


open Ppxlib
module Ast = Ast_builder.Default

let expand_expression expr =
  exp_to_parser expr

let map_all = object
    inherit Ast_traverse.map as super

    method! expression e =
      super#expression (exp_to_parser e)

    method! structure_item =
      (fun i -> super#structure_item (str_to_parser [i]))
  end

let rule_expr =
  let ctx = Extension.Context.expression in
  let pat = Ast_pattern.(single_expr_payload __) in
  let ext = Extension.declare "parser" ctx pat
              (fun ~loc:_ ~path:_ -> map_all#expression) in
  Context_free.Rule.extension ext

let rule_str_item =
  let ctx = Extension.Context.structure_item in
  let pat = Ast_pattern.(pstr __) in
  let ext = Extension.declare "parser" ctx pat
              (fun ~loc:_ ~path:_ l ->
                (flatten_str (List.map map_all#structure_item l))) in
  Context_free.Rule.extension ext

let _ =
  let rules = [rule_expr ; rule_str_item] in
  Driver.register_transformation ~rules "ppx_pacomb"
