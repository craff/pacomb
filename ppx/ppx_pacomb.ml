open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Longident
open Location

(*helper to build expressions *)
let grmod s = Exp.ident (mknoloc (Ldot(Lident "Grammar",s)))
let lxmod s = Exp.ident (mknoloc (Ldot(Lident "Lex",s)))
let rgmod s = Exp.ident (mknoloc (Ldot(Lident "Regexp",s)))

let unit_ = let loc = none in [%expr ()]

let app loc f x = Exp.apply ~loc f [(Nolabel, x)]
let app2 loc f x y = Exp.apply ~loc f [(Nolabel, x);(Nolabel, y)]

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
  let mapper =
    { default_mapper with
      expr = (fun mapper exp ->
        match exp.pexp_desc with
        | Pexp_ident { txt = (Lident id'); _} when id' = id ->
           found := true; exp
        | _ -> default_mapper.expr mapper exp)
    }
  in
  let _ = mapper.expr mapper e in
  !found

(* transform an expression in a pattern
   - "_" does not work. use "__" instead
   - (pat = lid) is the synta for as pattern *)
let rec exp_to_pattern e =
  let loc = e.pexp_loc in
  match e with
  | {pexp_desc = Pexp_ident({txt = Lident name; loc = loc_s}); _} ->
     let name = mkloc name loc_s in
     (Some name, Pat.var ~loc name)
  | [%expr [%e? e] = [%e? {pexp_desc = Pexp_ident({txt = Lident name
                                                  ;loc = loc_s}); _}]]
     ->
     let name = mkloc name loc_s in
     let (_, pat) = exp_to_pattern e in
     (Some name, Pat.alias pat name)
  | [%expr ([%e? e] : [%t? t])] ->
     let (name, pat) = exp_to_pattern e in
     (name, [%pat? ([%p pat] : [%t t])])
  | {pexp_desc = Pexp_tuple(l); _} ->
     let (_,pats) = List.split (List.map exp_to_pattern l) in
     (None, Pat.tuple ~loc pats)
  | [%expr lazy [%e? e]] ->
     let (name, pat) = exp_to_pattern e in
     (name, [%pat? lazy [%p pat]])
(* | [%expr let open [%m? m] in [%e? e]] ->*)
  | { pexp_desc = Pexp_open(_,m,e); _ } ->
     let (name, pat) = exp_to_pattern e in
     (name, Pat.open_ ~loc m  pat)
  | _ -> warn loc "expression eft of \"::\" does not represent a pattern"

(* transform an expression into a terminal *)
let exp_to_term exp =
  let loc = exp.pexp_loc in
  match exp with
  | {pexp_desc = Pexp_constant (Pconst_char _); _} ->
     [%expr [%e grmod "term"] ([%e lxmod "char"] [%e exp] ())]
  | [%expr CHAR([%e? s])] ->
     [%expr [%e grmod "term"] ([%e lxmod "char"] [%e s] ())]
  | {pexp_desc = Pexp_constant (Pconst_string _); _} ->
     [%expr [%e grmod "term"] ([%e lxmod "string"] [%e exp] ())]
  | [%expr STR([%e? s])] ->
     [%expr [%e grmod "term"] ([%e lxmod "string"] [%e s] ())]
  | [%expr INT] ->
     [%expr [%e grmod "term"] ([%e lxmod "int"] ())]
  | [%expr FLOAT] ->
     [%expr [%e grmod "term"] ([%e lxmod "float"] ())]
  | [%expr RE([%e? s])] ->
     [%expr [%e grmod "term"] ([%e lxmod "regexp"]
                                 ([%e rgmod "from_string"] [%e s]))]
  | _ -> exp

(* treat each iterm in a rule. Accept (pat::term) or term when
   - pat is an expression accepted by exp_to_pattern
   - term is an expression accepted by exp_to_term *)
let exp_to_rule_item (e, loc_e) =  match e.pexp_desc with
  | Pexp_construct
    ( { txt = Lident "::"; _}
    , Some({pexp_desc =
              Pexp_tuple
                [ epat ; exp ]; _})) ->
     let (name, pat) = exp_to_pattern epat in
     (Some (name, pat), None, exp_to_term exp, loc_e)
  | Pexp_apply({pexp_desc = Pexp_ident { txt = Lident ">:"; _}; _},
              [(Nolabel,{pexp_desc = Pexp_tuple([dpat;epat]); _})
              ;(Nolabel, exp) ]) ->
     let cs =
       try
         let (_,s) = List.find (fun (s,_) -> s.txt = "cs")
                       exp.pexp_attributes
         in
         match s with
         | PStr [{pstr_desc = Pstr_eval (e,_); _}] ->
           Printf.eprintf "cs\n%!"; Some e
         | _ -> raise Not_found
       with Not_found -> None
     in
     let (name, pat) = exp_to_pattern epat in
     let (_, dpat) = exp_to_pattern dpat in
     (Some (name, pat), Some (dpat,cs), exp_to_term exp, loc_e)
  | _ ->
     (None, None, exp_to_term e, loc_e)

(* transform exp into rule, that is list of rule item. Accept
   - a sequence of items (that is applications of items)
   - or () to denote the empty rule *)
let rec exp_to_rule e =
  let loc_e = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_apply({ pexp_desc =
      Pexp_apply({ pexp_desc =
        Pexp_ident
          { txt = Lident("="|"<"|">"|"<="|">="|"<>"|"=="|"!="|
                         "not"|"&&"|"||"); _ }; _}, _); _} as cond,
      (Nolabel,a3)::rest) ->
     let (rule,_) = exp_to_rule (Exp.apply a3 rest) in
     (rule, Some cond)
  | Pexp_apply(e1, args) ->
     let kn (_,e') = (e',merge_loc e'.pexp_loc loc_e) in
     let l = (e1, e.pexp_loc) :: List.map kn args in
     (List.map exp_to_rule_item l, None)
  | Pexp_construct({txt = Lident "()"; loc}, None) ->
     ([None, None, app loc (grmod "empty") unit_, loc_e], None)
  | _ ->
     ([exp_to_rule_item (e, e.pexp_loc)], None)

(* transform an expression into a list of rules with action
   - name_param is an optional arguments for an eventual parameter name
   - fn is a function to modify the action. It adds [Exp.fun _] conctructs *)
let rec exp_to_rules ?name_param ?(acts_fn=(fun exp -> exp)) e =
  match e.pexp_desc with
  (* base case [items => action] *)
  | Pexp_apply
    ( { pexp_desc = Pexp_ident {txt = Lident "=>"; _}; _ }
    , [(Nolabel,rule);(Nolabel,action)]) ->
     let (rule,cond) = exp_to_rule rule in
     let loc_a = action.pexp_loc in
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
          app loc (grmod "empty") (acts_fn action)
       | Warn att ->
          let loc = action.pexp_loc in
          let exp = app loc (grmod "empty") (acts_fn action) in
          add_attribute exp att

     in
     let fn (lpos,rpos,dep,item,loc_e) exp =
       let app_cs f cs =
         let args = match cs with
           | None -> []
           | Some e -> [Labelled "cs", e]
         in
         if args = [] then grmod f
         else Exp.apply (grmod f) args
       in
       let f = match (lpos,rpos,dep) with
         | false, false, None -> grmod "seq"
         | true , false, None -> grmod "seq_lpos"
         | false, true , None -> grmod "seq_rpos"
         | true , true , None -> grmod "seq_pos"
         | false, false, Some(_,cs) -> app_cs "dseq"      cs
         | true , false, Some(_,cs) -> app_cs "dseq_lpos" cs
         | false, true , Some(_,cs) -> app_cs "dseq_rpos" cs
         | true , true , Some(_,cs) -> app_cs "dseq_pos"  cs
       in
       let exp = match dep with
         | None -> exp
         | Some (pat,_) -> Exp.fun_ Nolabel None pat exp
       in
       app2 (merge_loc loc_e exp.pexp_loc) f item exp
     in
     let rule = List.fold_right fn rule action in
     let rule =
       match cond with
       | None -> rule
       | Some cond -> Exp.ifthenelse cond rule
                        (Some (app none (grmod "fail") unit_))
     in
     [rule]
  (* inheritance case [prio1 < prio2 < ... < prion] *)
  | Pexp_apply({ pexp_desc = Pexp_ident {txt = Lident "<"; _}; _ }
               , [_;_]) when name_param <> None ->
     let rec fn exp = match exp.pexp_desc with
       | Pexp_apply
         ( { pexp_desc = Pexp_ident {txt = Lident "<"; _}; _ }
         , [(Nolabel, x);(Nolabel, y)]) ->
          y :: fn x
       | _ -> [exp]
     in
     let prios = fn e in
     let (name,param,_) =
       match name_param with None -> assert false
                           | Some x -> x
     in
     let param = mknoloc (Lident param) in
     let loc = e.pexp_loc in
     let rec gn acc l =
       match l with
       | x::(y::_ as l) ->
          let e =
            app2 loc (grmod "seq2")
              (app loc (grmod "test")
                 (app2 loc (Exp.ident (mknoloc (Lident "=")))
                    (Exp.ident param) x))
              (app loc (Exp.ident name) y)
          in gn (e::acc) l
       | [] | [_] -> acc
     in
     gn [] prios
  (* alternatives represented as sequence *)
  | Pexp_sequence(e1,e2) ->
     exp_to_rules ?name_param ~acts_fn e1
     @ exp_to_rules ?name_param ~acts_fn e2
  (* not a grammar at all (no warning)! *)
  | _ -> raise Exit

(* transform an expression into grammar, by adding [alt] combinators
   to the result of exp_to_rules *)
and exp_to_grammar ?name_param ?(acts_fn=(fun exp -> exp)) exp =
  let rules = exp_to_rules ?name_param ~acts_fn exp in
  match rules with (* three cases for better location ? *)
  | [] -> app none (grmod "fail") unit_
  | [x] -> x
  | _ ->
     let rec fn = function
       | [] -> Exp.construct (mknoloc (Lident "[]")) None
       | x::l ->
          let exp = fn l in
          let loc = merge_loc x.pexp_loc exp.pexp_loc in
          Exp.construct ~loc (mknoloc (Lident "::"))
            (Some(Exp.tuple [x;exp]))
     in
     let exp = fn rules in
     app exp.pexp_loc (grmod "alt") exp

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
    let (param,exp) = match vb.pvb_expr.pexp_desc with
      | Pexp_fun (Nolabel, None, param, exp) when rec_ = Recursive ->
         (Some param, exp)
      | _ -> (None, vb.pvb_expr)
    in
    let name_param = match param with
      | None -> None
      | Some p -> Some ( mkloc (Lident name.txt) name.loc
                       , "blabla"  (* FIXME *)
                       , p)
    in
    let (changed,rules) = exp_to_grammar ?name_param exp in
    if changed && do_warn then
      warn vb.pvb_pat.ppat_loc
        "Pattern not allowed here for grammar parameter";
    let rules =
      if List.exists (fun (s,_) -> s.txt = "cached") vb.pvb_attributes then
        app loc (grmod "cache") rules
      else rules
    in
    (loc,changed,name,vb.pvb_pat,name_param,rules)
  in
  let ls = List.map gn vb in
  let (gr,orig) = List.partition
                    (fun (_,changed,_,_,_,_) -> changed && rec_ = Recursive)
                    ls
  in
  let set name = "set__grammar__" ^ name.txt in
  let declarations =
    let gn (loc,changed,(name:string loc),pat,param,_) =
      assert changed;
      let vd =
        match param with
        | None ->
           Vb.mk ~loc pat
             (app loc (grmod "declare_grammar")
                (Exp.constant ~loc:name.loc (Const.string name.txt)))
        | Some _ ->
           Vb.mk ~loc (Pat.tuple [pat; Pat.var (mkloc (set name) name.loc)])
             (app loc (grmod "grammar_family")
                (Exp.constant ~loc:name.loc (Const.string name.txt)))
      in
      [vd]
    in
    List.map gn gr
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
           app2 loc (grmod "set_grammar")
             (Exp.ident (Location.mkloc (Lident name.txt) name.loc))
             rules
        | Some (_,pn,pat) ->
           app loc
             (Exp.ident (Location.mkloc (Lident (set name)) name.loc))
             (Exp.fun_ ~loc Nolabel None (Pat.alias pat (mknoloc pn)) rules)
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
