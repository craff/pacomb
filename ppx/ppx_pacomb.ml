open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Longident
open Location

let grmod s = Exp.ident (mknoloc (Ldot(Lident "Grammar",s)))
let lxmod s = Exp.ident (mknoloc (Ldot(Lident "Lex",s)))
let rgmod s = Exp.ident (mknoloc (Ldot(Lident "Regexp",s)))

let unit_ = Exp.construct (Location.mknoloc (Lident "()")) None

let app loc f x = Exp.apply ~loc f [(Nolabel, x)]
let app2 loc f x y = Exp.apply ~loc f [(Nolabel, x);(Nolabel, y)]

let merge_loc loc1 loc2 = { loc1 with loc_end = loc2.loc_end }

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

let rec exp_to_grammar ?name_param ?(fn=(fun exp -> exp)) exp =
  let lexify exp =
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_constant (Pconst_char _) ->
       app loc (grmod "term") (app2 loc (lxmod "char") exp unit_)
    | Pexp_constant (Pconst_string _) ->
       app loc (grmod "term") (app2 loc (lxmod "string") exp unit_)
    | Pexp_construct({txt = Lident "INT"; _}, None) ->
       app loc (grmod "term") (app loc (lxmod "int") unit_)
    | Pexp_construct({txt = Lident "FLOAT"; _}, None) ->
       app loc (grmod "term") (app loc (lxmod "float") unit_)
    | Pexp_construct({txt = Lident "RE"; _}, Some s) ->
       app loc (grmod "term") (app loc (lxmod "regexp")
                                  (app loc (rgmod "from_string") s))
    | Pexp_apply({ pexp_desc = Pexp_ident { txt = Lident("="|"<"|">"|"<="|">="); _ }; _}, _) ->
       app loc (grmod "test") exp
    | _ -> exp
  in
  let item (e, loc_e) =  match e.pexp_desc with
    | Pexp_construct
      ( { txt = Lident "::"; _}
      , Some({pexp_desc =
                Pexp_tuple
                  [ {pexp_desc = Pexp_ident {txt = Lident name; loc}; _}
                  ; exp ]; _})) ->
       (Some (mkloc name loc),lexify exp, loc_e)
    | _ ->
       (None, lexify e, loc_e)
  in
  let items e =
    let loc_e = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_apply(e1, args) ->
       let kn (_,e') = (e',merge_loc e'.pexp_loc loc_e) in
       let l = (e1, e.pexp_loc) :: List.map kn args in
       List.map item l
    | Pexp_construct({txt = Lident "()"; loc}, None) ->
       [None, app loc (grmod "empty") unit_, loc_e]
    | _ -> [item (e, e.pexp_loc)]
  in
  let rec rules e =
    match e.pexp_desc with
    | Pexp_apply
      ( { pexp_desc = Pexp_ident {txt = Lident "=>"; _}; _ }
      , [(Nolabel,rule);(Nolabel,action)]) ->
       let rule = items rule in
       let loc_a = action.pexp_loc in
       let gn (fn, rule) (name, item, loc_e) = match name with
         | None    -> (fn, (false, false, false, item, loc_e) :: rule)
         | Some id ->
            let id_rpos = mkloc (id.txt ^ "_rpos") id.loc in
            let (fn,rpos) =
              if has_ident id_rpos.txt action then
                ((fun exp -> Exp.fun_ ~loc:loc_a Nolabel None
                               (Pat.var id_rpos) (fn exp)), true)
              else (fn, false)
            in
            let (fn,has_id) =
              if has_ident id.txt action then
                ((fun exp -> Exp.fun_ ~loc:loc_a Nolabel None
                               (Pat.var id) (fn exp)), true)
              else
                (fn, false)
            in
            let id_lpos = mkloc (id.txt ^ "_lpos") id.loc in
            let (fn,lpos) =
              if has_ident id_lpos.txt action then
                ((fun exp -> Exp.fun_ ~loc:loc_a Nolabel None
                               (Pat.var id_lpos) (fn exp)), true)
              else
                (fn, false)
            in
            (fn, (lpos,has_id,rpos,item,loc_e) :: rule)
       in
       let (fn, rule) = List.fold_left gn (fn, []) rule in
       let rule = List.rev rule in
       let action =
         try exp_to_grammar ~fn action
         with Exit ->
           let loc = action.pexp_loc in
           app loc (grmod "empty") (fn action)
       in
       let fn (lpos,has_id,rpos,item,loc_e) exp =
         let f = match (lpos,has_id,rpos) with
           | false, false, false -> "seq2"
           | false, true , false -> "seqf"
           | true , false, false -> "seq2_lpos"
           | true , true , false -> "seqf_lpos"
           | false, false, true  -> "seq2_rpos"
           | false, true , true  -> "seqf_rpos"
           | true , false, true  -> "seq2_pos"
           | true , true , true  -> "seqf_pos"
         in
         app2 (merge_loc loc_e exp.pexp_loc) (grmod f) item exp
       in
       let rule = List.fold_right fn rule action in
       [rule]
    | Pexp_apply
      ( { pexp_desc = Pexp_ident {txt = Lident "<"; _}; _ }
      , [_;_]) when name_param <> None ->
       let rec fn exp = match exp.pexp_desc with
         | Pexp_apply
           ( { pexp_desc = Pexp_ident {txt = Lident "<"; _}; _ }
           , [(Nolabel, x);(Nolabel, y)]) ->
            y :: fn x
         | _ -> [exp]
       in
       let prios = fn e in
       let (name,param) =
         match name_param with None -> assert false
                       | Some x -> x
       in
       let loc = e.pexp_loc in
       let rec gn acc l =
         match l with
         | x::(y::_ as l) ->
            let e =
              app2 loc (grmod "seq2")
                (app loc (grmod "test") (app2 loc (Exp.ident (mknoloc (Lident "=")))
                                     (Exp.ident param) x))
                (app loc (Exp.ident name) y)
            in gn (e::acc) l
         | [] | [_] -> acc
       in
       gn [] prios
    | Pexp_sequence(e1,e2) ->
       rules e1 @ rules e2
    | _ -> raise Exit
  in
  let fail = app exp.pexp_loc (grmod "fail") unit_ in
  let fn exp rule =
    let loc = merge_loc exp.pexp_loc rule.pexp_loc in
    app2 loc (grmod "alt") exp rule in
  List.fold_left fn fail (rules exp)

let exp_to_grammar ?name_param exp =
  try (true, exp_to_grammar ?name_param exp)
  with Exit -> (false, exp)

let str_to_grammar str =
  match str with
  | [{pstr_desc = Pstr_eval(e,_); _}] ->
     snd (exp_to_grammar e)
  | _  -> Exp.extension
            (extension_of_error
               (Location.error ~loc:(List.hd str).pstr_loc "shoud be an expression"))

exception Warning of attribute

let warn loc msg = raise (Warning (attribute_of_warning loc msg))

let str_to_parser items =
  let fn item =
    let is_grammar = ref false in
    try match item.pstr_desc with
    | Pstr_value(rec_,ls) ->
       let gn vb =
         let loc = vb.pvb_loc in
         let (name,do_warn) = match vb.pvb_pat.ppat_desc with
           | Ppat_var s -> (s         , false)
           | _          -> (mknoloc "", true )
         in
         let (param,exp) = match vb.pvb_expr.pexp_desc with
           | Pexp_fun (Nolabel, None, { ppat_desc = Ppat_var param; _ }, exp) ->
             (Some param, exp)
           | _ -> (None, vb.pvb_expr)
         in
         let name_param = match param with
           | None -> None
           | Some p -> Some ( mkloc (Lident name.txt) name.loc
                            , mkloc (Lident p.txt) p.loc)
         in
         let (changed,rules) = exp_to_grammar ?name_param exp in
         if changed && do_warn then
           warn vb.pvb_pat.ppat_loc
             "Pattern not allowed here for grammars";
         if changed then is_grammar := true;
         let rules =
           if List.exists (fun (s,_) -> s.txt = "cached") vb.pvb_attributes then
             app loc (grmod "cache") rules
           else rules
         in
         (loc,name,param,rules)
       in
       let ls = List.map gn ls in
       if not !is_grammar then [item] else
       begin match rec_ with
       | Nonrecursive ->
         let definitions =
           let gn (loc,name, param, rules) =
             match param with
               None -> Str.value Nonrecursive [Vb.mk ~loc (Pat.var name) rules]
             | Some name ->
                Str.value Nonrecursive
                  [Vb.mk ~loc (Pat.var name)
                     (Exp.fun_ Nolabel None (Pat.var name) rules)]
           in
           List.map gn ls
         in
         definitions
       | Recursive ->
          let set name = "set__grammar__" ^ name.txt in
          let declarations =
            let gn (loc,name, param, _) =
              let vd =
                match param with
                | None ->
                   Vb.mk ~loc (Pat.var name)
                     (app loc (grmod "declare_grammar")
                        (Exp.constant ~loc:name.loc (Const.string name.txt)))
                | Some _ ->
                   Vb.mk ~loc (Pat.tuple [Pat.var name; Pat.var (mkloc (set name) name.loc)])
                     (app loc (grmod "grammar_family")
                        (Exp.constant ~loc:name.loc (Const.string name.txt)))
              in
              Str.value Nonrecursive [vd]
            in
            List.map gn ls
         in
         let definitions =
           let fn (loc,name, param, rules) =
             let exp =
               match param with
               | None ->
                  app2 loc (grmod "set_grammar")
                    (Exp.ident (Location.mkloc (Lident name.txt) name.loc))
                    rules
               | Some n ->
                  app loc
                    (Exp.ident (Location.mkloc (Lident (set name)) name.loc))
                    (Exp.fun_ ~loc Nolabel None (Pat.var n) rules)
             in
             Str.value Nonrecursive [Vb.mk ~loc (Pat.any ()) exp]
           in
           List.map fn ls
         in
         declarations @ definitions
       end
    | _              -> [item]
    with Warning w ->
      (* NOTE: there is no place for attribute in structure_item:
         add an include for that! *)
      [Str.include_ { pincl_mod = Mod.structure items
                   ; pincl_loc = Location.none
                   ; pincl_attributes = [w] }]
  in
  let items = List.flatten (List.map fn items) in
  match items with
  | [x] -> x
  | _ ->
     Str.include_ { pincl_mod = Mod.structure items
                  ; pincl_loc = Location.none
                  ; pincl_attributes = [] }


let test_mapper _argv =
  let map_all_expr =
    { default_mapper with
      expr = (fun mapper exp -> default_mapper.expr mapper
                                  (snd (exp_to_grammar exp)))
    ; structure_item =  (fun mapper s -> str_to_parser
                                  [default_mapper.structure_item mapper s])
    }
  in
  { default_mapper with
    structure_item = (fun mapper item ->
      match item with
      | { pstr_desc = Pstr_extension (({ txt = "parser"; _ }, PStr str), _); _} ->
         default_mapper.structure_item map_all_expr (str_to_parser str)
      | other -> default_mapper.structure_item mapper other)
  ; expr = (fun mapper exp ->
      match exp with
      | { pexp_desc = Pexp_extension ({ txt = "grammar"; _ }, PStr str); _} ->
         str_to_grammar str
      | other -> default_mapper.expr mapper other)

  }

let () =
  register "ppx_test" test_mapper
