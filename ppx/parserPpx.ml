open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Longident
open Location

let grmod s = Exp.ident (Location.mknoloc (Ldot(Lident "Grammar",s)))
let lxmod s = Exp.ident (Location.mknoloc (Ldot(Lident "Lex",s)))

let unit_ = Exp.construct (Location.mknoloc (Lident "()")) None

let app f x = Exp.apply f [(Nolabel, x)]
let app2 f x y = Exp.apply f [(Nolabel, x);(Nolabel, y)]

let str_to_parser str =
  let fn item = match item.pstr_desc with
    | Pstr_value(_,ls) ->
       let gn vb =
         let name = match vb.pvb_pat.ppat_desc with
           | Ppat_var s -> s
           | _          -> exit 1
         in
         let lexify exp =
           match exp.pexp_desc with
           | Pexp_constant (Pconst_char _) ->
              app (grmod "term") (app2 (lxmod "char") exp unit_)
           | Pexp_constant (Pconst_string _) ->
              app (grmod "term") (app2 (lxmod "string") exp unit_)
           | Pexp_construct({txt = Lident "INT"; _}, None) ->
              app (grmod "term") (app (lxmod "int") unit_)
           | Pexp_construct({txt = Lident "FLOAT"; _}, None) ->
              app (grmod "term") (app (lxmod "float") unit_)
           | _ -> exp
         in
         let item e =  match e.pexp_desc with
           | Pexp_construct
               ( { txt = Lident "::"; _}
               , Some({pexp_desc =
                         Pexp_tuple
                           [ {pexp_desc = Pexp_ident {txt = Lident name; loc}; _}
                           ; exp ]; _})) ->
              (Pat.var (mkloc name loc),lexify exp)
           | _ ->
              (Pat.any (), lexify e)
         in
         let items e = match e.pexp_desc with
           | Pexp_apply(e1, args) ->
              let l = e1 :: List.map snd args in
              List.map item l
           | _ -> [item e]
         in
         let rec rules e = match e.pexp_desc with
           | Pexp_apply
             ( { pexp_desc = Pexp_ident {txt = Lident "=>"; _}; _ }
             , [(Nolabel,rule);(Nolabel,action)]) ->
              let rule = items rule in
              let args, rule = List.split rule in
              let fn exp name = Exp.fun_ Nolabel None name exp in
              let action = List.fold_left fn action args in
              let action = app  (grmod "empty") action in
              let fn item exp = app2 (grmod "seqf") item exp in
              let rule = List.fold_right fn rule action in
              [rule]
           | Pexp_sequence(e1,e2) ->
              rules e1 @ rules e2
           | _ -> exit 1
         in
         let rules = rules vb.pvb_expr in
         (name,rules)
       in
       let rules = List.map gn ls in
       let fail = app (grmod "fail") unit_ in
       let fn rule exp = app2 (grmod "alt") rule exp in
       List.map
         (fun (name, rules) -> (name, List.fold_right fn rules fail))
         rules
    | _              -> exit 1
  in
  let l = List.flatten (List.map fn str) in
  let declarations =
    let gn (name, _) =
      Str.value Nonrecursive
        [Vb.mk (Pat.var name)
           (app (grmod "declare_grammar") (Exp.constant (Const.string name.txt)))]
    in
    List.map gn l
  in
  let definitions =
    let fn (name, rules) =
      Str.value Nonrecursive
        [Vb.mk (Pat.any ())
           (app2 (grmod "set_grammar")
              (Exp.ident (Location.mkloc (Lident name.txt) name.loc))
              rules)]
    in
    List.map fn l
  in
  Str.include_ { pincl_mod = Mod.structure (declarations @ definitions)
               ; pincl_loc = Location.none
               ; pincl_attributes = [] }

let test_mapper _argv =
  { default_mapper with
    structure_item = fun mapper item ->
      match item with
      | { pstr_desc = Pstr_extension (({ txt = "parser"; _ }, PStr str), _); _} ->
         str_to_parser str
      | other -> default_mapper.structure_item mapper other; }

let () =
  register "ppx_test" test_mapper
