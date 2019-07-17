(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(* Type of a regular expression. *)
type regexp =
  | Chr of char                (* Single character.              *)
  | Set of Charset.t           (* Any character in a charset.    *)
  | Seq of regexp list         (* Sequence of regexps.           *)
  | Alt of regexp list         (* Alternative between regexps.   *)
  | Opt of regexp              (* Optional regexp.               *)
  | Str of regexp              (* Zero or more times the regexp. *)
  | Pls of regexp              (* One  or more times the regexp. *)
  | Sav of regexp              (* Save what is read.             *)

type t = regexp

let print ch re =
  let rec pregexp ch = function
    | Chr(c) -> Printf.fprintf ch "Chr(%C)" c
    | Set(s) -> Printf.fprintf ch "Set(%a)" Charset.print s
    | Seq(l) -> Printf.fprintf ch "Seq([%a])" pregexps l
    | Alt(l) -> Printf.fprintf ch "Alt([%a])" pregexps l
    | Opt(r) -> Printf.fprintf ch "Opt(%a)" pregexp r
    | Str(r) -> Printf.fprintf ch "Str(%a)" pregexp r
    | Pls(r) -> Printf.fprintf ch "Pls(%a)" pregexp r
    | Sav(r) -> Printf.fprintf ch "Sav(%a,<ref>)" pregexp r
  and pregexps ch = function
    | []    -> ()
    | [r]   -> pregexp ch r
    | r::rs -> Printf.fprintf ch "%a;%a" pregexp r pregexps rs
  in
  pregexp ch re

let rec accept_empty = function
  | Chr(_) -> false
  | Set(_) -> false
  | Seq(l) -> List.for_all accept_empty l
  | Alt(l) -> List.exists accept_empty l
  | Opt(_) -> true
  | Str(_) -> true
  | Pls(r) -> accept_empty r
  | Sav(r) -> accept_empty r

let accepted_first_chars : regexp -> Charset.t =
  let open Charset in
  let rec aux = function
    | Chr(c) -> singleton c
    | Set(s) -> s
    | Seq(l) -> begin
                  match l with
                  | []    -> empty
                  | r::rs -> if accept_empty r then
                               union (aux r) (aux (Seq(rs)))
                             else aux r
                end
    | Alt(l) -> List.fold_left (fun cs r -> union cs (aux r)) empty l
    | Opt(r) -> aux r
    | Str(r) -> aux r
    | Pls(r) -> aux r
    | Sav(r) -> aux r
  in aux

type construction =
    Acc of regexp list
  | Par of regexp list * regexp list

let push x = function
  | Acc l -> Acc (x::l)
  | Par (l1, l2) -> Acc(Alt(x :: l1)::l2)

let pop = function
  | Acc l -> l
  | Par _ -> invalid_arg "Regexp: final bar."

let from_string : string -> regexp = fun s ->
  let cs =
    let cs = ref [] in
    for i = String.length s - 1 downto 0 do
      cs := s.[i] :: !cs
    done; !cs
  in

  let read_range cs =
    let rec read_range acc = function
      | []              -> invalid_arg "Regexp: open charset."
      | ']'::cs         -> (acc, cs)
      | c1::'-'::c2::cs -> let r = Charset.range c1 c2 in
                           read_range (Charset.union acc r) cs
      | c::cs           -> read_range (Charset.add acc c) cs
    in read_range Charset.empty cs
  in
  let rec tokens cs =
    let is_spe c = List.mem c ['\\';'.';'*';'+';'?';'[';']'] in
    match cs with
    | '.' ::cs            -> `Set(Charset.full) :: tokens cs
    | '*' ::cs            -> `Str :: tokens cs
    | '+' ::cs            -> `Pls :: tokens cs
    | '?' ::cs            -> `Opt :: tokens cs
    | '\\'::'('::cs       -> `Opn :: tokens cs
    | '\\'::')'::cs       -> `Cls :: tokens cs
    | '\\'::'|'::cs       -> `Alt :: tokens cs
    | '\\'::c  ::cs       -> if is_spe c then `Chr(c) :: tokens cs
                             else invalid_arg "Regexp: invalid escape."
    | '\\'::[]            -> invalid_arg "Regexp: nothing to escape."
    | '[' ::'^':: ']'::cs -> let (rng, cs) = read_range cs in
                             let rng = Charset.add rng ']' in
                             `Set(Charset.complement rng) :: tokens cs
    | '[' ::']':: cs      -> let (rng, cs) = read_range cs in
                             `Set(Charset.add rng ']') :: tokens cs
    | '[' ::'^'::cs       -> let (rng, cs) = read_range cs in
                             `Set(Charset.complement rng) :: tokens cs
    | '[' ::cs            -> let (rng, cs) = read_range cs in
                             `Set(rng) :: tokens cs
    | c   ::cs            -> `Chr(c) :: tokens cs
    | []                  -> []
  in
  let ts = tokens cs in

  let rec build_re stk acc ts =
    match (stk, acc, ts) with
    | (stk   , acc    , `Chr(c)::ts) -> build_re stk (push (Chr c) acc) ts
    | (stk   , acc    , `Set(s)::ts) -> build_re stk (push (Set s) acc) ts
    | (stk   , Acc(Alt (re::l)::acc), `Str   ::ts) -> build_re stk (Acc(Alt(Str re::l) :: acc)) ts
    | (stk   , Acc(Alt (re::l)::acc), `Pls   ::ts) -> build_re stk (Acc(Alt(Pls re::l) :: acc)) ts
    | (stk   , Acc(Alt (re::l)::acc), `Opt   ::ts) -> build_re stk (Acc(Alt(Opt re::l) :: acc)) ts
    | (stk   , Acc(re::acc), `Str   ::ts) -> build_re stk (Acc(Str re :: acc)) ts
    | (stk   , Acc(re::acc), `Pls   ::ts) -> build_re stk (Acc(Pls re :: acc)) ts
    | (stk   , Acc(re::acc), `Opt   ::ts) -> build_re stk (Acc(Opt re :: acc)) ts
    | (_     , _     , `Str   ::_ )
    | (_     , _     , `Pls   ::_ )
    | (_     , _     , `Opt   ::_ ) ->
        invalid_arg "Regexp: modifier error."
    | (stk   , acc    , `Opn   ::ts) -> build_re (pop acc::stk) (Acc []) ts
    | ([]    , _      , `Cls   ::_ ) ->
        invalid_arg "Regexp: group not opened."
    | (s::stk, acc    , `Cls   ::ts) ->
        let re =
          match List.rev (pop acc) with
          | [re] -> re
          | l    -> Seq(l)
        in
        build_re stk (Acc(Sav(re)::s)) ts
    | (stk   , Acc(re::acc), `Alt   ::ts) -> build_re stk (Par([re],acc)) ts
    | (_     , Acc []      , `Alt   ::_ ) ->
        invalid_arg "Regexp: initial bar."
    | (_     , Par _       , `Alt   ::_ ) ->
        invalid_arg "Regexp: consecutive bar."
    | ([]    , acc         , []         ) ->
        begin
          match List.rev (pop acc) with
          | [re] -> re
          | l    -> Seq(l)
        end
    | (_     , _      , []         ) -> invalid_arg "Regexp: group error."
  in
  build_re [] (Acc []) ts

(* Exception raised when a regexp cannot be parsed. *)
exception Regexp_error of Input.buffer * int

let regexp_error : type a. Input.buffer -> int -> a = fun buf pos ->
  raise (Regexp_error(buf, pos))

let string_of_char_list : char list -> string = fun cs ->
  let b = Buffer.create 10 in
  List.iter (Buffer.add_char b) cs;
  Buffer.contents b

(* Input characters according to the given regexp. *)
let read : regexp -> Input.buffer -> int -> string list * Input.buffer * int =
  fun re buf pos ->
    let grps = ref [] in
    let rec sread_regexp re buf pos cs =
      match re with
      | Chr(ch)    ->
          let (c, buf, pos) = Input.read buf pos in
          if c = ch then (c::cs, buf, pos)
          else regexp_error buf pos
      | Set(chs)   ->
          let (c, buf, pos) = Input.read buf pos in
          if Charset.mem chs c then (c::cs, buf, pos)
          else regexp_error buf pos
      | Seq(r::rs) ->
          let (cs, buf, pos) = sread_regexp r buf pos cs in
          sread_regexp (Seq(rs)) buf pos cs
      | Seq([])    -> (cs, buf, pos)
      | Alt(r::rs) ->
          begin
            try sread_regexp r buf pos cs
            with Regexp_error(_,_) -> sread_regexp (Alt(rs)) buf pos cs
          end
      | Alt([])    -> regexp_error buf pos
      | Opt(r)     ->
          begin
            try sread_regexp r buf pos cs
            with Regexp_error(_,_) -> (cs, buf, pos)
          end
      | Str(r)     ->
          begin
            try
              let (cs, buf, pos) = sread_regexp r buf pos cs in
              sread_regexp re buf pos cs
            with Regexp_error(_,_) -> (cs, buf, pos)
          end
      | Pls(r)     ->
          let (cs, buf, pos) = sread_regexp r buf pos cs in
          sread_regexp (Str(r)) buf pos cs
      | Sav(r) ->
          let cs0 = cs in
          let rec fn acc = function
            | cs when cs == cs0 -> string_of_char_list acc
            | c::cs -> fn (c::acc) cs
            | [] -> assert false
          in
          let (cs, _, _ as res) = sread_regexp r buf pos cs in
          grps := fn [] cs :: !grps; res
    in
    let rec read_regexp re buf pos =
      match re with
      | Chr(ch)    ->
          let (c, buf, pos) = Input.read buf pos in
          if c = ch then (buf, pos)
          else regexp_error buf pos
      | Set(chs)   ->
          let (c, buf, pos) = Input.read buf pos in
          if Charset.mem chs c then (buf, pos)
          else regexp_error buf pos
      | Seq(r::rs) ->
          let (buf, pos) = read_regexp r buf pos in
          read_regexp (Seq(rs)) buf pos
      | Seq([])    -> (buf, pos)
      | Alt(r::rs) ->
          begin
            try read_regexp r buf pos
            with Regexp_error(_,_) -> read_regexp (Alt(rs)) buf pos
          end
      | Alt([])    -> regexp_error buf pos
      | Opt(r)     ->
          begin
            try read_regexp r buf pos
            with Regexp_error(_,_) -> (buf, pos)
          end
      | Str(r)     ->
          begin
            try
              let (buf, pos) = read_regexp r buf pos in
              read_regexp re buf pos
            with Regexp_error(_,_) -> (buf, pos)
          end
      | Pls(r)     ->
          let (buf, pos) = read_regexp r buf pos in
          read_regexp (Str(r)) buf pos
      | Sav(r) ->
         let (cs, buf, pos) = sread_regexp r buf pos [] in
         grps := string_of_char_list (List.rev cs) :: !grps;
         (buf, pos)
    in
    let (b,n) = read_regexp re buf pos in
    (!grps,b,n)
