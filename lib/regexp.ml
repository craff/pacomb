(** Type of a regular expression. *)
type regexp =
  | Chr of char        (** Single character.              *)
  | Set of Charset.t   (** Any character in a charset.    *)
  | Seq of regexp list (** Sequence of regexps.           *)
  | Alt of regexp list (** Alternative between regexps.   *)
  | Opt of regexp      (** Optional regexp.               *)
  | Str of regexp      (** Zero or more times the regexp. *)
  | Pls of regexp      (** One  or more times the regexp. *)
  | Sav of regexp      (** Save the matching string.      *)

(** Short synonym of {!type:regexp}. *)
type t = regexp

(** [pp ff re] outputs the regexp [re] to the formatter [ff]. *)
let rec pp : Format.formatter -> t -> unit = fun ff re ->
  let pp_sep ff _ = Format.pp_print_string ff ";" in
  let pp_list ff = Format.pp_print_list ~pp_sep pp ff in
  match re with
  | Chr(c) ->Format.fprintf ff "Chr(%C)" c
  | Set(s) ->Format.fprintf ff "Set(%a)" Charset.pp s
  | Seq(l) ->Format.fprintf ff "Seq([%a])" pp_list l
  | Alt(l) ->Format.fprintf ff "Alt([%a])" pp_list l
  | Opt(r) ->Format.fprintf ff "Opt(%a)" pp r
  | Str(r) ->Format.fprintf ff "Str(%a)" pp r
  | Pls(r) ->Format.fprintf ff "Pls(%a)" pp r
  | Sav(r) ->Format.fprintf ff "Sav(%a)" pp r

(** [accepts_empty re] tells whether the empty input is valid for [re]. *)
let rec accepts_empty : regexp -> bool = fun re ->
  match re with
  | Chr(_) -> false
  | Set(s) -> Charset.equal s Charset.empty
  | Seq(l) -> List.for_all accepts_empty l
  | Alt(l) -> List.exists accepts_empty l
  | Opt(_) -> true
  | Str(_) -> true
  | Pls(r) -> accepts_empty r
  | Sav(r) -> accepts_empty r

(** [accepted_first_chars re] returns the set of characters that are possible,
    valid first characters for matching [re]. *)
let rec accepted_first_chars : regexp -> Charset.t = fun re ->
  let rec aux_seq l =
    match l with
    | []   -> Charset.empty
    | r::l -> let cs = accepted_first_chars r in
              if accepts_empty r then Charset.union cs (aux_seq l) else cs
  in
  match re with
  | Chr(c) -> Charset.singleton c
  | Set(s) -> s
  | Seq(l) -> aux_seq l
  | Alt(l) -> let fn cs r = Charset.union cs (accepted_first_chars r) in
              List.fold_left fn Charset.empty l
  | Opt(r) -> accepted_first_chars r
  | Str(r) -> accepted_first_chars r
  | Pls(r) -> accepted_first_chars r
  | Sav(r) -> accepted_first_chars r

module Pacomb = struct
  module Lex = Lex
  module Grammar = Grammar
end

let%parser atom_charset first =
    (c1::CHAR) '-' (c2::CHAR) => (if c1 = '-' || (not first && c1 = ']') ||
                                       (first && c1 = '^') ||
                                         c2 = '-' then Lex.give_up ();
                                  Charset.range c1 c2)
  ; (c1::CHAR)                => (if (not first && (c1 = '-' || c1 = ']')) ||
                                       (first && c1 = '^') then Lex.give_up ();
                                  Charset.singleton c1)

let%parser p_charset =
  (cs1::atom_charset true) (cs2:: ~* (atom_charset false)) =>
    List.fold_left Charset.union cs1 cs2

let is_spe c = List.mem c ['\\';'.';'*';'+';'?';'[';']']

let%parser rec atom_regexp =
    '[' (cpl:: ~? '^') (cs::p_charset) ']' =>
      begin
        let cs = if cpl <> None then Charset.complement (Charset.add cs '\255')
                 else cs
        in
        Set cs
      end
  ; (c::CHAR) =>
      begin
        if is_spe c then Lex.give_up () else Chr c
      end
  ; '\\' (c::CHAR) =>
      begin
        if is_spe c then Chr c else Lex.give_up ()
      end
  ; '.' => Set (Charset.del Charset.full '\255')
  ; "\\(" (r::regexp) "\\)" => Sav r
  ; (r::atom_regexp) '?' => Opt r
  ; (r::atom_regexp) '*' => Str r
  ; (r::atom_regexp) '+' => Pls r

and seq_regexp =
  (rs :: ~+ atom_regexp) => Seq rs

and regexp =
  (rs :: ~+ ["\\|"] seq_regexp) => Alt rs

(* Exception raised when a regexp cannot be parsed. *)
exception Regexp_error of Input.buffer * Input.pos

let from_string : string -> regexp = fun s ->
  try Grammar.parse_string regexp Blank.none s
  with Pos.Parse_error(b,s,_) -> raise (Regexp_error(b,s))

open Lex

let from_regexp_grps : ?grps:int list -> regexp -> string list Lex.t =
  fun ?grps r ->
    let n = ref 0 in
    let do_save fn r =
      let n0 = !n in
      incr n;
      let r = fn r in
      match grps with
      | None -> save r (fun s l -> s :: l)
      | Some l ->
         if List.mem n0 l then save r (fun s l -> s :: l)
         else r
    in
    let rec fn = function
      | Chr c -> char c []
      | Set s -> appl (fun _ -> []) (charset s)
      | Alt l -> alts (List.map fn l)
      | Seq l -> seqs (List.map fn l) (@)
      | Opt r -> option [] (fn r)
      | Str r -> star (fn r) (fun () -> []) (@)
      | Pls r -> plus (fn r) (fun () -> []) (@)
      | Sav r -> do_save fn r
    in
    let r = do_save fn r in
    begin
      match grps with
      | None -> ()
      | Some l -> if List.exists (fun g -> g < 0 || g >= !n) l
                  then invalid_arg "from_regexp_grps"
    end;
    r

let from_regexp : regexp -> string Lex.t = fun r ->
  Lex.appl
    (function [s] -> s | _ -> assert false)
    (from_regexp_grps ~grps:[0] r)

(** create a terminal from a regexp. Returns the groups list, last to finish
    to be parsed is first in the result *)
let regexp_grps : ?name:string -> ?grps:int list -> regexp -> string list t =
  fun ?name ?grps r ->
    let r = from_regexp_grps ?grps r in
    { r with n = default r.n name }

let regexp : ?name:string -> regexp -> string t = fun ?name r ->
  let r = from_regexp r in
  { r with n = default r.n name }

let blank_regexp s = Blank.from_terminal (regexp (from_string s))
