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

type construction =
  | Acc of regexp list
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
                             let rng = Charset.add rng '\255' in
                             `Set(Charset.complement rng) :: tokens cs
    | '[' ::']':: cs      -> let (rng, cs) = read_range cs in
                             `Set(Charset.add rng ']') :: tokens cs
    | '[' ::'^'::cs       -> let (rng, cs) = read_range cs in
                             let rng = Charset.add rng '\255' in
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
    | (stk   , Acc(Alt (re::l)::acc), `Str   ::ts) ->
        build_re stk (Acc(Alt(Str re::l) :: acc)) ts
    | (stk   , Acc(Alt (re::l)::acc), `Pls   ::ts) ->
        build_re stk (Acc(Alt(Pls re::l) :: acc)) ts
    | (stk   , Acc(Alt (re::l)::acc), `Opt   ::ts) ->
        build_re stk (Acc(Alt(Opt re::l) :: acc)) ts
    | (stk   , Acc(re::acc), `Str   ::ts) ->
        build_re stk (Acc(Str re :: acc)) ts
    | (stk   , Acc(re::acc), `Pls   ::ts) ->
        build_re stk (Acc(Pls re :: acc)) ts
    | (stk   , Acc(re::acc), `Opt   ::ts) ->
        build_re stk (Acc(Opt re :: acc)) ts
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
exception Regexp_error of Input.buffer * Input.pos

let regexp_error : type a. Input.buffer -> Input.pos -> a = fun buf pos ->
  raise (Regexp_error(buf, pos))

let string_of_char_list : char list -> string = fun cs ->
  let b = Buffer.create 10 in
  List.iter (Buffer.add_char b) cs;
  Buffer.contents b

(* Input characters according to the given regexp. *)
let read : regexp -> Input.buffer -> Input.pos
           -> string list * Input.buffer * Input.pos =
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
