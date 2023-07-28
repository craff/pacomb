let default : 'a -> 'a option -> 'a = fun d o ->
  match o with
  | None   -> d
  | Some e -> e

type buf = Input.buffer
type idx = Input.idx

(** Exception to be raised when the input is rejected *)
exception NoParse

exception Give_up of string

(** [give_up ()] rejects parsing from a corresponding semantic action. *)
let give_up : ?msg:string -> unit -> 'a = fun ?msg () ->
  match msg with None -> raise NoParse
               | Some s -> raise (Give_up s)

type 'a lexeme = buf -> idx -> 'a * buf * idx

type _ ast =
  | Any : char ast
  | Any_utf8 : Uchar.t ast
  | Any_grapheme : string ast
  | Eof : unit ast
  | Char : char -> unit ast
  | Grapheme : string -> unit ast
  | String : string -> unit ast
  | Nat : int ast
  | Int : int ast
  | Float : float ast
  | CharLit : char ast
  | StringLit : string ast
  | Test : (char -> bool) -> char ast
  | NotTest : (char -> bool) -> unit ast
  | Seq : 'a t * 'b t * ('a -> 'b -> 'c) * 'c Assoc.key -> 'c ast
  | Alt : 'a t * 'a t -> 'a ast
  | Save : 'a t * (string -> 'a -> 'b) * 'b Assoc.key -> 'b ast
  | Option : 'a * 'a t -> 'a ast
  | Appl : 'a t * ('a -> 'b) * 'b Assoc.key -> 'b ast
  | Star : 'a t * (unit -> 'b) * ('b -> 'a -> 'b) * 'b Assoc.key -> 'b ast
  | Plus : 'a t * (unit -> 'b) * ('b -> 'a -> 'b) * 'b Assoc.key -> 'b ast
  | Sub : 'a t * ('a -> bool) * 'a Assoc.key -> 'a ast
  | Keyword : string * int -> unit ast
  | Custom : 'a lexeme * 'a Assoc.key -> 'a ast

(** Terminal: same as blank with a value returned *)
and 'a terminal = { n : string    (** name *)
                   ; f : 'a lexeme (** the terminal itself *)
                   ; a : 'a ast
                   ; c : Charset.t (** the set of characters accepted
                                       at the beginning of input *)
                   }

and 'a t = 'a terminal

let custom fn = Custom(fn, Assoc.new_key ())

let rec eq : type a b.a t -> b t -> (a,b) Assoc.eq =
  fun a b ->
    let open Assoc in
    match (a.a, b.a) with
    | Any, Any -> Eq
    | Any_utf8, Any_utf8 -> Eq
    | Any_grapheme, Any_grapheme -> Eq
    | Eof, Eof -> Eq
    | Char c1, Char c2 -> if c1 = c2 then Eq else NEq
    | Grapheme s1, Grapheme s2 -> if s1 = s2 then Eq else NEq
    | String s1, String s2 -> if s1 = s2 then Eq else NEq
    | Nat, Nat -> Eq
    | Int, Int -> Eq
    | Float, Float -> Eq
    | CharLit, CharLit -> Eq
    | StringLit, StringLit -> Eq
    | Test af, Test bf -> if af == bf then Eq else NEq
    | NotTest af, NotTest bf -> if af == bf then Eq else NEq
    | Seq(_,_,_,ak), Seq(_,_,_,bk) -> ak.eq bk.tok
    | Alt(a1,a2), Alt(b1,b2) ->
       begin
         match eq a1 b1, eq a2 b2 with
         | Eq, Eq -> Eq
         | _      -> NEq
       end
    | Save(_,_,ak), Save(_,_,bk) -> ak.eq bk.tok
    | Option(ad,a1), Option(bd,b1) ->
       begin
         match eq a1 b1 with
         | Eq -> if ad == bd then Eq else NEq
         | _ -> NEq
       end
    | Appl(_,_,ak), Appl(_,_,bk) -> ak.eq bk.tok
    | Star(_,_,_,ak), Star(_,_,_,bk) -> ak.eq bk.tok
    | Plus(_,_,_,ak), Plus(_,_,_,bk) -> ak.eq bk.tok
    | Sub(_,_,ak), Sub(_,_,bk) -> ak.eq bk.tok
    | Keyword(s1,uid1), Keyword(s2,uid2) ->
       if s1 = s2 && uid1 = uid2 then Eq else NEq
    | Custom(af,ak), Custom(bf,bk) ->
       begin
         match ak.eq bk.tok with
         | Eq when af == bf -> Eq
         | _ -> NEq
       end
    | _ -> NEq

let s0 = Input.from_string ""
let s1 = Input.from_string "\255 "(* for eof to passe the test *)
let accept_empty : type a. a t -> bool = fun t ->
  try ignore(t.f s0 Input.init_idx);
      try let (_,b,idx) = t.f s1 Input.init_idx in
          Input.int_of_byte_pos (Input.byte_pos b idx) <> 1
      with NoParse -> true
  with NoParse -> false

let test_from_lex : bool t -> buf -> idx -> buf -> idx -> bool =
  fun t _ _ buf idx ->
      try let (r,_,_) = t.f buf idx in r
      with NoParse | Give_up _ -> false

let blank_test_from_lex : bool t -> buf -> idx -> buf -> idx -> bool =
  fun t buf idx _ _ ->
      try let (r,_,_) = t.f buf idx in r
      with NoParse | Give_up _ -> false


(** Combinators to create terminals *)

let any : ?name:string -> unit -> char t = fun ?(name="ANY") () ->
  { n = name
  ; c = Charset.full
  ; a = Any
  ; f = fun s n -> let (c,_,_ as res) = Input.read s n
                   in if c = '\255' then raise NoParse else res
  }

(** Terminal accepting then end of a buffer only.
    remark: [eof] is automatically added at the end of a grammar by
    [Combinator.parse_buffer]. *)
let eof : ?name:string -> unit -> unit t = fun ?(name="EOF") () ->
  { n = name
  ; c = Charset.singleton '\255'
  ; a = Eof
  ; f = fun s n -> let (c,s,n) = Input.read s n in
                   if c = '\255' then ((),s,n) else raise NoParse
  }

let sp = Printf.sprintf

(** Terminal accepting a given char, remark: [char '\255'] is equivalent to
    [eof]. *)
let char : ?name:string -> char -> unit t = fun ?name c ->
  { n = default (sp "'%c'" c) name
  ; c = Charset.singleton c
  ; a = Char c
  ; f = fun s n ->
        let (c',s,n) = Input.read s n in
        if c = c' then ((),s,n) else raise NoParse
  }

let charset_from_test f =
  let l = ref Charset.empty in
  for i = 0 to 255 do
    let c = Char.chr i in
    if f c then l := Charset.add !l c
  done;
  !l

let any_utf8 : ?name:string -> unit -> Uchar.t t = fun ?name () ->
  { n = default "UTF8" name
  ; c = Charset.full
  ; a = Any_utf8
  ; f = fun s n ->
        let (c1,s,n) = Input.read s n in
        if c1 = '\255' then raise NoParse;
        let n1 = Char.code c1 in
        let (n0,s,n) =
          if n1 land 0b1000_0000 = 0 then (n1 land 0b0111_1111, s, n)
          else if n1 land 0b1110_0000 = 0b1100_0000 then
            begin
              let (c2,s,n) = Input.read s n in
              let n2 = Char.code c2 in
              if n2 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              (((n1 land 0b0001_1111) lsl 6) lor
                (n2 land 0b0011_1111), s , n)
            end
          else if n1 land 0b1111_0000 = 0b1110_0000 then
            begin
              let (c2,s,n) = Input.read s n in
              let n2 = Char.code c2 in
              if n2 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              let (c3,s,n) = Input.read s n in
              let n3 = Char.code c3 in
              if n3 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              (((n1 land 0b0000_1111) lsl 12) lor
                ((n2 land 0b0011_1111) lsl 6) lor
                  (n3 land 0b0011_1111), s, n)
            end
          else if n1 land 0b1111_1000 = 0b1111_0000 then
            begin
              let (c2,s,n) = Input.read s n in
              let n2 = Char.code c2 in
              if n2 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              let (c3,s,n) = Input.read s n in
              let n3 = Char.code c3 in
              if n3 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              let (c4,s,n) = Input.read s n in
              let n4 = Char.code c4 in
              if n4 land 0b1100_0000 <> 0b1000_0000 then raise NoParse;
              (((n1 land 0b0000_0111) lsl 18) lor
                ((n2 land 0b0011_1111) lsl 12) lor
                  ((n3 land 0b0011_1111) lsl 6) lor
                    (n4 land 0b0011_1111), s, n)
            end
          else raise NoParse
        in
        (Uchar.of_int n0,s,n)
  }

(** [string s] Accepts only the given string.*)
let string : ?name:string -> string -> unit t = fun ?name k ->
  if k = "" then invalid_arg "Lex.string: empty string";
  { n = default (sp "\"%s\"" k) name
  ; c = Charset.singleton k.[0]
  ; a = String k
  ; f = fun s n ->
        let l = String.length k in
        let rec fn i s n =
          if i >= l then (s,n) else
            let c,s,n = Input.read s n in
            if c <> k.[i] then raise NoParse;
            fn (i+1) s n
        in
        let (s,n) = fn 0 s n in
        ((),s,n) }

let utf8 : ?name:string -> Uchar.t -> unit t = fun ?name k ->
  string ?name (Utf8.encode k)

let any_grapheme : ?name:string -> unit -> string t = fun ?name () ->
  { n = default "GRAPHEME" name
  ; c = Charset.full
  ; a = Any_grapheme
  ; f = fun s n ->
        let rec fn acc s n =
          try
            let (c,s',n') = (any_utf8 ()).f s n in
            if acc <> [] && Utf8.grapheme_break_after acc c then
              (acc,s,n)
            else
              fn (c::acc) s' n'
          with NoParse ->
            if acc <> [] then (acc, s, n) else raise NoParse
        in
        try
          let (l,s,n) = fn [] s n in
          (String.concat "" (List.rev_map Utf8.encode l),s, n)
        with Invalid_argument _ -> raise NoParse }

let grapheme : ?name:string -> string -> unit t = fun ?name k ->
  if k = "" then invalid_arg "Lex.grapheme: empty string";
  { n = default ("GRAPHEME("^k^")") name
  ; c = Charset.singleton k.[0]
  ; a = Grapheme k
  ; f = fun s n ->
        let (k',s,n) = (any_grapheme ()).f s n in
        if k = k' then ((),s,n) else raise NoParse }

(** Accept a character for which the test returns [true] *)
let test : ?name:string -> (char -> bool) -> char t = fun ?name f ->
  let cs = charset_from_test f in
  { n = default (Charset.show cs) name
  ; c = charset_from_test f
  ; a = Test f
  ; f = fun s n ->
        let (c,s,n) = Input.read s n in
        if f c then (c, s, n)
        else raise NoParse
  }

(** Accept a character in the given charset *)
let charset : ?name:string -> Charset.t -> char t = fun ?name cs ->
  test ?name (Charset.mem cs)

(** Reject the input (raises [Noparse]) if the first character of the input
    passed the test. Does not read the character if the test fails. *)
let not_test : ?name:string -> (char -> bool) -> unit t =
  fun ?name f ->
  let cs = charset_from_test f in
  { n = default (sp "^%s" (Charset.show cs)) name
  ; c = Charset.complement cs
  ; a = NotTest f
  ; f = fun s n ->
        let (c,_,_) = Input.read s n in
        if (f c) then raise NoParse else ((), s, n)
  }

(** Reject the input (raises [Noparse]) if the first character of the input
    is in the charset. Does not read the character if not in the charset. *)
let not_charset : ?name:string -> Charset.t -> unit t =
  fun ?name cs -> not_test ?name (Charset.mem cs)

(** does a test on the result of a given lexer and reject if it returns
    false.*)
let sub : ?name:string -> ?charset:Charset.t -> 'a t -> ('a -> bool) -> 'a t =
  fun ?name ?charset t test ->
  { n = default (sp "sub(%s)" t.n) name
  ; c = default t.c charset
  ; a = Sub(t, test, Assoc.new_key ())
  ; f = fun s n ->
        let (c,_,_) as r = t.f s n in
        if test c then r else raise NoParse }


(** Compose two terminals in sequence *)
let seq : ?name:string -> 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun ?name t1 t2 f ->
  { n = default (sp "%s%s" t1.n t2.n) name
  ; c = if accept_empty t1 then Charset.union t1.c t2.c else t1.c
  ; a = Seq(t1, t2, f, Assoc.new_key ())
  ; f = fun s n ->
        let (s1,s,n) = t1.f s n in
        let (s2,s,n) = t2.f s n in
        (f s1 s2,s,n)
  }
let seq1 ?name t1 t2 = seq ?name t1 t2 (fun x _ -> x)
let seq2 ?name t1 t2 = seq ?name t1 t2 (fun _ x -> x)

(** multiple sequence *)
let seqs : 'a t list -> ('a -> 'a -> 'a) -> 'a t = fun l f ->
  let rec fn = function
  | [] -> invalid_arg "alts: empty list"
  | [r] -> r
  | r::l -> seq r (fn l) f
  in fn l

(** [alt t1 t2] parses the input with [t1] or [t2]. *)
let alt : ?name:string -> 'a t -> 'a t -> 'a t =
  fun ?name t1 t2 ->
  { n = default (sp "(%s)|(%s)" t1.n t2.n) name
  ; c = Charset.union t1.c t2.c
  ; a = Alt(t1, t2)
  ; f = fun s n ->
        try
          let (_,s1,n1 as r1) = t1.f s n in
          try
            let (_,s2,n2 as r2) = t2.f s n in
            let l1 = Input.byte_pos s1 n1 in
            let l2 = Input.byte_pos s2 n2 in
            if l2 > l1 then r2 else r1
          with NoParse -> r1
        with NoParse -> t2.f s n
  }

(** multiple alternatives *)
let rec alts : 'a t list -> 'a t = function
  | [] -> invalid_arg "alts: empty list"
  | [r] -> r
  | r::l -> alt r (alts l)

(** save the content of the buffer in a string *)
let save : ?name:string -> 'a t -> (string -> 'a -> 'b) -> 'b t =
  fun ?name t1 f ->
  { n = default t1.n name
  ; c = t1.c
  ; a = Save(t1,f, Assoc.new_key ())
  ; f = fun s n ->
        let (l,s1,n1) = t1.f s n in
        let len =
          Input.int_of_byte_pos (Input.byte_pos s1 n1)
          - Input.int_of_byte_pos (Input.byte_pos s n) in
        let str = Input.sub s n len in
        (f str l, s1, n1) }

(** Parses the given terminal 0 or 1 time. *)
let option : ?name:string -> 'a -> 'a t -> 'a t =
  fun ?name d t ->
  { n = default (sp "(%s)?" t.n) name
  ; c = Charset.full
  ; a = Option(d,t)
  ; f = fun s n ->
        try let (x,s,n) = t.f s n in (x,s,n)
        with NoParse -> (d,s,n) }

(** Applies a function to the result of the given terminal. *)
let appl : ?name: string -> ('a -> 'b) -> 'a t -> 'b t =
  fun ?name f t ->
  { n = default t.n name
  ; c = t.c
  ; a = Appl(t, f, Assoc.new_key ())
  ; f = fun s n -> let (x,s,n) = t.f s n in (f x,s,n) }

(** [star t a f] Repetition of a given terminal 0,1 or more times. *)
let star : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t =
  fun ?name t a f ->
  { n = default (sp "(%s)*" t.n) name
  ; c = t.c
  ; a = Star(t,a,f, Assoc.new_key ())
  ; f = fun s n ->
        let rec fn a s n =
          (try
            let (x,s',n') = t.f s n in
            if Input.buffer_equal s s' && n = n' then
              fun () -> (a,s,n)
            else
              fun () -> fn (f a x) s' n'
          with NoParse ->
            fun () -> (a,s,n)) ()
        in
        fn (a ()) s n }

(** Same as above but parses at least once .*)
let plus : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t =
  fun ?name t a f ->
  { n = default (sp "(%s)*" t.n) name
  ; c = t.c
  ; a = Plus(t,a,f, Assoc.new_key ())
  ; f = fun s n ->
        let rec fn a s n =
          (try
            let (x,s',n') = t.f s n in
            if Input.buffer_equal s s' && n = n' then
              fun () -> (a,s,n)
            else
              fun () -> fn (f a x) s' n'
           with NoParse ->
             fun () -> (a,s,n)) ()
        in
        let (x,s,n) = t.f s n in
        fn (f (a ()) x) s n }

(** Parses natura in base 10. ["+42"] is not accepted. *)
let nat : ?name:string -> unit -> int t = fun ?name () ->
  { n = default "NAT" name
  ; c = Charset.from_string "-+0-9"
  ; a = Nat
  ; f = fun s n ->
        let r = ref 0 in
        let (c,s,n) = Input.read s n in
        if not (c >= '0' && c <= '9') then raise NoParse;
        r := !r * 10 + (Char.code c - Char.code '0');
        let rec fn s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            r := !r * 10 + (Char.code c - Char.code '0');
            fn s n)
          else (s0,n0)
        in
        let (s,n) = fn s n in
        (!r,s,n) }

(** Parses an integer in base 10. ["+42"] is accepted. *)
let int : ?name:string -> unit -> int t = fun ?name () ->
  { n = default "INT" name
  ; c = Charset.from_string "-+0-9"
  ; a = Int
  ; f = fun s n ->
        let r = ref 0 in
        let f = ref (fun x -> x) in
        let (c,s,n) =
          let (c,s,n as r) = Input.read s n in
          if c = '+' then Input.read s n
          else if c = '-' then
            (f := (fun x -> -x); Input.read s n)
          else r
        in
        if not (c >= '0' && c <= '9') then raise NoParse;
        r := !r * 10 + (Char.code c - Char.code '0');
        let rec fn s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            r := !r * 10 + (Char.code c - Char.code '0');
            fn s n)
          else (s0,n0)
        in
        let (s,n) = fn s n in
        (!f !r,s,n) }

(** Parses a float in base 10. [".1"] is as ["0.1"]. *)
let float : ?name:string -> unit -> float t = fun ?name () ->
  { n = default "FLOAT" name
  ; c = Charset.from_string "-+0-9."
  ; a = Float
  ; f = fun s0 n0 ->
        let sg = ref 1.0 in
        let ve = ref 0.0 in
        let found_digit = ref false in
        let m = ref 0.0 in
        let digit c = float (Char.code c - Char.code '0')
        in
        let rec fn s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            found_digit := true;
            m := !m *. 10.0 +. digit c;
            fn s n)
          else (c,s,n,s0,n0)
        in
        let rec fne s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            found_digit := true;
            m := !m *. 10.0 +. digit c;
            ve := !ve +. 1.0;
            fne s n)
          else (c,s,n,s0,n0)
        in
        let (c,s,n,s0,n0) =
          let (c,s,n) = Input.read s0 n0 in
          if c = '+' then fn s n
          else if c = '-' then (sg := -1.0; fn s n)
          else if (c >= '0' && c <= '9') then (
            found_digit := true;
            m := digit c;
            fn s n)
          else (c,s,n,s0,n0)
        in
        let (c,s,n,s0,n0) =
          if c <> '.' then (c,s,n,s0,n0) else
            fne s n
        in
        if not !found_digit then raise NoParse;
        let sge = ref 1.0 in
        let e = ref 0.0 in
        let rec hn s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            found_digit := true;
            e := !e *. 10.0 +. digit c;
            hn s n)
          else (s0,n0)
        in
        let (s0,n0) =
          if c <> 'E' && c <> 'e' then (s0,n0) else
            begin
              let (c,s,n) =
                let (c,s,n as r) = Input.read s n in
                if c = '+' then
                  Input.read s n
                else if c = '-' then
                  (sge := -1.0; Input.read s n)
                else r
              in
              if not (c >= '0' && c <= '9') then raise NoParse;
              e := digit c;
              hn s n
            end
        in
        (!sg *. !m *. (10.0 ** (-. !ve)) *. (10.0 ** (!sge *. !e)), s0, n0) }

(** escaped char for string and char litteral below *)
exception Escaped
let escaped = fun c s n ->
  if c = '\\' then
    let (c,s,n) = Input.read s n in
    match c with
    | '\\' -> ('\\', s, n)
    | '\'' -> ('\'', s, n)
    | '\"' -> ('"', s, n)
    | 'n'  -> ('\n', s, n)
    | 'r'  -> ('\r', s, n)
    | 't'  -> ('\t', s, n)
    | 'b'  -> ('\b', s, n)
    | '0'..'2' ->
       let (c1,s,n) = Input.read s n in
       let (c2,s,n) = Input.read s n in
       if (c1 >= '0' && c1 <= '9' && c2 >= '0' && c2 <= '9') then
         begin
           let c = (  (Char.code c  - Char.code '0') * 100
                      + (Char.code c1 - Char.code '0') * 10
                      + (Char.code c2 - Char.code '0'))
           in
           if c < 256 then (Char.chr c, s, n) else
             raise NoParse
         end
       else raise NoParse
    | 'o' ->
       let (c1,s,n) = Input.read s n in
       let (c2,s,n) = Input.read s n in
       let (c3,s,n) = Input.read s n in
       if (c1 >= '0' && c1 <= '3' && c2 >= '0' && c2 <= '7'
           && c3 >= '0' && c3 <= '7') then
         let c = (  (Char.code c1 - Char.code '0') * 64
                  + (Char.code c2 - Char.code '0') * 8
                  + (Char.code c3 - Char.code '0'))
         in
         (Char.chr c, s, n)
       else raise NoParse
    | 'x' ->
       let (c1,s,n) = Input.read s n in
       let (c2,s,n) = Input.read s n in
       let x1 = match c1 with
         | '0'..'9' -> Char.code c1 - Char.code '0'
         | 'a'..'f' -> Char.code c1 - Char.code 'a' + 10
         | 'A'..'F' -> Char.code c1 - Char.code 'A' + 10
         | _       -> raise NoParse
       in
       let x2 = match c2 with
         | '0'..'9' -> Char.code c2 - Char.code '0'
         | 'a'..'f' -> Char.code c2 - Char.code 'a' + 10
         | 'A'..'F' -> Char.code c2 - Char.code 'A' + 10
         | _       -> raise NoParse
       in
      (Char.chr (x1 * 16 + x2), s, n)
    | _ -> raise NoParse
  else raise Escaped

(** char literal *)
let char_lit : ?name:string -> unit -> char t = fun ?name () ->
  { n = default "CHARLIT" name
  ; c = Charset.singleton '\''
  ; a = CharLit
  ; f = fun s n ->
        let (c,s,n) = Input.read s n in
        if c <> '\'' then raise NoParse;
        let (c,s,n as r) = Input.read s n in
        if c = '\'' || c = '\255' then raise NoParse;
        let (cr,s,n) = try escaped c s n with Escaped -> r in
        let (c,s,n) = Input.read s n in
        if c <> '\'' then raise NoParse;
        (cr,s,n)
  }

(** treatment of escaped newline in string literal *)
let rec skip_newline c s0 n0 =
  let rec fn s0 n0 =
    let (c,s,n) = Input.read s0 n0 in
    if c = ' ' || c = '\t' then fn s n
    else skip_newline c s n
  in
  if c = '\\' then
    let (c1,s,n) = Input.read s0 n0 in
    if c1 = '\n' then fn s n else (c,s0, n0)
  else (c,s0,n0)

(** string literal *)
let string_lit : ?name:string -> unit -> string t = fun ?name () ->
  { n = default "STRINTLIT" name
  ; c = Charset.singleton '"'
  ; a = StringLit
  ; f = fun s n ->
        let (c,s,n) = Input.read s n in
        if c <> '"' then raise NoParse;
        let b = Buffer.create 64 in
        let rec fn s n =
          let (c,s,n) = Input.read s n in
          let (c,s,n as r) = skip_newline c s n in
          if c = '"' then (s,n)
          else if c = '\255' then raise NoParse
          else
            begin
              let (cr,s,n) = try escaped c s n with Escaped -> r in
              Buffer.add_char b cr;
              fn s n
            end
        in
        let (s,n) = fn s n in
        (Buffer.contents b,s,n)
  }
