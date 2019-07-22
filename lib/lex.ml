open Utils

type buf = Input.buffer

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> int -> buf * int

(** Exception to be raised when the input is rejected *)
exception NoParse

(** Terminal: same as blank with a value returned *)
type 'a lexeme = buf -> int -> 'a * buf * int
type 'a terminal = { n : string    (** name *)
                   ; f : 'a lexeme (** the terminal itself *)
                   ; c : Charset.t (** the set of characters accepted
                                       at the beginning of input *) }
type 'a t = 'a terminal

(** Test wether a terminal accept the empty sequence *)
let s0 = Input.from_string ""
let accept_empty : type a. a t -> bool = fun t ->
  try ignore(t.f s0 0); true with NoParse -> false

(** Combinators to create terminals *)

(** Terminal accepting then end of a buffer only.
    remark: [eof] is automatically added at the end of a grammar by
    [Combinator.parse_buffer]. *)
let eof : ?name:string -> 'a -> 'a t = fun ?(name="EOF") x ->
  { n = name
  ; c = Charset.singleton '\255'
  ; f = fun s n ->
        if Input.is_empty s n then (x, s, n) else raise NoParse
  }

let sp = Printf.sprintf

(** Terminal accepting a given char, remark: [char '\255'] is equivalent to
    [eof]. *)
let char : ?name:string -> char -> 'a -> 'a t = fun ?name c x ->
  { n = Option.get (sp "%C" c) name
  ; c = Charset.singleton c
  ; f = fun s n ->
        let (c',s,n) = Input.read s n in
        if c = c' then (x,s,n) else raise NoParse
  }

let charset_from_test f =
  let l = ref Charset.empty in
  for i = 0 to 255 do
    let c = Char.chr i in
    if f c then l := Charset.add !l c
  done;
  !l

(** Accept a character for which the test returns [true] *)
let test : ?name:string -> (char -> bool) -> char t = fun ?name f ->
  let cs = charset_from_test f in
  { n = Option.get (Charset.show cs) name
  ; c = charset_from_test f
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
let not_test : ?name:string -> (char -> bool) -> 'a -> 'a t =
  fun ?name f a ->
  let cs = charset_from_test f in
  { n = Option.get (sp "^%s" (Charset.show cs)) name
  ; c = Charset.complement cs
  ; f = fun s n ->
        let (c,_,_) = Input.read s n in
        if (f c) then raise NoParse else (a, s, n)
  }

(** Reject the input (raises [Noparse]) if the first character of the input
    is in the charset. Does not read the character if not in the charset. *)
let not_charset : ?name:string -> Charset.t -> 'a -> 'a t =
  fun ?name cs a -> not_test ?name (Charset.mem cs) a

(** Compose two terminals in sequence *)
let seq : ?name:string -> 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t =
  fun ?name t1 t2 f ->
  { n = Option.get (sp "%s%s" t1.n t2.n) name
  ; c = if accept_empty t1 then Charset.union t1.c t2.c else t1.c
  ; f = fun s n ->
        let (s1,s,n) = t1.f s n in
        let (s2,s,n) = t2.f s n in
        (f s1 s2,s,n)
  }
let seq1 ?name t1 t2 = seq ?name t1 t2 (fun x _ -> x)
let seq2 ?name t1 t2 = seq ?name t1 t2 (fun _ x -> x)

(** [alt t1 t2] parses the input with [t1] or [t2]. *)
let alt : ?name:string -> 'a t -> 'a t -> 'a t =
  fun ?name t1 t2 ->
  { n = Option.get (sp "(%s)|(%s)" t1.n t2.n) name
  ; c = Charset.union t1.c t2.c
  ; f = fun s n ->
        try
          let (_,s1,n1 as r1) = t1.f s n in
          try
            let (_,s2,n2 as r2) = t2.f s n in
            let l1 = Input.line_num s1 in
            let l2 = Input.line_num s2 in
            if l2 > l1 || (l2 = l1 && n2 > n1) then r2 else r1
          with NoParse -> r1
        with NoParse -> t2.f s n
  }

let save : ?name:string -> 'a t -> (string -> 'a -> 'b) -> 'b t =
  fun ?name t1 f ->
  { n = Option.get t1.n name
  ; c = t1.c
  ; f = fun s n ->
        let (l,s1,n1) = t1.f s n in
        let len = Input.line_offset s1 + n1
                  - Input.line_offset s - n
        in
        let str = Input.sub s n len in
        (f str l, s1, n1) }

(** Parses the given terminal 0 or 1 time. *)
let option : ?name:string -> 'a -> 'a t -> 'a t =
  fun ?name d t ->
  { n = Option.get (sp "(%s)?" t.n) name
  ; c = Charset.full
  ; f = fun s n ->
        try let (x,s,n) = t.f s n in (x,s,n)
        with NoParse -> (d,s,n) }

(** Applies a function to the result of the given terminal. *)
let appl : ?name: string -> ('a -> 'b) -> 'a t -> 'b t =
  fun ?name f t ->
  { n = Option.get t.n name
  ; c = t.c
  ; f = fun s n -> let (x,s,n) = t.f s n in (f x,s,n) }

(** [star t a f] Repetition of a given terminal 0,1 or more times. *)
let star : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t =
  fun ?name t a f ->
  { n = Option.get (sp "(%s)*" t.n) name
  ; c = t.c
  ; f = fun s n ->
        let rec fn a s n =
          (try
            let (x,s,n) = t.f s n in
            fun () -> fn (f a x) s n
          with NoParse ->
            fun () -> (a,s,n)) ()
        in
        fn (a ()) s n }

(** Same as above but parses at least once .*)
let plus : ?name:string -> 'a t -> (unit -> 'b) -> ('b -> 'a -> 'b) -> 'b t =
  fun ?name t a f ->
  { n = Option.get (sp "(%s)*" t.n) name
  ; c = t.c
  ; f = fun s n ->
        let rec fn a s n =
          (try
            let (x,s,n) = t.f s n in
            fun () -> fn (f a x) s n
           with NoParse ->
             fun () -> (a,s,n)) ()
        in
        let (x,s,n) = t.f s n in
        fn (f (a ()) x) s n }

(** [string s] Accepts only the given string.*)
let string : ?name:string -> string -> 'a -> 'a t = fun ?name k x ->
  if k = "" then invalid_arg "Lex.string: empty string";
  { n = Option.get (sp "%S" k) name
  ; c = Charset.singleton k.[0]
  ; f = fun s n ->
        let l = String.length k in
        let rec fn i s n =
          if i >= l then (s,n) else
            let c,s,n = Input.read s n in
            if c <> k.[i] then raise NoParse;
            fn (i+1) s n
        in
        let (s,n) = fn 0 s n in
        (x,s,n) }

(** Parses an integer in base 10. ["+42"] is accepted. *)
let int : ?name:string -> unit -> int t = fun ?name () ->
  { n = Option.get "INT" name
  ; c = Charset.from_string "-+0-9"
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
  { n = Option.get "FLOAT" name
  ; c = Charset.from_string "-+0-9."
  ; f = fun s0 n0 ->
        let b = Buffer.create 16 in
        let found_digit = ref false in
        let rec fn s0 n0 =
          let (c,s,n) = Input.read s0 n0 in
          if (c >= '0' && c <= '9') then (
            found_digit := true;
            Buffer.add_char b c;
            fn s n)
          else (c,s,n,s0,n0)
        in
        let gn c s n s0 n0 =
          if (c >= '0' && c <= '9') then (
            found_digit := true;
            Buffer.add_char b c;
            fn s n)
          else (c,s,n,s0,n0)
        in
        let (c,s,n,s0,n0) =
          let (c,s,n) = Input.read s0 n0 in
          if c = '+' || c = '-' then
            (Buffer.add_char b c; fn s n)
          else gn c s n s0 n0
        in
        let (c,s,n,s0,n0) =
          if c <> '.' then (c,s,n,s0,n0) else
            begin
              Buffer.add_char b c;
              fn s n
            end
        in
        if not !found_digit then raise NoParse;
        let (_,_s,_n,s0,n0) =
          if c <> 'E' && c <> 'e' then (c,s,n,s0,n0) else
            begin
              Buffer.add_char b c;
              let (c,s,n) =
                let (c,s,n as r) = Input.read s n in
                if c = '+' || c = '-' then
                  (Buffer.add_char b c; Input.read s n)
                else r
              in
              if not (c >= '0' && c <= '9') then raise NoParse;
              Buffer.add_char b c;
              fn s n
            end
        in
        (float_of_string (Buffer.contents b), s0, n0) }

let rec alts : 'a t list -> 'a t = function
  | [] -> invalid_arg "alts: empty list"
  | [r] -> r
  | r::l -> alt r (alts l)

let seqs : 'a t list -> ('a -> 'a -> 'a) -> 'a t = fun l f ->
  let rec fn = function
  | [] -> invalid_arg "alts: empty list"
  | [r] -> r
  | r::l -> seq r (fn l) f
  in fn l

let from_regexp : Regexp.t -> string t = fun r ->
  let open Regexp in
  let rec fn = function
  | Chr c -> char c ()
  | Set s -> appl (fun _ -> ()) (charset s)
  | Alt l -> alts (List.map fn l)
  | Seq l -> seqs (List.map fn l) (fun () () -> ())
  | Opt r -> option () (fn r)
  | Str r -> star (fn r) (fun () -> ()) (fun () () -> ())
  | Pls r -> plus (fn r) (fun () -> ()) (fun () () -> ())
  | Sav r -> fn r
  in
  save (fn r) (fun s () -> s)

let from_regexp_grps : Regexp.t -> string list t = fun r ->
  let open Regexp in
  let rec fn = function
  | Chr c -> char c []
  | Set s -> appl (fun _ -> []) (charset s)
  | Alt l -> alts (List.map fn l)
  | Seq l -> seqs (List.map fn l) (@)
  | Opt r -> option [] (fn r)
  | Str r -> star (fn r) (fun () -> []) (@)
  | Pls r -> plus (fn r) (fun () -> []) (@)
  | Sav r -> save (fn r) (fun s l -> s :: l)
  in
  fn r

(** keyword *)
let keyword : ?name:string -> string -> (char -> bool) -> 'a -> 'a t =
  fun ?name k f x ->
    seq ?name (string k ()) (test f) (fun _ _ -> x)

(** create a terminal from a regexp. Returns the groups list, last to finish
    to be parsed is first in the result *)
let regexp_grps : ?name:string -> Regexp.t -> string list t = fun ?name r ->
  let r = from_regexp_grps r in
  { r with n = Option.get r.n name }

let regexp : ?name:string -> Regexp.t -> string t = fun ?name r ->
  let r = from_regexp r in
  { r with n = Option.get r.n name }

(** Functions managing blanks *)

(** Use when you have no blank chars *)
let noblank : blank = fun s n -> (s,n)

(** Blank from a charset *)
let blank_charset : Charset.t -> blank =
  fun cs s n ->
    let rec fn s n =
      let (c,s',n') = Input.read s n in
      if Charset.mem cs c then fn s' n' else (s,n)
    in
    fn s n

(** Blank from a terminal *)
let blank_terminal : 'a t -> blank =
  fun t s n ->
    try
      let (_,s,n) = t.f s n in
      (s,n)
    with NoParse -> (s,n)
