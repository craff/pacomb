(** Functions managing blanks *)

type buf = Input.buffer
type pos = Input.pos

(** A blank function is just a function progressing in a buffer *)
type blank = buf -> pos -> buf * pos
type t = blank

(** Use when you have no blank chars *)
let none : blank = fun s n -> (s,n)

(** Blank from a charset *)
let from_charset : Charset.t -> blank =
  fun cs s n ->
    let rec fn s n =
      let (c,s',n') = Input.read s n in
      if Charset.mem cs c then fn s' n' else (s,n)
    in
    fn s n

(** Blank from a terminal *)
let from_terminal : 'a Lex.t -> blank =
  fun t s n ->
    try
      let (_,s,n) = t.f s n in
      (s,n)
    with Lex.NoParse -> (s,n)

let line_comments : ?cs:Charset.t -> string -> blank =
  fun ?(cs=Charset.from_string " \t\n\r")  start_comment ->
    let start_comment = (Lex.string start_comment ()).f in
    fun s n ->
      let rec fn s n =
        let (c,s',n') = Input.read s n in
        if Charset.mem cs c then fn s' n' else
          try
            let (_,s,n) = start_comment s n in
            let rec gn s n =
              let (c,s',n') = Input.read s n in
              if c <> '\n' && c <> '\r' then gn s' n'
              else fn s n
            in
            gn s n
          with Lex.NoParse -> (s,n)
      in
      fn s n

(** Layout records *)

type layout_config =
  { old_blanks_before : bool
  (** Ignoring blanks with the old blank function before parsing? *)
  ; new_blanks_before : bool
  (** Then ignore blanks with the new blank function (before parsing)? *)
  ; new_blanks_after  : bool
  (** Use the new blank function one last time before resuming old layout? *)
  ; old_blanks_after  : bool
  (** Use then the old blank function one last time as well? *) }

let default_layout_config : layout_config =
  { old_blanks_before = true
  ; new_blanks_before = false
  ; new_blanks_after  = false
  ; old_blanks_after  = true }
