(** Functions managing positions *)

(** Position  in AST resulting  from parsetree may be  a non negligible  part of
   parsing complexity, both in speed and space. For instance: - computing column
   number in Unicode is costly - all information about a position takes a lot of
   machine words (up to 10)

    Moreover, in most cases, position are only used in error messages!

    Pacomb proposes to use three levels of positions to minimise the costs:

    1°)  the type  Pos.t =  Input.spos is  the type  return to  action. It  is a
   position in bytes together with some information about the file.

    2°) type type 'a located (or pos = unit located) is some data decorated with
   a start and end position. This is the type recommended to be used in AST.  it
   can be  constructed by the in_pos,  no_pos and mk_pos functions  below.  Each
   AST node located with a position costs 5 words.

    3°) the  type pos_info contain all  the information about a  position. It is
   produced by rescanning the file (or the string). If one parses a stream which
   is not  a regular  file, or  if the regular  file is  no more  available, the
   position is not really useful and pacomb will still print the byte position.
   *)

(** byte position from input, type type returned by parsing combinator *)
type spos = Input.spos

(** Type to represent data with a position *)
type pos =
  { offset_start : int
  ; offset_end   : int
  ; infos        : Input.infos }

let mk_pos s e infos =
  { offset_start = Input.int_of_byte_pos s
  ; offset_end   = Input.int_of_byte_pos e
  ; infos }

(** merging of two positions, create the smallest position pair containing both
   *)
let merge : pos -> pos -> pos = fun p1 p2 ->
  { p1 with
    offset_start = min p1.offset_start p2.offset_end
  ;  offset_end = max p1.offset_end p2.offset_end }


let phantom_pos =
  { offset_start = -1
  ; offset_end   = -1
  ; infos        = Input.phantom_infos }
let no_pos = phantom_pos

let has_pos p =
  p.offset_start <> -1 &&
  p.offset_end <> -1

let file_cache = Hashtbl.create 32
let str_cache = Hashtbl.create 32

(* The original file is not available *)
exception No_detailed_position

(* The file is available but has changed *)
exception File_changed

let init_file_cache file_name length date =
  try
    let ch = open_in file_name in
    let open Unix in
    let s = fstat (descr_of_in_channel ch) in
    if s.st_size <> length || s.st_mtime <> date then raise File_changed;
    let r =(ch, ref []) in
    Hashtbl.add file_cache file_name r;
    r
  with
  | Sys_error _ -> raise No_detailed_position

let init_str_cache str =
  let r =ref [] in
  Hashtbl.add str_cache str r;
  r

type pos_info =
  { start_line        : int
  ; start_col         : int
  ; start_line_offset : int
  ; start_byte        : int
  ; end_line          : int
  ; end_col           : int
  ; end_line_offset   : int
  ; end_byte          : int
  ; file_name         : string
  ; text              : string }

let cache_interval = 1024

let ch_sub_string ch n p =
  seek_in ch n;
  let s = p-n in
  let buf = Bytes.create s in
  really_input ch buf 0 s;
  Bytes.unsafe_to_string buf

let ch_col_num ch utf8 n p =
  if utf8 = Utf8.ASCII then p - n
  else
    begin
      let str = ch_sub_string ch n p in
      (*Printf.printf "text: %s\n%!" str;*)
      Utf8.length utf8 str
    end

let str_col_num str utf8 n p =
  if utf8 = Utf8.ASCII then p - n
  else
    begin
      let str = String.sub str n (p-n) in
      Utf8.length utf8 str
    end

let pos_info
    : ?relocate:(string -> string) -> ?text:bool -> pos -> pos_info
  = fun ?(relocate=fun x -> x) ?(text=false) pos ->
    let file_name, cache, seek_in, input_char, col_num, sub_string =
      match Input.stream_infos pos.infos with
      | File { name; length; date } ->
         let name0 = relocate name in
         let (ch, cache) = try Hashtbl.find file_cache name0
                           with Not_found -> init_file_cache name0 length date
         in
         let seek_in = seek_in ch in
         let input_char () = input_char ch in
         (name, cache, seek_in, input_char, ch_col_num ch, ch_sub_string ch)
      | String str ->
         let cache = try Hashtbl.find str_cache str
                     with Not_found -> init_str_cache str
         in
         let pos = ref 0 in
         let seek_in n = pos := n in
         let input_char () =
           if !pos >= String.length str then raise End_of_file;
           str.[!pos]
         in
         ("", cache, seek_in, input_char, str_col_num str, String.sub str)
      | Stream ->
         raise No_detailed_position
    in
    let n0 = pos.offset_start in
    let n1 = pos.offset_end   in
    if n0 = -1 || n1 = -1 then raise No_detailed_position;
    let utf8 = Input.utf8 pos.infos in
    let rec fn n = function
      | (p, _ , _, _)::ls when p > n -> fn n ls
      | c            ::_             -> c
      | []                           -> (0, 0,  1, file_name)
    in
    let rec gn n p lo ln name =
      let (ln, name) = Input.find_directives pos.infos p ln name in
      if p mod cache_interval = 0 then cache := (p, lo, ln, name) :: !cache;
      if p = n then (lo, ln, name) else
        begin
          assert(p < n);
          let c = try input_char () with End_of_file -> assert false in
          (*Printf.printf "%d %d %d %d %C\n" n p lo ln c;*)
          let p = p + 1 in
          if c = '\n' then gn n p p (ln + 1) name
          else gn n p lo ln name
        end
    in
    let to_eol n =
      let rec fn n =
        try
          let c = input_char () in
          if c = '\n' then n else fn (n+1)
        with
          End_of_file -> n
      in
      seek_in n;
      fn n
    in
    let (start_line_offset, start_line, file_name) =
      let (p, lo, ln, file_name) = fn n0 !cache in
      seek_in p;
      gn n0 p lo ln file_name
    in
    let (end_line_offset  , end_line, _) =
      let (p, lo, ln, file_name) = fn n1 !cache in
      seek_in p;
      gn n1 p lo ln file_name
    in
    let start_col = col_num utf8 start_line_offset n0 in
    let end_col   = col_num utf8 end_line_offset n1 in
    let text      = if text then sub_string start_line_offset (to_eol n1)
                    else ""
    in
    { start_line; start_col; start_line_offset
    ; end_line  ; end_col  ; end_line_offset
    ; start_byte = n0; end_byte = n1
    ; file_name ; text }

(* quote functions *)
type quote =
  { numbers  : bool
  ; prefix   : string
  ; header   : string
  ; footer   : string
  ; enlight  : string -> string }

type style =
  | OCaml
  | Short

let decorate : int -> string -> string = fun width s ->
  if s = "" then
    String.make width '='
  else
    let n = width - String.length s in
    let n1 = n / 2 in
    let n2 = n - n1 - 2 in
    String.make n1 '=' ^ " " ^ s ^ " " ^ String.make n2 '='

let _red : string -> string =
  fun s -> "\027[0m\027[31m" ^ s ^ "\027[0m"

let ulined : string -> string =
  fun s -> "\027[0m\027[4m" ^ s ^ "\027[0m"

let default_quote =
  { numbers  = true
  ; prefix   = ""
  ; header   = ""
  ; footer   = ""
  ; enlight  = ulined }

let quote_text : quote -> out_channel -> pos_info -> unit =
  fun quote ch pos ->
    let open Printf in
    if pos.text = "" then () else
    let lines = String.split_on_char '\n' pos.text in
    let start = pos.start_line in
    (*Printf.printf "start %d offset %d text: %s\n%!" start offset pos.text;*)
    let max_num =
      String.length (string_of_int (List.length lines + start - 1))
    in
    let last = List.length lines - 1 in
    let print i line =
      let line =
        if i = 0 && i = last then
          begin
            let byte_offset1 = pos.start_byte - pos.start_line_offset in
            let byte_offset2 = pos.end_byte - pos.end_line_offset in
            let s1 = byte_offset2 - byte_offset1 in
            let s2 = String.length line - byte_offset2 in
            String.sub line 0 byte_offset1 ^
              quote.enlight (String.sub line byte_offset1 s1) ^
                String.sub line byte_offset2 s2

          end
        else if i = 0 then
          begin
            let byte_offset = pos.start_byte - pos.start_line_offset in
            let s = String.length line - byte_offset in
            String.sub line 0 byte_offset ^
              quote.enlight (String.sub line byte_offset s)
          end
        else if i = last then
          begin
            let byte_offset = pos.end_byte - pos.end_line_offset in
            let s = String.length line - byte_offset in
            quote.enlight (String.sub line 0 byte_offset) ^
              String.sub line byte_offset s
          end
        else quote.enlight line
      in
      let number =
        if quote.numbers then
          let num = string_of_int (i + start) in
          let pad = String.make (max_num - String.length num) ' ' in
          pad ^ num ^ "|"
        else ""
      in
      fprintf ch "%s%s%s\n" quote.prefix number line
    in
    if quote.header <> "" then fprintf ch "%s\n" quote.header;
    List.iteri print lines;
    if quote.footer <> "" then
      fprintf ch "%s%s" quote.prefix quote.footer

let print_spos ?(style=OCaml) () ch ((infos,n):spos) =
  let open Printf in
  if n = Input.phantom_byte_pos then
    fprintf ch "NO POSITION"
  else
  let n = Input.int_of_byte_pos n in
  let name = Input.filename infos in
  if name = "" then
    let format : (_,_,_) format = match style with
      | OCaml -> "character %d"
      | Short -> "%d"
    in
    fprintf ch format n
  else
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, character %d"
      | Short -> "%S:%d"
    in
    fprintf ch format name n

let print_pos ?(style=OCaml) () ch pos =
  let open Printf in
  let n1 = pos.offset_start  in
  let n2 = pos.offset_end in
  if n1 = -1 || n2 = -1 then
    fprintf ch "NO POSITION"
  else
  let name = Input.filename pos.infos in
   if name = "" then
    let format : (_,_,_) format = match style with
      | OCaml -> "character %d-%d"
      | Short -> "%d-%d"
    in
    fprintf ch format n1 n2
  else
    let format : (_,_,_) format = match style with
      | OCaml -> "File %S, character %d to %d"
      | Short -> "%S:%d:%d"
    in
    fprintf ch format name n1 n2

let print_pos_info ?(style=OCaml) ?quote () ch (pos:pos_info) =
  let open Printf in
  let str_pos =
    if pos.file_name = "" then
      if pos.start_line = pos.end_line then
        if pos.start_col = pos.end_col then
          let format : (_,_,_) format = match style with
            | OCaml -> "line %d, characters %d"
            | Short -> "%d:%d"
          in
          sprintf format pos.start_line pos.start_col
        else
          let format : (_,_,_) format = match style with
            | OCaml -> "line %d, characters %d-%d"
            | Short -> "%d:%d-%d"
          in
          sprintf format pos.start_line pos.start_col pos.end_col
      else
        let format : (_,_,_) format = match style with
          | OCaml -> "line %d, character %d - line %d, character %d"
          | Short -> "%d:%d-%d:%d"
        in
        sprintf format pos.start_line
          pos.start_col pos.end_line pos.end_col
    else
      if pos.start_line = pos.end_line then
        if pos.start_col = pos.end_col then
          let format : (_,_,_) format = match style with
            | OCaml -> "File %S, line %d, characters %d"
            | Short -> "%S:%d:%d"
          in
          sprintf format pos.file_name pos.start_line pos.start_col
        else
          let format : (_,_,_) format = match style with
            | OCaml -> "File %S, line %d, characters %d-%d"
            | Short -> "%S:%d:%d-%d"
          in
          sprintf format pos.file_name pos.start_line
            pos.start_col pos.end_col
      else
        let format : (_,_,_) format = match style with
          | OCaml -> "File %S, line %d, character %d - line %d, character %d"
          | Short -> "%S:%d:%d-%d:%d"
        in
        sprintf format pos.file_name pos.start_line
          pos.start_col pos.end_line pos.end_col
  in
  match quote with
  | None -> fprintf ch "%s" str_pos
  | Some q -> quote_text { q with header = decorate 79 str_pos;
                                  footer = decorate 70 "" }
                ch pos

let print_spos ?(style=OCaml) ?quote () ch (infos,offset as p) =
  try
    let p2 = mk_pos offset offset infos in
    print_pos_info ~style ?quote () ch (pos_info ~text:(quote<>None) p2)
  with No_detailed_position ->
    print_spos ~style () ch p

let print_pos ?(style=OCaml) ?quote () ch p =
  try
    print_pos_info ~style ?quote () ch (pos_info ~text:(quote<>None) p)
  with No_detailed_position ->
    print_pos ~style () ch p

let print_buf_pos ?(style=OCaml) ?quote () ch (buf,idx) =
  print_spos ~style ?quote () ch (Input.spos buf idx)

(** exception returned by the parser *)
exception Parse_error of Input.buffer * Input.idx * string list

let fail_no_parse (_:exn) = exit 1

(** A helper to handle exceptions *)
let handle_exception ?(error=fail_no_parse) ?(style=OCaml) f a =
  try f a with Parse_error(buf, pos, msgs) as e ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: %a.\n%!")
      (print_buf_pos ~style ()) (buf, pos);
    if msgs <> [] then
      begin
        let open Format in
        let prl ch l = List.iter (fprintf ch "%s@ ") l in
        eprintf "@[<hov 2>expecting:@ %a@]@." prl msgs
      end;
    error e
