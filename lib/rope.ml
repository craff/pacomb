let _ = Printexc.record_backtrace true

type t = Empty
       | Cons of { buffer : string
                 ; offset : int
                 ; length : int
                 ; next   : t Lazy.t }

let cons buffer offset length next =
  if length = 0 then Empty
  else (Printf.printf "%d %d\n%!" length offset;
        Cons{ buffer; offset; length; next })

let length = function
           | Empty                 -> 0
           | Cons { length= n; _ } -> n

let newline = function
  | Empty -> false
  | Cons { buffer; offset; length; _ } ->
     buffer.[offset + length - 1] = '\n'

let next = function
  | Empty -> raise End_of_file
  | Cons { next = lazy next; _ } -> next

let get_char rope n = match rope with
  | Empty -> invalid_arg "Rope.get_char, empty rope"
  | Cons { buffer; offset; length; _ } ->
     if n < 0 || n >= length then invalid_arg "Rop.get_char: invalid index";
     buffer.[offset + n]

let default_size = 16384

let from_string_cont buffer length cont =
  (* assume length > 0 *)
  let rec fn offset (current:int) =
    if current >= length then
      cons buffer offset (current - offset) (lazy (cont ()))
    else
      begin
        if buffer.[current] = '\n' then
          begin
            let current = current + 1 in
            Cons { buffer; offset; length = current - offset
                   ; next = lazy (fn current current) }
          end
        else if buffer.[current] = '\255' then
          cons buffer offset (current - offset) (lazy Empty)
        else
          fn offset (current + 1)
      end
  in
  fn 0 0

let from_string s =
  from_string_cont s (String.length s) (fun () -> Empty)

let from_channel ?(close=false) ?(block_size=default_size) ch =
  let buffer = Bytes.create block_size in
  let rec gn () =
    let n = input ch buffer 0 block_size in
    if n = 0 then (if close then close_in ch; Empty)
    else
      let buffer = Bytes.unsafe_to_string buffer in
      from_string_cont buffer n gn
  in
  gn ()

let from_fd ?(close=false) ?(block_size=default_size) fd =
  let buffer = Bytes.create block_size in
  let rec gn () =
    let n = Unix.read fd buffer 0 block_size in
    if n = 0 then (if close then Unix.close fd; Empty)
    else
      let buffer = Bytes.unsafe_to_string buffer in
      from_string_cont buffer n gn
  in
  gn ()
