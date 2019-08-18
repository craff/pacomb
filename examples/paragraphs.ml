open Pacomb

let%parser word =
    (w::RE"[-a-zA-Z0-9']+") => w
  ; (p::RE"[.;,:!?]") => p

(* blank with at most one newline for paragraphs *)

let blank1 = Lex.blank_regexp "[ \t\r]*\n?[ \t\r]*"

(* general blank, no newline, we parse them. *)
let blank2 = Lex.blank_regexp "[ \t\r]*"

(* paragraph separator, at least one newline, or EOF *)
(* not: ~+ in pervasives forces __:: ?, strange*)
let%parser sep = (__:: ~+ '\n') => () ; EOF => ()

(* for paragraph, we change the blank from blank2 to blank1
   and we parse with blank1 after to read the first newline if any,
   the default would be to parse with blank2 only *)
let%parser paragraph = Grammar.layout
                         ~config:Lex.{ default_layout_config with
                           new_blanks_after = true
                         ; old_blanks_after = false }
                         blank1
                         ((p:: ~+ word) sep => p)

(* test may have initial newlines *)
let%parser text = (~? sep) (t:: ~* paragraph) => t

(* we call the parser *)
let _ =
  let t = Pos.handle_exception (Grammar.parse_channel text blank2) stdin in
  Printf.printf "%d paragraphs\n%!" (List.length t);
  List.iteri (fun i p -> Printf.printf "  paragraph %d: %d word(s)\n%!" i (List.length p)) t
