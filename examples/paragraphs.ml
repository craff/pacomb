open Pacomb

let%parser word =
    (w::RE"[-a-zA-Z0-9']+") => w
  ; (p::RE"[.;,:!?]") => p

(* blank with at most one newline for paragraphs *)
let blank1 = Regexp.blank_regexp "[ \t\r]*\n?[ \t\r]*"

(* general blank, no newline, we parse them as separator. *)
let blank2 = Regexp.blank_regexp "[ \t\r]*"

(* for paragraph, we use [Grammar.layout] to change the blank from [blank2] to
   [blank1] and we parse with blank1 after the paragraph to read the last
   newline at the end of each paragraph, the default would be to parse with
   [blank2] only after the paragraph. This way, the minimum number of newline left to
   parse as paragraph separation is 1.  *)
let%parser
      [@layout blank1 ~config:Blank.{ default_layout_config with
                                      new_blanks_after = true
                                    ; old_blanks_after = false }]
      paragraph = ((p:: ~+ word) => (p, p_pos))

(* paragraph separator, at least one newline *)
let%parser sep = (~+ (CHAR('\n'))) => ()

(* text are list separated by sep and may have initial and final newlines *)
let%parser text = (~? sep) (t:: ~* [sep] paragraph) (~? sep) => t

(* we call the parser *)
let _ =
  let t = Pos.handle_exception (Grammar.parse_channel text blank2) stdin in
  Printf.printf "%d paragraphs\n%!" (List.length t);
  List.iteri (fun i (p,pos) -> Printf.printf "  paragraph %d at %a: %d word(s)\n%!"
                                 i (Pos.print_interval ~style:Short ()) pos (List.length p)) t
