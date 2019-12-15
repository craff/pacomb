open Pacomb
open Grammar

(* Blank function *)
let blank = Regexp.blank_regexp "\\(\\([#][^\n]*\\)\\|[ \r\t\026]+\\)*"
   (* bug: "\\([ \r\t\026]\\|\\(\\(#[^\n]*\\)\\)*" *)

(* Parser for hexadecimal integers *)
let%parser char =
  (i::RE"[0-9A-Fa-F]+") => Uchar.of_int (int_of_string ("0x" ^ i))

let%parser sep = "÷" => true ; "×" => false

let%parser rec sample_aux = (l::sample_aux) (c::char) (s::sep) => (c,s) :: l
                          ; (c::char) (s::sep)                 => [(c,s)]

let%parser sample = sep (l::sample_aux) => List.rev l

let%parser rec break =                   () => []
                 ; (g::GRAPHEME) (l::break) => g::l

let good = ref true

let test pos l0 =
  try
    let chars = List.map fst l0 in
    let s = Utf8.of_list chars in
    let rec fn = function
      | [] -> []
      | []::_ -> Printf.eprintf "unexpected empty at %a\n%!"
                   (Pos.print_pos ()) pos; good := false; raise Exit
      | [x]::l -> (x,true)::fn l
      | (x::l1)::l -> (x,false)::fn (l1::l)
    in
    let l = parse_string ~utf8:Utf8.UTF8 break Blank.none s in
    let l = List.map (fun s -> Utf8.to_list s) l in
    let l = fn l in
    if l <> l0 then
      begin
        Printf.eprintf "break fail at %a\n%!"
          (Pos.print_pos ()) pos;
        List.iter (fun (l,b) -> Printf.eprintf "%x %b " (Uchar.to_int l) b) l;
        Printf.eprintf "\n%!";
        good := false;
        raise Exit
      end
  with Exit -> ()

  (* Single mapping parser *)
let%parser test = (l::sample) (~* '\n' => ()) => test l_lpos l

let%parser tests =
  (star ('\n' => ())) (star test) => ()

let parse = parse_channel ~utf8:Utf8.UTF8 tests blank

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 2 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <GraphemeBreakTest.txt>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in

  (* Parsing and preparing the data *)
  let infile = open_in infile in
  let _ = Pos.handle_exception parse infile in

  close_in infile;
  if not !good then exit 1
