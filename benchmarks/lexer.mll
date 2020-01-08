{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}
rule token = parse
  | 'a'            { A }
  | eof            { EOF }
