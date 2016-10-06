(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
open String
}
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['a'-'z'] as lxm { LETTER lxm }
  | '+'            { PLUS }
  | '*'            { STAR }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }
