(* File lexer.mll *)
{
open Lexing
open Parser        (* The type token is defined in parser.mli *)
exception Eof
exception SyntaxError of string
open String

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                               pos_lnum = pos.pos_lnum + 1
        }
}

let digit = ['0'-'9']
let integer = digit+
let letter = ['a'-'z']
let vars = letter (digit|letter)*
(* Technically, name can only contain letters, but we leave this
 * to the type checker in order to simplify parsing.
 * *)
let name = letter (digit|letter)*
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | white { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | integer as lxm { INT (int_of_string lxm) }
  | "true"         { TRUE(true) }
  | "false"        { FALSE(false) }
  | "=="           { EQUALS }
  | "&&"           { AND }
  | "func"         { FUNC }
  | "go"           { GO }
  | "<-"           { SEND }
  | "newChannel"   { NEWCHANNEL }
  | ":="           { DECLARE }
  | "while"        { WHILE }
  | "if"           { IF }
  | "else"         { ELSE }
  | "return"       { RETURN }
  | "print"        { PRINT }
  | "int"          { INTTYPE }
  | "bool"         { BOOLTYPE }
  | "chan int"     { CHANINTTYPE }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '>'            { GT }
  | '!'            { NOT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | ';'            { SEMICOLON }
  | ','            { COMMA }
  | '='            { ASSIGN }
  | name as lxm { NAME(lxm) }
  | _              { raise (SyntaxError("Unexpected: " ^ Lexing.lexeme lexbuf)) }
  | eof            { EOF }
