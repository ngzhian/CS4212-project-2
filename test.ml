open Lexer
open Lexing
open Printf

exception Error of string

let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname
            pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
    try Parser.main Lexer.token lexbuf with
        | SyntaxError msg ->
                fprintf stderr "%a: %s\n" print_position lexbuf msg;
                        raise (Error msg)
        | Parsing.Parse_error ->
                fprintf stderr "%a: syntax error\n" print_position lexbuf;
                                          exit (-1)

let rec parse_and_print lexbuf =
    try
        parse_with_error lexbuf;
        printf "SUCCESS\n"
    with SyntaxError _ ->
        printf "FAIL\n"

let loop filename () =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

(* part 2 *)
let () =
    loop Sys.argv.(1) ()
