(* runs a full source code -> vm test *)

open Lexer
open Lexing
open Printf
open Typecheck
open Normalize
open Genir
open Codegen
open Vm

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

let translateToIrc maybeAst =
  match maybeAst with
  | Some ast ->
      let irc = translate ast in
      Some irc
  | None -> None

let codeGenToVm irc =
  match irc with
  | Some irc ->
    (match codeGen irc with
    | Some cg -> Some cg
    | None -> printf "Codegen failed.\n"; None)
  | None -> None

let runVm instr =
  match instr with
  | Some is -> run is
  | None -> run []

let printAst ast =
  match ast with
  | Some ast -> ()
  | None -> printf "Ast does not typecheck.\n"

let rec run_all lexbuf =
  try
    let prog = parse_with_error lexbuf in
    let typedAst = typecheck prog in
    let irc = translateToIrc typedAst in
    let vmCode = codeGenToVm irc in
    let _ = runVm vmCode in
    ()
  with SyntaxError _ ->
    printf "FAIL\n"

let () =
  if Array.length Sys.argv < 2
  then
    (
    printf "Usage: %s /path/to/mini/go/file\n" Sys.argv.(0);
    let _ = exit 1 in ()
    )
  else
    let filename = Sys.argv.(1) in
    let inx = open_in filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    run_all lexbuf;
    close_in inx
