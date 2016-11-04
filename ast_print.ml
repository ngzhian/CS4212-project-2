open Ast
open Printf

let rec to_string_type types =
  match types with
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyChan ty -> "TyChan"
  | TyFunc (params, return)-> "TyFunc"

let to_string_locals locals =
  match locals with
  | Locals l ->
      (List.map (fun (s, t) -> s ^ ":" ^ (to_string_type t)) l
      |> String.concat ", ")

let rec to_string_stmt stmt =
  match stmt with
  | Seq (a, b) -> to_string_stmt a ^ ";" ^ to_string_stmt b
  | Go a -> "Go"
  | Transmit (name , exp) -> "Transmit"
  | RcvStmt (name) -> "RcvStmt"
  | Decl (None, name, exp) -> "Decl " ^ name ^ ":" ^ "void"
  | Decl (Some ty, name, exp) -> "Decl " ^ name ^ ":" ^ (to_string_type ty)
  | DeclChan (name) -> "DeclChan "
  | Assign (name, exp) -> "Assign "
  | While (exp, l, b) -> sprintf "w {%s} [%s] " (to_string_stmt b) (to_string_locals l)
  | ITE (exp, l, b, r, c) -> sprintf "if [%s] [%s]" (to_string_locals l) (to_string_locals r)
  | Return (exp) -> "Return "
  | FuncCall (name, params) -> "FuncCall "
  | Print (exp) -> "Print "
  | Skip -> ""

let rec to_string_exp exp =
  match exp with
  | And (a, b) -> "And "
  | Eq (a, b) -> "Eq "
  | Gt (a, b) -> "Gt "
  | Plus (a, b) -> "Plus "
  | Minus (a, b) -> "Minus "
  | Times (a, b) -> "Times "
  | Division (a, b) -> "Division "
  | Not (a) -> "Not "
  | RcvExp (name) -> "RcvExp "
  | IConst (name) -> "IConst "
  | BConst (name) -> "BConst "
  | Var (name) -> "Var "
  | FuncExp (name, params) -> "FuncExp "


let rec to_string_proc proc =
  match proc with
  | Proc (name, params, types, ls, stmt) ->
      let p' = List.map (fun (e, t) -> to_string_exp e) params in
      sprintf "%s(%s) { %s } [%s]" name (String.concat ", " p') (to_string_stmt stmt) (to_string_locals ls)

let to_string_prog ast =
  match ast with
  | Prog (procs, stmt) ->
      "Prog\n  " ^ (String.concat "\n  " (List.map to_string_proc procs)) ^ (to_string_stmt stmt)
