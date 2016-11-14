(*
 * Definition and functions related to env
 * *)
open Ast
open Ast_print

(* the type env is a list of function signature
 * and a list of context (vars and their types) *)
type env = Env of ((string * types) list) list

let lu lst var =
  try
    (Some (snd (List.find (fun (el2,_) -> var = el2) lst)))
  with
  | Not_found -> None

let rec env_to_string (Env ps) =
  List.map
  (fun s -> (
    String.concat "," (List.map (fun (s, ty) -> s ^ (to_string_type ty)) s))) ps
  |> String.concat "|"

(* enters a new scope *)
let new_block (Env lst) =
  match lst with
  | [] -> Env ([])
  | xs -> Env ([]::xs)

(* Add binding v : t to environment *)
let add v t (Env lst) =
  match lst with
  | [] -> Env ([[(v,t)]])
  | x::xs -> Env (((v,t)::x)::xs)

let add_locals_to_env (Locals l) env =
 List.fold_left (fun env (n, ty) -> add n ty env) (new_block env) l

let init_env () = Env []

(* Lookup the type of a var only in the immediate environment *)
let lookup_immd (Env lst) var = match lst with
  | [] -> None
  | []::xs -> None
  | l::xs -> lu l var

(* Lookup the type of a var in the environment *)
let rec lookup (Env lst) var =
  match lst with
  | [] -> None
  | x::xs ->
    (match lu x var with
    | None -> lookup (Env xs) var
    | result -> result)

let merge_env (Env a) (Env b) = Env (a@b)
