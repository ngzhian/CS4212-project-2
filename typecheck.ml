open Ast
open Printf
open Str

(* the type env is a list of function signature
 * and a list of context (vars and their types) *)
type env = Env of ((string * types) list) list

let newBlock (Env lst) =
  match lst with
  | [] -> Env ([])
  | xs -> Env ([]::xs)

(* Add binding v : t to environment *)
let add v t (Env lst) =
  match lst with
  | [] -> Env ([[(v,t)]])
  | x::xs -> Env (((v,t)::x)::xs)

let initEnv () = Env []

let lu lst var =
  try
    (Some (snd (List.find (fun (el2,_) -> var = el2) lst)))
  with
  | Not_found -> None

let lookupImmd (Env lst) var = match lst with
  | [] -> None
  | []::xs -> None
  | l::xs -> lu l var

(* Lookup the type of a var in the environment *)
let rec lookup (Env lst) var =
  match lst with
  | [] -> None
  | []::xs -> lookup (Env xs) var
  | x::xs -> lu x var

let mergeEnv (Env a) (Env b) = Env (a@b)

(* Helper to get string representation of types *)
let type_to_string ty =
  match ty with
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyChan _ -> "TyChan"
  | TyFunc _ -> "TyFunc"

let rec env_to_string (Env ps) =
  let name_type ctx = (
    match ctx with
    | (a, b) -> a ^ ": " ^ (type_to_string b)) in
  let mm p = List.map (fun (l) -> (List.map name_type l
                                   |> String.concat ", " )) p in
  String.concat "; " (mm ps)

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | _ -> false

(* both types are Int return *)
let rec bothInt mt1 mt2 = match (mt1, mt2) with
  | (Some TyInt, Some TyInt) -> Some TyInt
  | _ -> None

let rec paramsMatch inferredTypes declaredTypes =
  match (inferredTypes, declaredTypes) with
  | [], [] -> true
  | (Some t1)::r1, h2::r2 -> eqTy t1 h2 && paramsMatch r1 r2
  | _ -> false

(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e =
  match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | And (a, b) -> (* both exp must be bool *)
    (match (inferTyExp env a, inferTyExp env b) with
     | (Some TyBool, Some TyBool) -> Some TyBool
     | _ -> None)
  | Eq (a, b) -> (* both exp must have same types, exp type is TyBool *)
    (match (inferTyExp env a, inferTyExp env b) with
     | (Some ta, Some tb) -> if (eqTy ta tb) then (Some TyBool) else None
     | _ -> None)
  | Gt (a, b) -> (* both exp must be int, but the exp type is TyBool *)
    (match (inferTyExp env a, inferTyExp env b) with
     | (Some TyInt, Some TyInt) -> Some TyBool
     | _ -> None)
  | Plus (a, b) -> bothInt (inferTyExp env a) (inferTyExp env b)
  | Minus (a, b) -> bothInt (inferTyExp env a) (inferTyExp env b)
  | Times (a, b) -> bothInt (inferTyExp env a) (inferTyExp env b)
  | Division (a, b) -> bothInt (inferTyExp env a) (inferTyExp env b)
  | Not a ->
    (match (inferTyExp env a) with
     | Some TyBool -> Some TyBool
     | _ -> None)
  | RcvExp a -> lookup env a
  | Var a -> lookup env a
  | FuncExp (name, ps) ->
    (match lookup env name with
     | Some (TyFunc (params, returnType)) ->
       let inferred = List.map (inferTyExp env) ps in
       if paramsMatch inferred params then Some returnType else None
     | _ -> None)

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
   use 'option' to report failure

   As discussed, there's a bit of design space when it comes to 'nested' type declarations.
   The below is just a sketch.

*)
let rec typeCheckStmt (env : env) stmt = match stmt with
  | Skip -> Some env
  | Seq (s1, s2) ->
    (match typeCheckStmt env s1 with
     | None -> None
     | Some e2 -> typeCheckStmt e2 s2)
  | Go s ->
    (match (typeCheckStmt (newBlock env) s) with
     | Some _ -> Some env
     | None -> None)
  | Transmit (ch, e) ->
    (match (lookup env ch) with
     | Some (TyChan _) ->
       (* according to tips, e should always be an int *)
       (match inferTyExp env e with
        | Some TyInt -> Some env
        | _ -> None)
     | _ -> None)
  | RcvStmt ch ->
    (match (lookup env ch) with
     | Some (TyChan t1) -> Some env
     | _ -> None)
  | Decl (v, e) ->
    (match (lookupImmd env v) with
     (* Decl only works if v was not previously declared *)
     | None -> (
         match (inferTyExp env e) with
         | Some t1 -> Some (add v t1 env)
         | _ -> None
       )
     | Some _ -> None)
  | DeclChan v ->
    Some (add v (TyChan TyInt) env)
  | Assign (v,e) ->
    (match (lookup env v) with
     | None -> None (* Unknown variable *)
     | Some t1 -> let t2 = inferTyExp env e in
       (match t2 with
        | None -> None
        | Some t3 -> if eqTy t1 t3 then Some env else None))
  | While (e, s) ->
    (match (inferTyExp env e) with
     | Some TyBool ->
       (match typeCheckStmt (newBlock env) s with
        | Some e2 -> Some env
        | None -> None)
     | _ -> None)
  | ITE (e, s1 ,s2) ->
    (match (inferTyExp env e) with
     | Some TyBool ->
       (match (typeCheckStmt (newBlock env) s1, typeCheckStmt (newBlock env) s2) with
        | (Some _, Some _) -> Some env
        | _ -> None)
     | _ -> None)
  | Return e ->
    (match inferTyExp env e with
     | None -> None
     | Some _ -> Some env)
  | FuncCall (v, ps) ->
    (match (lookup env v) with
     | Some (TyFunc (params, _)) ->
       let inferred = List.map (inferTyExp env) ps in
       if paramsMatch inferred params then Some env else None
     | _ -> None)
  | Print e -> Some env

(* TODO invalid function name *)
let collectFn env proc = match proc with
  | Proc (fn, params, ret, body) ->
    match lookup env fn with
    | Some (_) -> None (* function was already declared *)
    | _ ->
      let pt = List.map (fun (e, t) -> t) params in
      (match ret with
       | Some rt -> Some (add fn (TyFunc (pt, rt)) env)
       (* default return type to TyInt, it might be void *)
       | None -> Some (add fn (TyFunc (pt, TyInt)) env))

let rec collectFns env procs = match procs with
  | p::rest ->
    (match collectFn env p with
     | Some newEnv -> collectFns newEnv rest
     | _ -> None)
  | [] -> Some env

(* Type check a list of params in function declaration.
 * Param names cannot be repeated, cannot contain 0-9.
 * Returns an env with all function params.
 * *)
let rec typeCheckParams paramEnv params =
  let validParamName n = not (Str.string_match (Str.regexp "[0-9]") n 0) in
  match params with
  | (Var x, t)::rest ->
    (match (validParamName x, lookup paramEnv x) with
     | false, _ -> None
     (* param names cannot be repeated *)
     | true, Some _ -> None
     | true, None -> typeCheckParams (add x t paramEnv) rest)
  (* param can only be vars *)
  | _::rest -> None
  | [] -> Some paramEnv

let rec typeCheckFunctionBody env stmt rt =
  (match stmt with
   | Return x -> (
       let xt = inferTyExp env x in
       if xt = rt then Some env else None)
   | _ -> typeCheckFunctionBody env stmt rt)

(* Type checks a proc and its body.
 * Type checks all params, then type checks body with the params env
 * merged with the surrouding env.
 * Returns the original env that was passed in.
 * *)
let typeCheckProc env (Proc (fn, params, ret, body)) =
  let emptyEnv = initEnv () in
  match typeCheckParams emptyEnv params with
  | Some paramsEnv ->
    (* type check stmt with paramsEnv containing params *)
    (match typeCheckStmt (mergeEnv paramsEnv env) body with
     | Some _ -> (
         match typeCheckFunctionBody (mergeEnv paramsEnv env) body ret with
         | Some _ -> Some env
         | None -> None)
     | None -> None)
  | None -> None

(* All procs are assumed to be recursive, because the env
 * we use to typecheck contains all function definitions
 * that have already been collected.
 * *)
let rec typeCheckProcs env procs = match procs with
  | p::rest ->
    (match typeCheckProc env p with
     | Some newEnv -> typeCheckProcs newEnv rest
     | _ -> None)
  | [] -> Some env

(* Top level type checker for the entire prog *)
let typecheck (Prog (procs, stmt)) =
  match collectFns (initEnv ()) procs with
  | Some initEnv ->
    (match typeCheckProcs initEnv procs with
     | Some newEnv -> typeCheckStmt newEnv stmt
     | None -> None)
  | None -> None

(*

What's still missing are implementations for

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)
