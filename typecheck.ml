open Ast
open Printf
open Str

(* the type env is a list of function signature
 * and a list of context (vars and their types) *)
type env = Env of (string * types) list

(* Helper to get string representation of types *)
let type_to_string ty =
    match ty with
    | TyInt -> "TyInt"
    | TyBool -> "TyBool"
    | TyChan _ -> "TyChan"
    | TyFunc _ -> "TyFunc"

let to_string l =
    List.map (fun (a, b) -> a ^ ": " ^ (type_to_string b)) l
    |> String.concat ", "
let rec env_to_string env =
    match env with
    | Env ps -> to_string ps

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
    | Some t1::r1, h2::r2 -> eqTy t1 h2 && paramsMatch r1 r2
    | _ -> false

(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el env =
    match env with
    | Env lst ->
            try (Some (snd (List.find (fun (el2,_) -> el = el2) lst)))
            with
            | Not_found -> None
let add v t env =
    match env with
    | Env lst -> Env ((v, t) :: lst)

(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e =
    match e with
    | IConst i -> Some TyInt
    | BConst b -> Some TyBool
    | And (a, b) -> (* both exp must be bool *)
        (match (inferTyExp env a, inferTyExp env b) with
         | (Some TyBool, Some TyBool) -> Some TyBool
         | _ -> None)
    | Eq (a, b) -> (* both exp must have same types *)
        (match (inferTyExp env a, inferTyExp env b) with
         | (Some ta, Some tb) -> if (eqTy ta tb) then (Some TyBool) else None
         | _ -> None)
    | Gt (a, b) ->
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
    | RcvExp a -> lookup a env
    | Var a -> lookup a env
    | FuncExp (n, ps) ->
        (match lookup n env with
         | Some (TyFunc (params, returnType)) ->
                 let inferred = List.map (inferTyExp env) ps in
                 if paramsMatch inferred params then Some returnType else None
         | _ -> None)


  (* add remaining cases *)               

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure 

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec typeCheckStmt (env : env) stmt = match stmt with
  | Seq (s1, s2) ->
      (match typeCheckStmt env s1 with
       | None -> None
       | Some e2 -> typeCheckStmt e2 s2)
  | Go s ->
      (match (typeCheckStmt env s) with
       | Some _ -> Some env
       | None -> None)
  | Transmit (ch, e) ->
      (match (lookup ch env) with
       | Some (TyChan _) ->
         (* according to tips, e should always be an int *)
         (match inferTyExp env e with
          | Some TyInt -> Some env
          | _ -> None)
       | _ -> None)
  | RcvStmt ch ->
      (match (lookup ch env) with
       | Some (TyChan t1) -> Some env
       | _ -> None)
  | Decl (v, e) ->
      (match (lookup v env) with
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
      (match (lookup v env) with
       | None -> None (* Unknown variable *)
       | Some t1 -> let t2 = inferTyExp env e in
           (match t2 with
            | None -> None
            | Some t3 -> if eqTy t1 t3 then Some env else None))
  | While (e, s) ->
      (match (inferTyExp env e) with
       | Some TyBool ->
           (match typeCheckStmt env s with
            | Some e2 -> Some env
            | None -> None)
       | _ -> None)
  | ITE (e, s1 ,s2) ->
      (match (inferTyExp env e) with
       | Some TyBool ->
           (match (typeCheckStmt env s1, typeCheckStmt env s2) with
             | (Some _, Some _) -> Some env
             | _ -> None)
       | _ -> None)
  | Return e -> Some env
  | FuncCall (v, ps) ->
      (match (lookup v env) with
         | Some (TyFunc (params, _)) ->
                 let inferred = List.map (inferTyExp env) ps in
                 if paramsMatch inferred params then Some env else None
         | _ -> None)
  | Print e -> Some env

(* disallow funcs with same name? *)
let collectFn env proc = match proc with
  | Proc (fn, params, Some ret, body) -> (
      let pt = List.map (fun (e, t) -> t) params in
      Some ((fn, TyFunc (pt, ret)) :: env))
  | Proc (fn, params, None, body) -> (
      let pt = List.map (fun (e, t) -> t) params in
      (* default return type to TyInt, it might be void *)
      Some ((fn, TyFunc (pt, TyInt)) :: env))

let rec collectFns env procs = match procs with
  | p::rest ->
      (match collectFn env p with
       | Some newEnv -> collectFns newEnv rest
       | _ -> None)
  | [] -> Some env

(* Type checks a function declaration param.
 * Ensures that param names are names (no digits).
 * *)
let typeCheckParam env param = match param with
  | (Var x, t) -> if Str.string_match (Str.regexp "[0-9]") x 0 then None else Some env
  | _ -> Some env

(* Type check a list of params in function declaration *)
let rec typeCheckParams env params = match params with
  | p::rest ->
      (match typeCheckParam env p with
       | Some newEnv -> typeCheckParams newEnv rest
       | None -> None)
  | [] -> Some env

(* Type checks a proc and its body.
 * Collects all params into a new env, then type checks body with new env.
 * The env returns is the original env that was passed in with this
 * function definition added.
 * *)
let typeCheckProc env proc = match proc with
  (* need to type check return value *)
  (* are functions recursive? *)
  | Proc (fn, params, ret, body) ->
      (* check that param names are valid names *)
      (match typeCheckParams env params with
       | Some newEnv ->
           (match typeCheckStmt newEnv body with
            | Some _ -> Some (add fn (TyFunc (List.map (fun (e, t) -> t) params, TyInt)) env)
            | None -> None)
       | None -> None)

let rec typeCheckProcs env procs = match procs with
  | p::rest ->
      (match typeCheckProc env p with
       | Some newEnv -> typeCheckProcs newEnv rest
       | _ -> None)
  | [] -> Some env

let typecheck prog = match prog with
  | Prog (procs, stmt) ->
      (match typeCheckProcs (Env []) procs with
       | Some newEnv -> typeCheckStmt newEnv stmt
       | None -> None)

(*

What's still missing are implementations for 

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)   
