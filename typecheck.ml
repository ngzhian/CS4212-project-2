open Ast
open Printf
open Str

(* the type env is a list of function signature
 * and a list of context (vars and their types) *)
type env = Env of ((string * types) list) list

let new_block (Env lst) =
  match lst with
  | [] -> Env ([])
  | xs -> Env ([]::xs)

(* Add binding v : t to environment *)
let add v t (Env lst) =
  match lst with
  | [] -> Env ([[(v,t)]])
  | x::xs -> Env (((v,t)::x)::xs)

let init_env () = Env []

let lu lst var =
  try
    (Some (snd (List.find (fun (el2,_) -> var = el2) lst)))
  with
  | Not_found -> None

(* Lookup the type of a var only in the immediate environment *)
let lookup_immd (Env lst) var = match lst with
  | [] -> None
  | []::xs -> None
  | l::xs -> lu l var

(* Lookup the type of a var in the environment *)
let rec lookup (Env lst) var =
  match lst with
  | [] -> None
  | []::xs -> lookup (Env xs) var
  | x::xs -> lu x var

let merge_env (Env a) (Env b) = Env (a@b)

(* equality among types *)
let rec eq_ty t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eq_ty t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) ->
    let eqTypes l1 l2 =
      (List.length l1 == List.length l2) &&
      (List.for_all (fun (t1,t2) -> eq_ty t1 t2) (List.combine l1 l2)) in
    (match (t1, t2) with
     | None, Some _ | Some _, None -> false
     | Some t1', Some t2' -> eq_ty t1' t2' && eqTypes ts1 ts2
     | None, None -> eqTypes ts1 ts2)
  | _ -> false

(* both types are Int return *)
let rec both_int mt1 mt2 = match (mt1, mt2) with
  | (Some TyInt, Some TyInt) -> Some TyInt
  | _ -> None

let rec params_match inferredTypes declaredTypes =
  match (inferredTypes, declaredTypes) with
  | [], [] -> true
  | (Some t1)::r1, h2::r2 -> eq_ty t1 h2 && params_match r1 r2
  | _ -> false

(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec infer_ty_exp env e =
  match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | And (a, b) -> (* both exp must be bool *)
    (match (infer_ty_exp env a, infer_ty_exp env b) with
     | (Some TyBool, Some TyBool) -> Some TyBool
     | _ -> None)
  | Eq (a, b) -> (* both exp must have same types, exp type is TyBool *)
    (match (infer_ty_exp env a, infer_ty_exp env b) with
     | (Some ta, Some tb) -> if (eq_ty ta tb) then (Some TyBool) else None
     | _ -> None)
  | Gt (a, b) -> (* both exp must be int, but the exp type is TyBool *)
    (match (infer_ty_exp env a, infer_ty_exp env b) with
     | (Some TyInt, Some TyInt) -> Some TyBool
     | _ -> None)
  | Plus (a, b) -> both_int (infer_ty_exp env a) (infer_ty_exp env b)
  | Minus (a, b) -> both_int (infer_ty_exp env a) (infer_ty_exp env b)
  | Times (a, b) -> both_int (infer_ty_exp env a) (infer_ty_exp env b)
  | Division (a, b) -> both_int (infer_ty_exp env a) (infer_ty_exp env b)
  | Not a ->
    (match (infer_ty_exp env a) with
     | Some TyBool -> Some TyBool
     | _ -> None)
  | RcvExp a -> lookup env a
  | Var a -> lookup env a
  | FuncExp (name, ps) ->
    (match lookup env name with
     | Some (TyFunc (params, returnType)) ->
       let inferred = List.map (infer_ty_exp env) ps in
       if params_match inferred params then returnType else None
     | _ -> None)

let merge_locals l1 l2 = match l1, l2 with
  | Locals l1', Locals l2' ->
     Locals (l1' @ l2')

let rec collect_locals stmt = match stmt with
  | Seq (s1, s2) -> merge_locals (collect_locals s1) (collect_locals s2)
  | Go s -> collect_locals s
  | While (e, _, st) -> collect_locals st
  | ITE (e, _, sl, _, sr) -> merge_locals (collect_locals sl) (collect_locals sr)
  | Decl (None, v, e) -> Locals []
  | Decl (Some ty, v, e) -> Locals [(v, ty)]
  | DeclChan c -> Locals [(c, TyInt)]
  | _ -> Locals []

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
   use 'option' to report failure
*)
let rec typecheck_stmt (env : env) stmt locals = match stmt with
  | Skip -> Some (env, stmt)
  | Seq (s1, s2) ->
    (match typecheck_stmt env s1 locals with
     | None -> None
     | Some (e2, st) ->
       (match typecheck_stmt e2 s2 locals with
        | None -> None
        | Some (e3, st') -> Some (env, Seq (st, st'))))
  | Go s ->
    (match (typecheck_stmt (new_block env) s locals) with
     | Some (_, s') -> Some (env, s')
     | None -> None)
  | Transmit (ch, e) as st ->
    (match (lookup env ch) with
     | Some (TyChan _) ->
       (* according to tips, e should always be an int *)
       (match infer_ty_exp env e with
        | Some TyInt -> Some (env, st)
        | _ -> None)
     | _ -> None)
  | RcvStmt ch as st ->
    (match (lookup env ch) with
     | Some (TyChan t1) -> Some (env, st)
     | _ -> None)
  | Decl (t, v, e) ->
    (match (lookup_immd env v) with
     (* TODO allow nested re-decls in inner *)
     (* Decl only works if v was not previously declared *)
     | None -> (
         match (infer_ty_exp env e) with
         | Some t1 -> Some (add v t1 env, Decl ((Some t1, v, e)))
         | _ -> None
       )
     | Some _ -> None)
  | DeclChan v as st ->
    Some (add v (TyChan TyInt) env, st)
  | Assign (v,e) as st ->
    (match (lookup env v) with
     | None -> None (* Unknown variable *)
     | Some t1 -> let t2 = infer_ty_exp env e in
       (match t2 with
        | None -> None
        | Some t3 -> if eq_ty t1 t3 then Some (env, st) else None))
  | While (e, _, s) as st ->
    (match (infer_ty_exp env e) with
     | Some TyBool ->
       (match typecheck_stmt (new_block env) s locals with
        | Some (_, s') -> Some (env, While (e, collect_locals st, s'))
        | None -> None)
     | _ -> None)
  | ITE (e, _, s1, _, s2) ->
    (match (infer_ty_exp env e) with
     | Some TyBool ->
       (let tcl = typecheck_stmt (new_block env) s1 locals in
        let tcr = typecheck_stmt (new_block env) s2 locals in
        match tcl, tcr with
        | (Some (_, s1'), Some (_, s2')) ->
            Some (env, ITE (e, collect_locals s1, s1', collect_locals s2, s2'))
        | _ -> None)
     | _ -> None)
  | Return e as st ->
    (match infer_ty_exp env e with
     | None -> None
     | Some _ -> Some (env, st))
  | FuncCall (v, ps) as st ->
    (match (lookup env v) with
     | Some (TyFunc (params, _)) ->
       let inferred = List.map (infer_ty_exp env) ps in
       if params_match inferred params then Some (env, st) else None
     | _ -> None)
  | Print e as st -> Some (env, st)

(* TODO invalid function name *)
let collect_fn env proc = match proc with
  | Proc (fn, params, ret, locals, body) ->
    match lookup env fn with
    | Some (_) -> None (* function was already declared *)
    | _ ->
      let pt = List.map (fun (e, t) -> t) params in
      Some (add fn (TyFunc (pt, ret)) env)

let rec collect_fns env procs = match procs with
  | p::rest ->
    (match collect_fn env p with
     | Some newEnv -> collect_fns newEnv rest
     | _ -> None)
  | [] -> Some env

(* Type check a list of params in function declaration.
 * Param names cannot be repeated, cannot contain 0-9.
 * Returns an env with all function params.
 * *)
let rec typecheck_params paramEnv params =
  let validParamName n = not (Str.string_match (Str.regexp "[0-9]") n 0) in
  match params with
  | (Var x, t)::rest ->
    (match (validParamName x, lookup paramEnv x) with
     | false, _ -> None
     (* param names cannot be repeated *)
     | true, Some _ -> None
     | true, None -> typecheck_params (add x t paramEnv) rest)
  (* param can only be vars *)
  | _::rest -> None
  | [] -> Some paramEnv

let rec typecheck_fn_body env stmt rt =
  (match stmt with
   | Seq (l, r) ->
       (match typecheck_fn_body env l rt with
        | Some e' -> typecheck_fn_body e' r rt
        | None -> None)
   | Go g -> typecheck_fn_body env g rt
   | While (_, _, s) -> typecheck_fn_body env s rt
   | ITE (_, _, l, _, r) ->
       (match typecheck_fn_body env l rt with
        | Some e' -> typecheck_fn_body e' r rt
        | None -> None)
   | Transmit _ | RcvStmt _ | FuncCall _ | Skip
   | Decl _ | DeclChan _ | Assign _ | Print _ -> Some env
   | Return x -> (
       let xt = infer_ty_exp env x in
       if xt = rt then Some env else None)
   )

(* Type checks a proc and its body.
 * Type checks all params, then type checks body with the params env
 * merged with the surrouding env.
 * Returns the original env that was passed in.
 * *)
let typecheck_proc env proc =
  match proc with
  | (Proc (fn, params, ret, locals, body)) ->
    (let emptyEnv = init_env () in
     match typecheck_params emptyEnv params with
     | Some paramsEnv ->
       (* type check stmt with paramsEnv containing params *)
       (match typecheck_stmt (merge_env paramsEnv env) body locals with
        | Some (_, body') -> (
            match typecheck_fn_body (merge_env paramsEnv env) body' ret with
            | Some _ ->
                Some (env, Proc(fn, params, ret, collect_locals body', body'))
            | None -> None)
        | None -> None)
     | None -> None)

(* All procs are assumed to be recursive, because the env
 * we use to typecheck contains all function definitions
 * that have already been collected.
 * *)
let rec typecheck_procs env procs = match procs with
  | p::rest ->
    (match typecheck_proc env p with
     | Some (newEnv, st) ->
       (match typecheck_procs newEnv rest with
        | Some (e', st') -> Some (e', st::st')
        | None -> None)
     | None -> None)
  | [] -> Some (env, [])

(* Top level type checker for the entire prog *)
(* might want to consider threading the locals instead of doing collecting at the top level *)
let typecheck (Prog (procs, locals, stmt)) =
  match collect_fns (init_env ()) procs with
  | Some init_env ->
    (match typecheck_procs init_env procs with
     | Some (newEnv, st) ->
       (match typecheck_stmt newEnv stmt (Locals []) with
        | Some (_, st') -> Some (Prog (st, collect_locals st', st'))
        | None -> None)
     | None -> None)
  | None -> None
