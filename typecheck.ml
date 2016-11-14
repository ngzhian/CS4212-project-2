open Ast
open Str
open Ast_print
open Env

let (>>=) m f = match m with
| Some x -> f x
| None -> None

module RNMap = Map.Make(String);;
let empty_renames = RNMap.empty

(* tries to get a new name for a variable that does not occur in env
 * x0, x1, ...
 * *)
let new_name_env var env =
  let x = ref 0 in
  let rec gen var =
    match lookup env var with
    | None -> var
    | Some _ -> x := !x + 1; gen (var ^ (string_of_int !x))
  in
  let result = gen var in
  result

let rename_s s renames =
  if RNMap.mem s renames
  then (RNMap.find s renames)
  else s

let rename_exp exp renames =
  let rec rn_exp exp =
    match exp with
    | And (l, r) -> And (rn_exp l, rn_exp r)
    | Eq (l, r) -> Eq (rn_exp l, rn_exp r)
    | Gt (l, r) -> Gt (rn_exp l, rn_exp r)
    | Plus (l, r) -> Plus (rn_exp l, rn_exp r)
    | Minus (l, r) -> Minus (rn_exp l, rn_exp r)
    | Times (l, r) -> Times (rn_exp l, rn_exp r)
    | Division (l, r) -> Division (rn_exp l, rn_exp r)
    | Not e -> Not (rn_exp e)
    | Var x -> Var (rename_s x renames)
    | RcvExp x -> RcvExp (rename_s x renames)
    | e -> e
  in
  rn_exp exp

(* equality among types *)
let rec eq_ty t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eq_ty t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) ->
    let eq_types l1 l2 =
      (List.length l1 == List.length l2) &&
      (List.for_all (fun (t1,t2) -> eq_ty t1 t2) (List.combine l1 l2)) in
    (match (t1, t2) with
     | None, Some _ | Some _, None -> false
     | Some t1', Some t2' -> eq_ty t1' t2' && eq_types ts1 ts2
     | None, None -> eq_types ts1 ts2)
  | _ -> false

(* both types are Int return *)
let rec both_int mt1 mt2 = match (mt1, mt2) with
  | (Some TyInt, Some TyInt) -> Some TyInt
  | _ -> None

(* check that the inferred params types match the declared params types in func def *)
let rec params_match inferred declared =
  match (inferred, declared) with
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

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
   use 'option' to report failure
*)
let rec typecheck_stmt (env : env) stmt locals renames = match stmt with
  | Skip -> Some (env, stmt, locals, renames)
  | Seq (s1, s2) ->
    (match typecheck_stmt env s1 locals renames with
     | None -> None
     | Some (e2, st, l', rn') ->
       (match typecheck_stmt e2 s2 l' rn' with
        | None -> None
        | Some (e3, st', l'', rn'') -> Some (env, Seq (st, st'), l'', rn'')))
  | Go s ->
    (match (typecheck_stmt (new_block env) s locals renames) with
     | Some (_, s', l', _) -> Some (env, s', l', renames)
     | None -> None)
  | Transmit (ch, e) ->
    (match (lookup env ch) with
     | Some (TyChan _) ->
       (* according to tips, e should always be an int *)
       (match infer_ty_exp env e with
        | Some TyInt -> Some (env, Transmit (rename_s ch renames, rename_exp e renames), locals, renames)
        | _ -> None)
     | _ -> None)
  | RcvStmt ch ->
    (match (lookup env ch) with
     | Some (TyChan t1) -> Some (env, RcvStmt (rename_s ch renames), locals, renames)
     | _ -> None)
  | Decl (_, v, e) ->
    (* allow redeclaration when moving into nested block *)
    (match (lookup_immd env v) with
     | None -> (
       (* if this name is used somewhere in the env, need to rename *)
       let v', rn' =
         (match (lookup env v) with
          | None -> v, renames
          | Some ty -> let v' = new_name_env v env in (v', RNMap.add v v' renames)) in
       match (infer_ty_exp env e) with
       | Some ty ->
           let env' = add v' ty env in
           let locals' = add_local v' ty locals in
           (* should not rename vars in decl, e.g. x := 1 + x *)
           let decl = Decl (Some ty, v', e) in
           Some (env', decl, locals', rn')
       | _ -> None)
     | Some _ -> None)
  | DeclChan v as st ->
    Some (add v (TyChan TyInt) env, st, add_local v (TyChan TyInt) locals, renames)
  | Assign (v,e) ->
    (match (lookup env v) with
     | None -> None (* Unknown variable *)
     | Some t1 -> let t2 = infer_ty_exp env e in
       (match t2 with
        | None -> None
        | Some t3 ->
            if eq_ty t1 t3
            then Some (env, Assign(rename_s v renames, rename_exp e renames), locals, renames)
            else None))
  | While (e, _, s) ->
    (match (infer_ty_exp env e) with
     | Some TyBool ->
       (match typecheck_stmt (new_block env) s locals renames with
        | Some (_, s', l', _) -> Some (env, While (rename_exp e renames, l', s'), l', renames)
        | None -> None)
     | _ -> None)
  | ITE (e, _, s1, _, s2) ->
    (* note the locals in the 2 clauses are collected *)
    (match (infer_ty_exp env e) with
     | Some TyBool ->
         (match typecheck_stmt (new_block env) s1 locals renames with
         | None -> None
         | Some (_, s1', l', _) ->
             (match typecheck_stmt (new_block env) s2 l' renames with
             | None -> None
             | Some (_, s2', l'', _)->
                 Some (env, ITE (rename_exp e renames, l', s1', l'', s2'), l'', renames)))
     | _ -> None)
  | Return e ->
    (match infer_ty_exp env e with
     | None -> None
     | Some _ -> Some (env, Return (rename_exp e renames), locals, renames))
  | FuncCall (v, ps) as st ->
    (match (lookup env v) with
     | Some (TyFunc (params, _)) ->
       let inferred = List.map (infer_ty_exp env) ps in
       if params_match inferred params then Some (env, st, locals, renames) else None
     | _ -> None)
  | Print e -> Some (env, Print (rename_exp e renames), locals, renames)

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
  | (Proc (fn, params, ret, _, body)) ->
    (let emptyEnv = init_env () in
     match typecheck_params emptyEnv params with
     | Some paramsEnv ->
       (* type check stmt with paramsEnv containing params *)
       (match typecheck_stmt (new_block (merge_env paramsEnv env)) body (Locals []) empty_renames with
        | Some (env', body', l', _) ->
            (* env with the locals merged in *)
            let new_env = add_locals_to_env l' env' in
            (match typecheck_fn_body new_env body' ret with
            | Some _ -> Some (env, Proc(fn, params, ret, l', body'))
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

let (>>=) a b = match a with
| Some x -> (b x)
| None -> None

(* Top level type checker for the entire prog *)
let typecheck (Prog (procs, _, stmt)) =
  match collect_fns (init_env ()) procs with
  | Some init_env ->
    (match typecheck_procs init_env procs with
     | Some (newEnv, ty_procs) ->
       (match typecheck_stmt (new_block newEnv) stmt (Locals []) empty_renames with
        | Some (_, ty_stmt, ls, _) -> Some (Prog (ty_procs, ls, ty_stmt))
        | None -> None)
     | None -> None)
  | None -> None
