open Ast
let nameSupply = ref 1
let freshName _ =  nameSupply := !nameSupply + 1;
  String.concat "" ["temp" ; string_of_int (!nameSupply )]


let rec normalizeExp e = match e with
  | Eq (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Eq (snd r1, snd r2))
  | And (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     And (snd r1, snd r2))

  | Gt (e1, e2) ->  let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Gt (snd r1, snd r2))

  | Plus (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Plus (snd r1, snd r2))

  | Minus (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Minus (snd r1, snd r2))

  | Times (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Times (snd r1, snd r2))

  | Division (e1, e2) -> let r1 = normalizeExp e1 in
    let r2 = normalizeExp e2 in
    (Seq (fst r1, fst r2),
     Division (snd r1, snd r2))

  | Not e1            -> let r1 = normalizeExp e1 in
    (fst r1,
     Not (snd r1))

  | RcvExp ch         -> let x = freshName() in
    (Decl (Some TyInt, x, RcvExp ch),
     Var x)

  (* Introduce Skip for convenience, maybe should have
     represented a sequence of commands as a list ... *)

  | IConst i          -> (Skip, IConst i)

  | BConst b          -> (Skip, BConst b)

  | Var x              -> (Skip, Var x)

  (* Need to normalize function arguments and make sure that order remains right *)

  | FuncExp (x,es)     -> let rs = List.map normalizeExp es in
    let c  = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in
    let xs = List.map snd rs in
    let y = freshName() in
    (Seq (c, Decl (None, y, FuncExp (x,xs))),
     Var y)

let rec normalizeStmt s = match s with
  | Seq (s1,s2) -> Seq (normalizeStmt s1, normalizeStmt s2)
  | Go s        -> Go (normalizeStmt s)
  | Transmit (x,e) -> let r = normalizeExp e in
    Seq (fst r, Transmit (x, snd r))
  | RcvStmt x   -> RcvStmt x
  | Decl (t, x,e)  -> let r = normalizeExp e in
    Seq (fst r, Decl (t, x, snd r))
  | DeclChan x  -> DeclChan x
  | Assign (x,e) -> let r = normalizeExp e in
    Seq (fst r, Assign (x, snd r))
  | While (e, ls, s)  -> let r = normalizeExp e in
    Seq (fst r, While (snd r, ls, normalizeStmt s))
  | ITE (e, l1, s1, l2, s2) -> let r = normalizeExp e in
    Seq (fst r, ITE (snd r, l1, normalizeStmt s1, l2, normalizeStmt s2))
  | Return e      -> let r = normalizeExp e in
    Seq (fst r, Return (snd r))
  | FuncCall (x, es) -> let rs = List.map normalizeExp es in
    let c = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in
    let xs = List.map snd rs in
    Seq (c, FuncCall (x,xs))
  | Print e       -> let r = normalizeExp e in
    Seq (fst r, Print (snd r))
  | Skip          -> Skip


let normalizeProc p = match p with
    Proc (x, args, tys, l, s) -> Proc (x, args, tys, l, normalizeStmt s)


let normalizeProg p = match p with
    Prog (ps, l, s) -> Prog (List.map normalizeProc ps, l, normalizeStmt s)
