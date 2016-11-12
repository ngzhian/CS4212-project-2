open Ast
open Printf
open Irc

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
  !labelSupply
let freshName _ =  labelSupply := !labelSupply + 1;
  String.concat "" ["genirc" ; string_of_int (!labelSupply )]

let currentLocals = ref []
let localNames l = match l with
  | Locals s -> List.map (fun (x, _) -> x) s

type symtable = SymTable of (string * int) list

let symtable = SymTable []

let lookup name =
  let rec lu name t =
    match t with
    | SymTable [] -> failwith "symbol not found"
    | SymTable ((n, l)::ss) -> if n = name then l else lu name (SymTable ss)
  in lu name symtable

let insertSym name label =
  match symtable with
    | SymTable ss -> SymTable ((name, label)::ss)

(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)
let rec translateB exp = match exp with
  | And (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    let l1 = freshLabel() in
    let l2 = freshLabel() in
    (code1
     @ (irc_ZeroJump (place1,l1))
     @ code2
     @ [IRC_Assign (x, IRC_Var place2);
        IRC_Goto l2 ]
     @
     [IRC_Label l1;
      IRC_Assign (x, IRC_IConst 0);
      IRC_Label l2],
     x)
  | Eq (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Eq (place1, place2))]
    , x)
  | Gt (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Gt (place1, place2))]
    , x)
  | Plus (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Plus (place1, place2))]
    , x)
  | Minus (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Minus (place1, place2))]
    , x)
  | Times (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Times (place1, place2))]
    , x)
  | Division (e1, e2) ->
    let code1, place1 = translateB e1 in
    let code2, place2 = translateB e2 in
    let x = freshName() in
    ( code1
      @ code2
      @ [IRC_Assign (x, IRC_Division (place1, place2))]
    , x)
  | Not e ->
    let x = freshName() in
    let code, place = translateB e in
    ( code
      @ [IRC_Assign (x, IRC_Not place)]
    , x)
  | IConst i -> let x = freshName() in
    ([IRC_Assign (x, IRC_IConst i)], x)
  | BConst true -> let x = freshName() in
    ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
    ([IRC_Assign (x, IRC_IConst 0)], x)
  | Var v ->
      let x = freshName() in
    ([IRC_Assign (x, IRC_Local(v, !currentLocals))], x)
  | FuncExp (f, ps) ->
    let x = freshName() in
    let n = List.length ps in
    let t' = List.map translateB ps in
    let places = List.map (fun t -> [IRC_Param (snd t)]) t' in
    let codes = List.map fst t' in
    let placesgen = List.fold_left (@) [] places in
    let codesgen = List.fold_left (@) [] codes in
    ( codesgen
      @ placesgen
      @ [IRC_Call (lookup f, n)] (* todo, fill this in with something *)
      @ [IRC_Get x], x)
  | RcvExp _ -> ([],"")

let rec translateStmt stmt = match stmt with
  | Seq (s1, s2) ->
      let c1 = translateStmt s1 in
      let c2 = translateStmt s2 in
      (c1 @ c2)
  | Go _ -> ([])
  | Transmit _ -> ([])
  | RcvStmt _ -> ([])
  | Decl (ty, n, e) ->
      let code, place = translateB e in
      (code
      @ [IRC_AssignLocal (n, IRC_Var place, !currentLocals)])
  | DeclChan _ -> ([])
  | Assign (n, e) ->
      let code, place = translateB e in
      (code @ [IRC_AssignLocal (n, IRC_Var place, !currentLocals)])
  | While (e, l, s) ->
      let beg = freshLabel() in
      let body = freshLabel() in
      let exit = freshLabel() in
      let ecode, eplace = translateB e in
      let scode = translateStmt s in
      ( [IRC_Label beg]             (* begin:     *)
      @ ecode                       (*   e.code   *)
      @ irc_ZeroJump (eplace, exit) (*   jez exit *)
      @ [IRC_Label body]            (* body:      *)
      @ scode                       (*   s.code   *)
      @ [IRC_Goto beg]              (*   j begin  *)
      @ [IRC_Label exit]            (* exit:      *)
      )
  | ITE (e, s1, t, s2, f) ->
      let bfalse = freshLabel () in
      let exit = freshLabel () in
      let ecode, eplace = translateB e in
      let tcode = translateStmt t in
      let fcode = translateStmt f in
      ( ecode                         (*   e.code     *)
      @ irc_ZeroJump (eplace, bfalse) (*   jez bfalse *)
      @ tcode                         (*   t.code     *)
      @ [IRC_Goto exit]               (*   goto exit  *)
      @ [IRC_Label bfalse]            (* false:       *)
      @ fcode                         (*   f.code     *)
      @ [IRC_Label exit]              (* exit:        *)
      )
  | Return e ->
      let ecode, eplace = translateB e in
      (ecode @ [IRC_Return eplace])
  | FuncCall (fn, ps) ->
      (* e.g. f(a, b, c) *)
      (* t1 = eval a *)
      (* t2 = eval b *)
      (* t3 = eval c *)
      (* IRC_Param t1 *)
      (* IRC_Param t2 *)
      (* IRC_Param t3 *)
      (* IRC_Call x 3 where x is the label of function *)
      let codes, places = List.split (List.map translateB ps) in (* evaluate all the params *)
      (
        (* all the code needed to evaluate arguments *)
        (List.concat codes)
        (* save the place we can find the evaluated arguments *)
        @ (List.map (fun (p) -> IRC_Param p) places)
        (* call the function by label, we should have this value after walking the proc list *)
        @ [IRC_Call (lookup fn, List.length ps)]
      )
  | Print exp ->
      let code, place = translateB exp in
      (
        code
        @
        [IRC_Print place]
      )
  | Skip -> []

let translateProc proc =
  match proc with
  | Proc (n, params, ty, locals, stmt) ->
      let _ = currentLocals := localNames locals in
      let fnl = freshLabel() in
      let _ = insertSym n fnl in
      let tm = translateStmt stmt in
      (* activation record *)
      ( [IRC_Label fnl]
      (* initialize locals *)
      @ match locals with Locals s -> List.map (fun _ -> IRC_PushE 0) s
      (* proc body *)
      @ tm
      (* pop values *)
      @ match locals with Locals s -> List.map (fun _ -> IRC_PopE) s
      @ []
      )

let translate prog =
  match prog with
  | Prog (procs, l, stmt) ->
      let translatedProcs = List.concat (List.map
    (fun p ->
      let ps = translateProc p in
      let _ = currentLocals := [] in (* reset locals *)
      ps)
    procs
    ) in
      let _ = currentLocals := localNames l in
      (* i need to make rte space for local vars in main proc *)
      let initLocals = List.map (fun l -> IRC_PushE 0) !currentLocals in
      let translatedStmt = translateStmt stmt in
      IRC (translatedProcs @ initLocals @ translatedStmt)
