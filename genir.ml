open Ast
open Irc

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
  !labelSupply
let freshName _ =  labelSupply := !labelSupply + 1;
  String.concat "" ["genirc" ; string_of_int (!labelSupply )]


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
  | Var v -> ([IRC_Get v], v)
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
      @ [IRC_Call (1, n)]
      @ [IRC_Get x], x)
  | RcvExp _ -> ([],"")
