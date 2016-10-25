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
  | BConst true -> let x = freshName() in
    ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
    ([IRC_Assign (x, IRC_IConst 0)], x)
  | And (e1, e2) -> (* 
                       e1.code;
                       if !e1.result goto l1
                       e2.code;
                       x = e2.result;
                       goto l2;
                       l1:
                         x = 0;             Booleans represented as integers
                       l2:
                         *)

    let r1 = translateB e1 in
    let r2 = translateB e2 in
    let x = freshName() in
    let l1 = freshLabel() in
    let l2 = freshLabel() in
    ((fst r1)
     @
     (irc_ZeroJump (snd r1,l1))
     @
     (fst r2)
     @
     [IRC_Assign (x, IRC_Var (snd r2));
      IRC_Goto l2 ]
     @
     [IRC_Label l1;
      IRC_Assign (x, IRC_IConst 0);
      IRC_Label l2],
     x)
