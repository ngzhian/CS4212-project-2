open Irc

let to_string_irc_exp irc_exp =
  match irc_exp with
  | IRC_And (a, b) -> "IRC_And " ^ a ^ b
  | IRC_Eq (a, b) -> "IRC_Eq " ^ a ^ b
  | IRC_Gt (a, b) -> "IRC_Gt " ^ a ^ b
  | IRC_Plus (a, b) -> "IRC_Plus " ^ a ^ b
  | IRC_Minus (a, b) -> "IRC_Minus " ^ a ^ b
  | IRC_Times (a, b) -> "IRC_Times " ^ a ^ b
  | IRC_Division (a, b) -> "IRC_Division " ^ a ^ b
  | IRC_Not (a) -> "IRC_Not " ^ a
  | IRC_IConst (a) -> "IRC_IConst " ^ (string_of_int a)
  | IRC_Var (a) -> "IRC_Var " ^ a

let to_string_irc_cmd irc_cmd =
  match irc_cmd with
  | IRC_Assign (label, exp) -> "IRC_Assign " ^ label ^ " " ^ (to_string_irc_exp exp)
  | IRC_Label (label) -> "IRC_Label " ^ (string_of_int label)
  | IRC_Goto (label) -> "IRC_Goto " ^ (string_of_int label)
  | IRC_NonzeroJump (label, a) -> "IRC_NonzeroJump " ^ label ^ " " ^ (string_of_int a)
  | IRC_Param (label) -> "IRC_Param " ^ label
  | IRC_Call (label, a) -> "IRC_Call " ^ (string_of_int label) ^ " " ^ (string_of_int a)
  | IRC_Return (label) -> "IRC_Return " ^ label
  | IRC_Get (label) -> "IRC_Get " ^ label

let to_string_irc irc =
  match irc with
  | IRC cmds ->
      List.mapi (fun i -> fun cmd -> (string_of_int (i+1)) ^ " " ^ (to_string_irc_cmd cmd) ^ "\n") cmds
      |> String.concat ""
