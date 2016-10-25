let nameSupply = ref 1
let freshName _ =  nameSupply := !nameSupply + 1;
  String.concat "" ["irctemp" ; string_of_int (!nameSupply )]

type irc = IRC of (irc_cmd list)

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
            | IRC_Param of string
            | IRC_Call of int * int (* (label, number of parameters *)
            | IRC_Return of string
            | IRC_Get of string

and irc_exp = IRC_And of string * string
            | IRC_Eq of string * string
            | IRC_Gt of string * string
            | IRC_Plus of string * string
            | IRC_Minus of string * string
            | IRC_Times of string * string
            | IRC_Division of string * string
            | IRC_Not of string
            | IRC_IConst of int
            | IRC_Var of string

(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
  [IRC_Assign (y, IRC_Not x);
   IRC_NonzeroJump (y,l)]
