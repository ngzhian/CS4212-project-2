(* Abstract Syntax Tree (AST) representation of Mini-Go *)

type prog = Prog of (proc list) * locals * stmt

and proc = Proc of string * ((exp * types) list) * (types option) * locals * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types option)

and stmt = Seq of stmt * stmt
          | Go of stmt
          | Transmit of string * exp
          | RcvStmt of string 
          | Decl of (types option) * string * exp (* Decl of types option * string * exp *)
          | DeclChan of string
          | Assign of string * exp
          | While of exp * locals * stmt
          | ITE of exp * locals * stmt * locals * stmt
          | Return of exp
          | FuncCall of string * (exp list)
          | Print of exp
          | Skip

and exp = And of exp * exp
         | Eq of exp * exp
         | Gt of exp * exp
         | Plus of exp * exp
         | Minus of exp * exp
         | Times of exp * exp
         | Division of exp * exp
         | Not of exp
         | RcvExp of string
         | IConst of int
         | BConst of bool
         | Var of string
         | FuncExp of string * (exp list)

 and locals = Locals of (string * types) list (* local variables occuring in some nested block *)
