open Vm

let string_of_vm ins =
  match ins with
  | Halt -> "Halt"
  | PushS i -> "PushS " ^ (string_of_int i)
  | PopS -> "PopS"
  | Add -> "Add"
  | Sub -> "Sub"
  | Div -> "Div"
  | Mult -> "Mult"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Eq -> "Eq"
  | Output -> "Output"
  | NonZero i -> "NonZero " ^ (string_of_int i)
  | Zero i -> "Zero " ^ (string_of_int i)
  | Jump i -> "Jump " ^ (string_of_int i)
  | JumpMemLoc i -> "JumpMemLoc " ^ (string_of_int i)
  | Assign (i, j) -> "Assign " ^ (string_of_int i) ^ " " ^ (string_of_int j)
  | PushMemToStack i -> "PushMemToStack loc:" ^ (string_of_int i)
  | AssignMemFromStack (i, j) -> "AssignMemFromStack relpos:" ^ (string_of_int i) ^ " loc:" ^ (string_of_int j)
  | Lock i -> "Lock " ^ (string_of_int i)
  | Unlock i -> "Unlock " ^ (string_of_int i)
  | PushE i -> "PushE " ^ (string_of_int i)
  | PopE -> "PopE"
  | PushMemToEnv i -> "PushMemToEnv loc:" ^ (string_of_int i)
  | AssignMemFromEnv (i, j) -> "AssignMemFromEnv relpos:" ^ (string_of_int i) ^ " loc:" ^ (string_of_int j)
  | UpdateToEnv (i, j) -> "UpdateToEnv relpos:" ^ (string_of_int i) ^ " loc:" ^ (string_of_int j)
