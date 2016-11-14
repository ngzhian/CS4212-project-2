open Printf

(* A simple stack-based VM with shared memory *)

(* VM supports only integers, so Booleans need to be mapped to integer
   where 0 = false and 1 = true


 *)

(* let helpassignloc loc = [assignfromstack(0, loc), pops] *)

type instructions =
                    Halt
                  (* Stack operations *)
                  | PushS of int
                  | PopS
                  | Add
                  | Sub
                  | Div
                  | Mult
                  | Lt
                  | Gt
                  | Eq

                  | Output

                  (* (Conditional) jumps *)
                  | NonZero of int
                  | Zero of int
                  | Jump of int
                  | JumpMemLoc of int

                  (* Memory operations *)

                  | Assign of int*int
                  | PushMemToStack of int
                  | AssignMemFromStack of int*int

                  | Lock of int
                  | Unlock of int

                  (* run-time (environment) stack *)

                  | PushE of int
                  | PopE
                  | PushMemToEnv of int
                  | AssignMemFromEnv of int*int
                  | UpdateToEnv of int*int
                  | PopStackToEnv of int

(* helpers to print instructions *)
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
  | Assign (loc, i) ->
      sprintf "Assign mem[%d] <- %d" loc i
  | PushMemToStack loc ->
      sprintf "PushMemToStack sta <- mem[%d]" loc
  | AssignMemFromStack (relPos, loc) ->
      sprintf "AssignMemFromStack mem[%d] <- sta[~%d]" loc relPos
  | Lock i -> "Lock " ^ (string_of_int i)
  | Unlock i -> "Unlock " ^ (string_of_int i)
  | PushE i -> "PushE " ^ (string_of_int i)
  | PopE -> "PopE"
  | PushMemToEnv i -> sprintf "PushMemToEnv env <- mem[%d]" i
  | AssignMemFromEnv (relPos, loc) ->
      sprintf "AssignMemFromEnv mem[%d] <- env[~%d]" loc relPos
  | UpdateToEnv (relPos, loc) ->
      sprintf "UpdateToEnv env[~%d] <- mem[%d]" relPos loc
  | PopStackToEnv relPos ->
      sprintf "PopStackToEnv env[~%d] <- sta" relPos


type lockInfo = { locked : bool;
                  threadID : int }

(* sp and ep refer to the next available position *)
type thread = { pc : int ref;
                code : instructions list;
                stack : int array;
                env : int array;
                sp : int ref;
                ep : int ref}

let string_of_thread t =
  sprintf "pc: %2d, sp: %2d, ep %2d, code: %-40s, stack:%2d, env:%2d"
  !(t.pc)
  !(t.sp)
  !(t.ep)
  (List.nth t.code !(t.pc) |> string_of_vm)
  (if !(t.sp) > 0 then (Array.get t.stack (!(t.sp)-1)) else 0)
  (if !(t.ep) > 0 then (Array.get t.env (!(t.ep)-1)) else 0)

type state = { mem : int array;
               memLock : lockInfo array;
               threads : (thread list) ref;
               activeThread : int ref}


let nameSupply = ref 1
let fresh _ =  nameSupply := !nameSupply + 1;
               !nameSupply

(* global shared memory *)
let memSize = 20000
let mkMem _ = Array.make memSize 0

let mkMemLock _ = Array.make memSize {locked = false; threadID = 0}

(* computation stack *)
let stkSize = 20000
let mkStk _ = Array.make stkSize 0

(* run-time environment stack *)
let envSize = 20000
let mkEnv _ = Array.make envSize 0

let mkThread cs = { pc = ref 0;
                    code = cs;
                    stack = mkStk();
                    env = mkEnv();
                    sp = ref 0;
                    ep = ref 0 }

let initState cs = { mem = mkMem();
                     memLock = mkMemLock();
                     threads = ref [mkThread cs];
                     activeThread = ref 0}

let inc r = r := !r + 1
let dec r = r := !r - 1


let singleStep id mem memLock t =
  (* uncomment to debug *)
  (* let _ = printf "%s\n" (string_of_thread t) in *)
  match (List.nth t.code !(t.pc)) with
  | Halt ->   true
  | PushS i -> t.stack.(!(t.sp)) <- i;
               inc t.pc;
               inc t.sp;
               false

  | PopS -> inc t.pc;
            dec t.sp;
            false

  (* | Reverse n -> ( *)
  (*   Array.sub t.stack (t.sp - n) n *)

  (* ) *)

  (* recall that sp refers to the next available position,
     so must subtract 1 to access top element *)
  | Add -> let i = !(t.sp) - 1
           in t.stack.(i-1) <- t.stack.(i) + t.stack.(i-1);
              inc t.pc;
              dec t.sp;
              false

  | Sub -> let i = !(t.sp) - 1
          (* modified this slightly
           * when generating a - b, we generate code to push a then push be
           * so it should be t.stack.(i-1) - t.stack.(i), instead of other way
           * *)
           in t.stack.(i-1) <- t.stack.(i-1) - t.stack.(i);
              inc t.pc;
              dec t.sp;
              false

  | Div -> let i = !(t.sp) - 1
          (* modified this slightly
           * when generating a - b, we generate code to push a then push be
           * so it should be t.stack.(i-1) - t.stack.(i), instead of other way
           * *)
           in t.stack.(i-1) <- t.stack.(i-1) / t.stack.(i);
              inc t.pc;
              dec t.sp;
              false

  | Mult -> let i = !(t.sp) - 1
            in t.stack.(i-1) <- t.stack.(i) * t.stack.(i-1);
               inc t.pc;
               dec t.sp;
               false

  | Lt -> let i = !(t.sp) - 1
          in (if t.stack.(i-1) < t.stack.(i)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false

  | Gt -> let i = !(t.sp) - 1
          in (if t.stack.(i-1) > t.stack.(i)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false

  | Eq -> let i = !(t.sp) - 1
          in (if t.stack.(i) == t.stack.(i-1)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false

  | Output -> Printf.printf "%d \n" t.stack.(!(t.sp) - 1);
              inc t.pc;
              false

  | NonZero i -> let x = t.stack.(!(t.sp) - 1) in
                 dec t.sp;
                 (if x == 0
                 then inc t.pc
                 else t.pc := i);
                 false

  | Zero i ->    let x = t.stack.(!(t.sp) - 1) in
                 inc t.sp;
                 (if x == 0
                 then t.pc := i
                 else inc t.pc);
                 false

  | Jump i -> t.pc := i;
              false

  | JumpMemLoc loc -> t.pc := mem.(loc);
                      false

  | Assign (loc,i) -> inc t.pc;
                      mem.(loc) <- i;
                      false

  | PushMemToStack loc -> inc t.pc;
                       t.stack.(!(t.sp)) <- mem.(loc);
                       inc t.sp;
                       false

  (* deref of the relative position relPos and assign to mem loc,
     to access top-most stack element set relPos=1,
     recall that sp refers to the next available position *)
  | AssignMemFromStack (relPos,loc) -> inc t.pc;
                                    mem.(loc) <- t.stack.(!(t.sp) - relPos);
                                    false

  (* lock a memory cell, note that acess via assign doesn't check if cell is locked,
     hence, we assume that in case of shared memory, every access is protected by lock *)
  | Lock loc -> if memLock.(loc).locked
                then true
                else (memLock.(loc) <- {locked = true; threadID = id};
                      inc t.pc;
                      false)

  (* only the owner can unlock a memory cell, multiple unlock yield failure (true) *)
  | Unlock loc -> if (not (memLock.(loc).locked && memLock.(loc).threadID == id))
                  then true
                  else (memLock.(loc) <- {locked = false; threadID = id};
                      inc t.pc;
                      false)

  | PushE i -> t.env.(!(t.ep)) <- i;
               inc t.pc;
               inc t.ep;
               false

  | PopE -> inc t.pc;
            dec t.ep;
            false

  (* Push value at memory location to RTE *)
  | PushMemToEnv loc -> inc t.pc;
                     t.env.(!(t.ep)) <- mem.(loc);
                     inc t.ep;
                     false

  | PopStackToEnv relPos -> inc t.pc;
                      t.env.(!(t.ep) - relPos) <- t.stack.(!(t.sp) - 1);
                      dec t.sp;
                      false

  | AssignMemFromEnv (relPos,loc) -> inc t.pc;
                                  mem.(loc) <- t.env.(!(t.ep) - relPos);
                                  false

  (* Update RTE relative position to value at memory location *)
  | UpdateToEnv (relPos,loc) -> inc t.pc;
  (* temporary assignments are consumed via stack *)
                                t.env.(!(t.ep) - relPos) <- mem.(loc);
                                false

let debug txt = Printf.printf txt;
                Printf.printf "\n"

let run cs = let st = initState cs in
             let stop = ref false in
             while not !stop do
                  stop := true;
                  for i = 0 to List.length !(st.threads) - 1 do 
                    if not (singleStep i st.mem st.memLock (List.nth !(st.threads) i))
                    then stop := false
                  done;
             done;
             st

let testProg1 = [PushS 1; PushS 2; Add; Output; Halt]

let testProg2 = [PushS 1; PushS 2; Lt; Output; Halt]

let getThread st i = (List.nth !(st.threads) i)
