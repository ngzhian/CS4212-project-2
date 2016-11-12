open Vm
open Printf
open Irc

(* from expression name to memory location *)
module Memory = Map.Make(String);;
module Labels = Map.Make(struct type t = int let compare = compare end)

let memory = ref Memory.empty;;
let labels = ref Labels.empty;;

let memLoc = ref 0
let upMem _ =  memLoc := !memLoc + 1; !memLoc
let downMem =  memLoc := !memLoc - 1


let localRelPos name locals =
  let rec indexOf name locals c =
    match locals with
    | [] -> -1
    | x::xs -> if String.equal name x then c else indexOf name xs (c+1) in
  (List.length locals) - (indexOf name locals 0)

let codeGenIrcExp exp =
  match exp with
  | IRC_And _ -> [ ] (* not used since we transated this to lower level instrs *)
  | IRC_Eq _ -> [ Vm.Eq ]
  | IRC_Gt _ -> [ Vm.Gt ]
  | IRC_Plus _ -> [ Vm.Add ]
  | IRC_Minus _ -> [ Vm.Sub ]
  | IRC_Times _ -> [ Vm.Mult ]
  | IRC_Division _ -> [ Vm.Div ]
  | IRC_Not v -> [ Vm.PushS 0; Vm.Eq ] (* equivalent to v == 0 *)
  | IRC_IConst i -> [ Vm.PushS i ]
  | IRC_Local (name, locals) ->
      let m = upMem () in
      let relpos = localRelPos name locals in
      (* push from rte to stack *)
      [
        Vm.AssignMemFromEnv (relpos, m);
        Vm.PushMemToStack m
      ]
  | IRC_Var n -> [] (* our temp variables are on the stack, so nothing needs to be done *)

let codeGenIrcCmd cmd ic =
  match cmd with
  | IRC_PopE -> [ Vm.PopE ]
  | IRC_PushE i -> [ Vm.PushE i ]
  (* updating a local variable *)
  | IRC_AssignLocal (name, exp, locals) ->
      (* get a fresh memory location *)
      let m = upMem () in
      (* find the relative position of this local variable in the RTE activation record *)
      let relpos = localRelPos name locals in
      codeGenIrcExp exp
      @
      [
        Vm.AssignMemFromStack (1, m); (* copy sp - relpos to mem loc *)
        Vm.UpdateToEnv (relpos, m); (* update rte *)
        PopS
      ]
  | IRC_Assign (name, exp) ->
      (* temporary assignments are consumed via stack *)
      codeGenIrcExp exp
  | IRC_Label l ->
      (* take note of what instruction number this label is at *)
      labels := Labels.add l ic !labels;
      []
  | IRC_Goto l ->
      (* will be filled in with the proper pc later *)
      [ Vm.Jump l ]
  | IRC_NonzeroJump (x, l) ->
      [ Vm.NonZero l ]
  | IRC_Param v ->
      (* temp variables are all on the stack, will be pushed to RTE by IRC_Call *)
      []
  | IRC_Call (fn, n) ->
      let return_add = [ PushE (ic + 1) ] in
      let rec build acc n =
        if n == 0 then acc else (Vm.PushE 0):: (build acc (n - 1)) in
      (* push default values for all args into RTE *)
      let initArguments = build [] n in
      (* then replace default values of values from stack *)
      (* top of stack has value of rightmost argument, which goes to the top of the RTE *)
      let stackToEnv = List.mapi (fun i _ -> Vm.PushStackToEnv (i + 1)) initArguments in
      let jump = [Jump fn] in

        return_add
      @ initArguments
      @ stackToEnv
      @ jump

      (* todo *)
  | IRC_Return _ ->
      [ Vm.Halt ]
      (* todo *)
  | IRC_Get name ->
      let m = upMem () in
      let _ = memory := Memory.add name m !memory in
      [
        AssignMemFromEnv (3 (* todo get relpos of local *), m);
        PushMemToStack m;
      ]
  | IRC_Print v ->
      [
        Vm.Output
      ]

(* substitute the temp jump labels with actual instruction index to jump to *)
let subLabels =
  List.map
  (fun i ->
    match i with
    | NonZero l -> NonZero (Labels.find l !labels)
    | Zero l -> Zero (Labels.find l !labels)
    | Jump l -> Jump (Labels.find l !labels)
    | _ -> i)

let rec codeGen (IRC is) =
  List.fold_left
  (fun (instrs, labels) i ->
    (* generate code for the next instruction *)
    let cg = codeGenIrcCmd i (List.length instrs) in
    (* right now labels update is mutated, so we simply apply it *)
    (* we can probably pass this in an do an update next time *)
    ((instrs @ cg), labels))
  ([], labels)
  is
  |> fst
  (* always add a Vm.Halt as the last instruction *)
  |> (fun instrs -> instrs @ [Vm.Halt])
  |> subLabels
  |> (fun s -> Some s)
