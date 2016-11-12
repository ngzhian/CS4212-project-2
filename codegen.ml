open Vm
open Printf
open Irc

(* from expression name to memory location *)
module Memory = Map.Make(String);;
module Labels = Map.Make(String);;

let memory = ref Memory.empty;;
let labels = ref Labels.empty;;

let memLoc = ref 0
let upMem _ =  memLoc := !memLoc + 1; !memLoc
let downMem =  memLoc := !memLoc - 1

let index_of name locals =
  let rec find name locals c =
    match locals with
    | [] -> -1
    | x::xs -> if String.equal name x then c else find name xs (c+1) in
  find name locals 0

let codeGenIrcExp exp =
  match exp with
  | IRC_And (name1, name2) -> [Vm.Halt] (* todo *)
  | IRC_Eq _ -> [Vm.Eq]
  | IRC_Gt _ -> [Vm.Gt]
  | IRC_Plus _ -> [Vm.Add]
  | IRC_Minus _ -> [Vm.Sub]
  | IRC_Times _ -> [Vm.Mult]
  | IRC_Division _ -> [Vm.Div]
  | IRC_Not v -> (* equivalent to v == 0 *)
    [ Vm.PushS 0; Vm.Eq ]
  | IRC_IConst i -> [Vm.PushS i]
  | IRC_Local (name, locals) ->
      let m = upMem () in
      let relpos = (List.length locals) - (index_of name locals) in
      (* push from rte to stack *)
      [
        Vm.AssignMemFromEnv (relpos, m);
        Vm.PushMemToStack m
      ]
  (* trying to get a temporary variable, so get it from mem *)
  (* trying to get a temporary variable, only generate as temp, should be on stack *)
  | IRC_Var n -> []

let codeGenIrcCmd cmd =
  match cmd with
  (* find memory loc of name, then copy exp *)
  | IRC_PopE -> [Vm.PopE]
  | IRC_PushE i -> [Vm.PushE i]
  (* updating a local *)
  | IRC_AssignLocal (name, exp, locals) ->
      let m = upMem () in
      let relpos = (List.length locals) - (index_of name locals) in
      codeGenIrcExp exp
      @
      [
        Vm.AssignMemFromStack (1, m); (* copy sp - relpos to mem loc *)
        Vm.UpdateToEnv (relpos, m); (* update rte *)
        PopS
      ]
  (* temporary assignments, find a place in memory and assign tmp var there *)
  | IRC_Assign (name, exp) ->
      let m = upMem () in
      let _ = memory := Memory.add name m !memory in
      codeGenIrcExp exp
      @
      [
      ]
  | IRC_Label l ->
      (* take note of what instruction number this label is at *)
      let _ = labels := Labels.add (string_of_int l) 1 !labels in
      []
  | IRC_Goto l ->
      [Vm.JumpTemp l]
  (* todo *)
  | IRC_NonzeroJump (x, l) ->
      [
        Vm.PushMemToStack (Memory.find x !memory);
        Vm.NonZero l
      ]
            (* | IRC_NonzeroJump of string * int  (1* if x L = if x non-zero then jump to L *1) *)
      (* todo *)
  | IRC_Param v ->
      (* find the temp variable from memory, then push its value to the RTE *)
      [Vm.PushMemToEnv (Memory.find v !memory)]
  | IRC_Call _ ->
      (* push return addr, push args, jump *)
      [Vm.Halt]
      (* todo *)
  | IRC_Return _ ->
      [Vm.Halt]
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

let rec codeGen (ir : Irc.irc) =
  match ir with
  | IRC [] -> Some []
  | IRC (x::xs) ->
    let cg = codeGenIrcCmd x in
    (match (codeGen (IRC xs)) with
     | Some g -> Some (cg @ g @ [Vm.Halt])
     | None -> None)
