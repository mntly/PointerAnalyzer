module Checker.Return64Detection.CallerChecker

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open PointerAnalyzer.Frontend.ProgramDFA
open Checker.Return64Detection.Return64Types
open Checker.Return64Detection.Analysis.StatementIndex

module StmtIndex = Checker.Return64Detection.Analysis.StatementIndex

let private edx = Intel.Register.toRegID Intel.Register.EDX

/// <summary>
/// Represent the result of analyzing statement.
/// </summary>
/// <remarks>
/// <c>EDXUsed</c> indicates that EDX is used in current statement.
/// <c>EDXOverwritten</c> indicates that EDX is defined and not used in current
/// statement.
/// <c>KeepScanning</c> represents that the analyzer can not determine EDX is
/// used or defined.
/// </remarks>
type private ScanResult =
  | EDXUsed of CallerEvidence
  | EDXOverwritten
  | KeepScanning

/// Check given SSA variable is EDX
let private isEDX variable =
  match variable.Kind with
  | RegVar (_, registerId, _) -> registerId = edx
  | _ -> false

/// Extract SSA variables USED in given expr
let rec private variablesInExpr expr =
  match expr with
  | Var variable -> Set.singleton variable
  | UnOp (_, _, inner)
  | Cast (_, _, inner)
  | Extract (inner, _, _) -> variablesInExpr inner
  | BinOp (_, _, left, right)
  | RelOp (_, _, left, right) ->
    Set.union (variablesInExpr left) (variablesInExpr right)
  | Ite (condition, _, trueExpr, falseExpr) ->
    Set.unionMany
      [ variablesInExpr condition
        variablesInExpr trueExpr
        variablesInExpr falseExpr ]
  | Load (_, _, address) -> variablesInExpr address
  | Store (_, _, address, value) ->
    Set.union (variablesInExpr address) (variablesInExpr value)
  | ExprList expressions ->
    expressions |> List.map variablesInExpr |> Set.unionMany
  | Num _
  | FuncName _
  | Undefined _ -> Set.empty

/// Extract SSA variables USED in given stmt
let private usedVariables =
  function
  | Def (_, expression) -> variablesInExpr expression
  | Phi (_, _) -> Set.empty
  // | Phi (destination, sourceIds) ->
  //   sourceIds
  //   |> Seq.map (fun identifier ->
  //     { destination with
  //         Identifier = identifier })
  //   |> Set.ofSeq
  | Jmp (IntraJmp _) -> Set.empty
  | Jmp (IntraCJmp (condition, _, _)) -> variablesInExpr condition
  | Jmp (InterJmp target) -> variablesInExpr target
  | Jmp (InterCJmp (condition, trueTarget, falseTarget)) ->
    Set.unionMany
      [ variablesInExpr condition
        variablesInExpr trueTarget
        variablesInExpr falseTarget ]
  | ExternalCall (callee, inputs, _) ->
    Set.union (variablesInExpr callee) (inputs |> Set.ofList)
  | LMark _
  | SideEffect _ -> Set.empty

/// Iterate B2R2 DFA Use chain to determine given variable is used or not.
/// If given variable is appeared except PHI statements, `transitivePhiUses`
/// says corresponding variable is Used.
/// If given variable is used to PHI, given variable is marked as Used only
/// when corresponding PHI result also Used.
let private transitivePhiUses
  (edges: SSAEdges)
  (statementIndex: StatementIndex)
  variable
  =
  let rec visit visited current =
    if Set.contains current visited then
      []
    else
      let visited = Set.add current visited

      match edges.Uses.TryGetValue current with
      | false, _ -> []
      | true, uses ->
        uses
        |> Seq.choose (fun location -> Map.tryFind location statementIndex)
        |> Seq.collect (fun entry ->
          match entry.Statement with
          | Phi (destination, _) -> visit visited destination
          | _ ->
            [ { Variable = current
                ProgramPoint = entry.ProgramPoint
                Reason = "EDX-derived PHI result used after function call" } ])
        |> Seq.toList

  visit Set.empty variable |> List.distinct

/// Check whether given statement defines EDX
let private definedEDX =
  function
  | Def (variable, _)
  | Phi (variable, _) -> isEDX variable
  | ExternalCall (_, _, _) ->
    (* EDX is caller-saved register *)
    true
  | _ -> false

/// Convert BitVector into UInt64.
let private tryUInt64 (value: BitVector) =
  try
    Some (value.ToUInt64 ())
  with _ ->
    None

/// Resolve the target of an InterJmp using either its value or B2R2's
/// constant-propagation result.
let private tryJumpTarget (caller: FunctionDFAResult) =
  function
  | Num value -> tryUInt64 value
  | Var variable ->
    caller.DFAResult.ConstValue variable |> Option.bind tryUInt64
  | _ -> None

/// Check whether given stmt is function call.
/// This utilize Callee of B2R2 first, and DFA result second.
let private isFunctionCall
  (functions: Map<Addr, FunctionDFAResult>)
  (caller: FunctionDFAResult)
  (programPoint: ProgramPoint)
  stmt
  =
  match stmt with
  | Jmp (InterJmp target) ->
    match Map.tryFind programPoint.Address caller.Callees with
    | Some callees when not (Set.isEmpty callees) -> true
    | _ ->
      tryJumpTarget caller target
      |> Option.exists (fun targetAddress ->
        Map.containsKey targetAddress functions)
  | _ -> false

/// Check given call site is in given block
let private containsCallSite callSite (block: IVertex<SSABasicBlock>) =
  block.VData.Internals.Statements
  |> Array.exists (fun (programPoint, _) -> programPoint.Address = callSite)

/// Extract FunctionAbstraction of given callee in caller CFG
let private abstractionsForCallSite
  calleeAddr
  callSite
  (caller: FunctionDFAResult)
  =
  caller.CFG.Vertices
  |> Array.filter (fun block ->
    (* Check current block is FunctionAbstraction *)
    block.VData.Internals.IsAbstract
    (* Check current block is about callee *)
    && block.VData.Internals.AbstractContent.EntryPoint = calleeAddr
    (* Check current block is called from callsite *)
    && caller.CFG.GetPreds block |> Array.exists (containsCallSite callSite))

/// Starting from successor of given callee FunctionAbstraction, check whether
/// EDX is overwritten or used. If EDX is used before overwritten, it may be
/// consider as return value.
let private continuationEvidence
  functions
  callerAddress
  callSite
  (caller: FunctionDFAResult)
  (calleeAbst: IVertex<SSABasicBlock>)
  =
  let callerCFG = caller.CFG
  let edges = SSAEdges callerCFG
  let statementIndex = StmtIndex.build callerCFG

  let rec visit visited (block: IVertex<SSABasicBlock>) =
    if Set.contains block.ID visited || block.VData.Internals.IsAbstract then
      []
    else
      let visited = Set.add block.ID visited

      (*
        Analyze give statements and determine EDX is used, defined, or not 
        determined
      *)
      let rec scanStmt remainingStatements =
        match remainingStatements with
        | [] -> KeepScanning
        | (programPoint, stmt) :: rest ->
          (* Check EDX is used in current Statement *)
          (* Used before overwriting EDX, it may acts as return value *)
          let edxUses = usedVariables stmt |> Set.filter isEDX

          if not (Set.isEmpty edxUses) then
            let variable = Set.minElement edxUses

            let directUse =
              { Variable = variable
                ProgramPoint = programPoint
                Reason = "EDX used after function call" }

            let confirmedUses =
              match stmt with
              // | Phi (destination, _) ->
              //   match transitivePhiUses edges statementIndex destination with
              //   | [] -> []
              //   | uses -> directUse :: uses
              | _ -> [ directUse ]

            match confirmedUses with
            | uses when not (List.isEmpty uses) ->
              EDXUsed
                { CallerAddress = callerAddress
                  CallSite = callSite
                  EDXVariable = variable
                  Uses = uses }
            | _ -> scanStmt rest
          elif isFunctionCall functions caller programPoint stmt then
            (* EDX is caller-saved register *)
            EDXOverwritten
          elif definedEDX stmt then
            (* EDX is overwritten. EDX may not act as return value *)
            EDXOverwritten
          else
            (* Can not determine. Analyze next statement *)
            scanStmt rest

      match block.VData.Internals.Statements |> Array.toList |> scanStmt with
      | EDXUsed evidence -> [ evidence ]
      | EDXOverwritten -> []
      | KeepScanning ->
        callerCFG.GetSuccs block |> Array.toList |> List.collect (visit visited)

  callerCFG.GetSuccs calleeAbst
  |> Array.toList
  |> List.collect (visit Set.empty)

/// Extract caller-callee relationship of given binary, and
/// construct mapping from callee address to Set(caller address, call site)
let private callerSites (program: ProgramDFAResult) =
  (* Construct (callee address, caller address, call site) seq *)
  let constCalleeCaller (callerAddress, caller) =
    caller.Callees
    |> Map.toSeq
    |> Seq.collect (fun (callSite, targets) ->
      targets |> Seq.map (fun target -> target, (callerAddress, callSite)))

  let constCalleeCallerSet (target, entries) =
    target, entries |> Seq.map snd |> Set.ofSeq

  program.Functions
  |> Map.toSeq
  |> Seq.collect constCalleeCaller
  |> Seq.groupBy fst
  |> Seq.map constCalleeCallerSet
  |> Map.ofSeq

let private evidenceForCaller
  functions
  calleeAddr
  callSite
  (caller: FunctionDFAResult)
  =
  let calleeAbstractions = abstractionsForCallSite calleeAddr callSite caller

  calleeAbstractions
  |> Seq.collect (continuationEvidence functions caller.Address callSite caller)
  |> Seq.toList

let apply (program: ProgramDFAResult) detections =
  (* Construct callee-caller mapping *)
  let sites = callerSites program

  (* Apply Caller-Callee relationship *)
  let applyInner calleeAddr detection =
    match detection.BasicStatus with
    | NotReturn64
    | Unknown
    | UnknownCallEvidence -> detection
    | Return64 ->
      (*
        Add Caller-Callee relationship only the basic heuristic said that
        function return 64 bit value
      *)
      let targetSites =
        Map.tryFind calleeAddr sites |> Option.defaultValue Set.empty

      (*
        Iterate successor blocks of FunctionAbstraction of each caller, and
        extract the EDX usecase before defining
      *)
      let evidence =
        targetSites
        |> Seq.collect (fun (callerAddress, callSite) ->
          let caller = Map.find callerAddress program.Functions
          evidenceForCaller program.Functions calleeAddr callSite caller)
        |> Seq.distinct
        |> Seq.toList

      let status =
        if not (List.isEmpty evidence) then
          (* EDX is used before defining *)
          (* It may consider EDX acts as return value *)
          Return64
        elif Set.isEmpty targetSites then
          (*
            Does not exist callers. Can not determine given callee returns 64
            bit value or not
          *)
          UnknownCallEvidence
        else
          (* Other case, EDX may not act as return value *)
          NotReturn64

      { detection with
          Status = status
          CallerEvidence = evidence }

  Map.map applyInner detections
