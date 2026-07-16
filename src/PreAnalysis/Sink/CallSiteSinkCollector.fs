module PointerAnalyzer.PreAnalysis.Sink.CallSiteSinkCollector

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// <summary>
/// Track current register states and live registers.
/// </summary>
/// <remarks>
/// <c>CurrentRegisters</c> keeps current live registers.
/// <c>Sinks</c> stores the live registers used as arguments at each function
/// call.
/// </remarks>
type private ScanState =
  { CurrentRegisters: Map<RegisterID, Variable>
    Sinks: Set<Variable> }

/// <summary>
/// Represent the result of CallSiteSink extraction.
/// </summary>
/// <remarks>
/// <c>Sinks</c> stores the live registers used as arguments at each function
/// call.
/// <c>Visited</c> keeps  the prograpoint already visited.
/// </remarks>
type private RunResult =
  { Sinks: Set<Variable>
    Visited: Set<int> }

/// If given SSA variable is register, then updates current register.
let private updateCurrentRegister variable (state: ScanState) =
  match tryRegisterId variable with
  | Some registerId ->
    { state with
        CurrentRegisters = Map.add registerId variable state.CurrentRegisters }
  | None -> state

/// Update given registers to live SSA Variables.
let private addSinks (state: ScanState) sinks =
  { state with
      Sinks = Set.union state.Sinks sinks }

/// Extract function arguments with soundness.
/// Extract all posible register used as arguments.
let private callsiteRegisterSinks platform (state: ScanState) =
  (* Extract current registers used as function arguments *)
  let filterReg regId =
    Map.tryFind regId state.CurrentRegisters

  let regArg = platform.ArgumentRegisters |> List.choose filterReg |> Set.ofList

  addSinks state regArg

/// Mark registers in jump target expression as live.
let private controlFlowTargetSinks stmt =
  match stmt with
  | Jmp (IntraJmp _) -> Set.empty
  | Jmp (IntraCJmp (condition, _, _)) -> variablesInExpr condition
  | Jmp (InterJmp target) -> variablesInExpr target
  | Jmp (InterCJmp (condition, trueTarget, falseTarget)) ->
    Set.unionMany
      [ variablesInExpr condition
        variablesInExpr trueTarget
        variablesInExpr falseTarget ]
  | ExternalCall (callee, _, _) -> variablesInExpr callee
  | _ -> Set.empty

/// Resolve a direct target or a target proved constant by B2R2 DFA.
let private tryResolveTarget function_ target =
  match target with
  | Num value -> Some (value.ToUInt64 ())
  | Var variable ->
    function_.DFAResult.ConstValue variable
    |> Option.map (fun value -> value.ToUInt64 ())
  | _ -> None

/// Determine given stmt is function call or not.
/// This function determines using first B2R2 Callee information,
/// second jump target address.
let private isFunctionCall
  (functions: Map<Addr, FunctionDFAResult>)
  function_
  (programPoint: ProgramPoint)
  stmt
  =
  match stmt with
  | Jmp (InterJmp target) ->
    if Map.containsKey programPoint.Address function_.Callees then
      (* Check current stmt is in B2R2 Callee information *)
      true
    else
      (* Check jump target is valid function address *)
      target
      |> tryResolveTarget function_
      |> Option.exists (fun address -> Map.containsKey address functions)
  | ExternalCall _ -> true
  | _ ->
    (*
      ToDo
        How should I handle conditional jump?
    *)
    false

/// Iterate stmts in one block to extract registers used as arguments of
/// function call at each callsite
let private scanStatement
  platform
  (functions: Map<Addr, FunctionDFAResult>)
  function_
  (state: ScanState)
  ((programPoint, stmt): ProgramPoint * Stmt)
  =
  let state =
    if isFunctionCall functions function_ programPoint stmt then
      (* Extract current regiseter only when current stmt is call instruction *)
      callsiteRegisterSinks platform state
    else
      state

  (* If current stmt defines register, update corresponding register *)
  let state =
    match definedVariable stmt with
    | Some variable -> updateCurrentRegister variable state
    | None -> state

  (* If current stmt is jmp, extract registers in jump target expression *)
  let state = stmt |> controlFlowTargetSinks |> addSinks state

  state

/// Extract undefined RegVar used.
/// If there exist multiple undefined RegVar with same register,
/// use RegVar with minimum Identifier.
let private incomingRegisters (cfg: SSACFG) =
  let edges = SSAEdges cfg

  edges.Uses.Keys
  |> Seq.filter (fun variable -> not (edges.Defs.ContainsKey variable))
  |> Seq.choose (fun variable ->
    tryRegisterId variable
    |> Option.map (fun registerId -> registerId, variable))
  |> Seq.groupBy fst
  |> Seq.map (fun (registerId, variables) ->
    let variable =
      variables |> Seq.map snd |> Seq.minBy (fun value -> value.Identifier)

    registerId, variable)
  |> Map.ofSeq

/// Collect live registers used for function arguments at each callsite.
/// StackVar is handled separately as a default-live variable.
let collect
  platform
  (functions: Map<Addr, FunctionDFAResult>)
  (function_: FunctionDFAResult)
  =
  let cfg = function_.CFG

  let initialState =
    { CurrentRegisters = incomingRegisters cfg
      Sinks = Set.empty }

  (*
    Extract registers used as argument at each function call starting with
    given block
  *)
  let rec run (block: IVertex<SSABasicBlock>) inputState visited =
    if Set.contains block.ID visited then
      (* If current block is already visited ,do not visited again *)
      { Sinks = Set.empty; Visited = visited }
    else
      (* Update visited block *)
      let visited = Set.add block.ID visited

      (* Extract stmts of each block and process them *)
      let state =
        block.VData.Internals.Statements
        |> Seq.fold (scanStatement platform functions function_) inputState

      (* Move to successed block *)
      cfg.GetSuccs block
      |> Array.fold
        (fun result successor ->
          let successorResult = run successor state result.Visited

          { Sinks = Set.union result.Sinks successorResult.Sinks
            Visited = successorResult.Visited })
        { Sinks = state.Sinks
          Visited = visited }

  cfg.Roots
  |> Array.fold
    (fun result root ->
      let rootResult = run root initialState result.Visited

      { Sinks = Set.union result.Sinks rootResult.Sinks
        Visited = rootResult.Visited })
    { Sinks = Set.empty
      Visited = Set.empty }
  |> fun result -> result.Sinks
