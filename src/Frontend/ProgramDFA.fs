module PointerAnalyzer.Frontend.ProgramDFA

open B2R2
open B2R2.MiddleEnd
open B2R2.MiddleEnd.ControlFlowAnalysis
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.FunctionDFA

/// <summary>
/// Per-function DFA result.
/// </summary>
/// <remarks>
/// <c>Address</c> is function address.
/// <c>Name</c> is recovered function name.
/// <c>CFG</c> is B2R2's <see cref="B2R2.MiddleEnd.ControlFlowGraph.SSACFG" />.
/// <c>RetAddresses</c> stores addresses that B2R2 identifies as
/// return instructions in the original function CFG.
/// <c>DFAResult</c> is derived from B2R2's data-flow framework in
/// <see cref="B2R2.MiddleEnd.DataFlow" />.
/// <c>Callees</c> is mapping from callsite to callee address. This is derived
/// from B2R2's control-flow framework in
/// <see cref="B2R2.MiddleEnd.ControlFlowAnalysis" />.
/// </remarks>
type FunctionDFAResult =
  { Address: Addr
    Name: string
    CFG: SSACFG
    RetAddresses: Set<Addr>
    DFAResult: FunctionDFA
    Callees: Map<Addr, Set<Addr>> }

/// <summary>
/// DFA result of entire binary.
/// </summary>
/// <remarks>
/// <c>Binary</c> is loaded binary from
/// <see cref="PointerAnalyzer.Frontend.BinaryLoader.LoadedBinary">.
/// <c>Functions</c> is per-function DFA result.
/// <c>VisitOrder</c> sorts functions from callee to caller.
/// </remarks>
type ProgramDFAResult =
  { Binary: LoadedBinary
    Functions: Map<Addr, FunctionDFAResult>
    VisitOrder: Addr list }

/// Recovered LowUIR functions which can be lifted to SSA more than once.
type RecoveredProgram =
  { Binary: LoadedBinary
    Functions: Function list }

module ProgramDFA =
  /// Collect normal return instructions before lifting the CFG to SSA.
  let private extractRetAddr (function_: Function) =
    function_.CFG.Exits
    |> Seq.choose (fun vertex ->
      let block = vertex.VData.Internals

      if block.IsAbstract || not block.LastInstruction.IsRET then
        None
      else
        Some block.LastInstruction.Address)
    |> Set.ofSeq

  (* Integrate callSite |-> Callee Mapping from Control-Flow Analysis of B2R2 *)
  let private callees (function_: Function) =
    let updateCalleeMap calleeMap (KeyValue (callSite: CallSite, callee)) =
      let targets =
        match callee with
        | RegularCallee target -> Set.singleton target
        | IndirectCallees targets -> targets
        | SyscallCallee _ // Maybe handle syscall?
        | UnresolvedIndirectCallees
        | NullCallee -> Set.empty

      if Set.isEmpty targets then
        calleeMap
      else
        Map.add callSite.CallSiteAddress targets calleeMap

    if isNull function_.Callees then
      Map.empty
    else
      function_.Callees |> Seq.fold updateCalleeMap Map.empty

  /// From all callees, filtering only internal functions
  let private internalCallees (funcs: Map<Addr, FunctionDFAResult>) func =
    let calleeSeq = func.Callees |> Map.toSeq |> Seq.collect (snd >> Set.toSeq)

    let internalFuncs =
      calleeSeq
      |> Seq.filter (fun address -> Map.containsKey address funcs)
      |> Set.ofSeq

    internalFuncs

  /// Sort functions from Callee to Caller.
  /// The modular analysis is processed with this order.
  let private revDFS functions =
    let rec dfs address (visited, visitOrder) =
      if Set.contains address visited then
        visited, visitOrder
      else
        let newVisited = Set.add address visited
        let function_ = Map.find address functions

        let calleeSet = internalCallees functions function_

        let visited, visitOrder =
          Set.fold
            (fun acc callee -> dfs callee acc)
            (newVisited, visitOrder)
            calleeSet

        visited, address :: visitOrder

    let funcAddrSeq = functions |> Map.toSeq |> Seq.map fst

    funcAddrSeq
    |> Seq.fold (fun acc address -> dfs address acc) (Set.empty, [])
    |> snd
    |> List.rev

  /// Recover LowUIR CFGs once so different SSA lifters can reuse them.
  let recover binary =
    let brew = BinaryBrew binary.Handle

    let functions =
      brew.Functions.Sequence
      |> Seq.filter (fun function_ -> not function_.IsExternal)
      |> Seq.toList

    { Binary = binary; Functions = functions }

  /// Lift recovered functions with the supplied SSA lifter and run DFA.
  let buildWithLifter (lifter: ISSALiftable) recovered =
    let binary = recovered.Binary

    (* Run DFA on single function, construct FunctionDFAResult *)
    let constrFunDFA (func: Function) =
      let cfg = lifter.Lift func.CFG
      let dfaResult = FunctionDFA.create binary.Handle cfg

      func.EntryPoint,
      { Address = func.EntryPoint
        Name = func.Name
        CFG = cfg
        RetAddresses = extractRetAddr func
        DFAResult = dfaResult
        Callees = callees func }

    let functionMap =
      recovered.Functions
      |> Seq.map constrFunDFA
      |> Map.ofSeq

    let visitOrder = revDFS functionMap

    { Binary = binary
      Functions = functionMap
      VisitOrder = visitOrder }

  /// Lift recovered functions with B2R2's original SSA behavior.
  let build recovered =
    let lifter = SSALifterFactory.Create recovered.Binary.Handle
    buildWithLifter lifter recovered

  /// Recover every function, lift to SSA, and run DFA.
  let runDFA binary = binary |> recover |> build
