module PointerAnalyzer.Frontend.ProgramDFA

open B2R2
open B2R2.MiddleEnd
open B2R2.MiddleEnd.ControlFlowAnalysis
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.FunctionDFA

/// <summary>
/// Per-function pre-analysis result.
/// </summary>
/// <remarks>
/// <c>Address</c> is function address.
/// <c>Name</c> is recovered function name.
/// <c>CFG</c> is B2R2's <see cref="B2R2.MiddleEnd.ControlFlowGraph.SSACFG" />.
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
    DFAResult: FunctionDFA
    Callees: Map<Addr, Set<Addr>> }

/// <summary>
/// Pre-analysis result of entire binary.
/// </summary>
/// <remarks>
/// <c>Binary</c> is loaded binary from
/// <see cref="PointerAnalyzer.Frontend.BinaryLoader.LoadedBinary">.
/// <c>Functions</c> is per-function pre-analysis result.
/// </remarks>
type ProgramDFAResult =
  { Binary: LoadedBinary
    Functions: Map<Addr, FunctionDFAResult> }

module ProgramDFA =
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

  /// For each functions in binary, process DFA and integrate them
  let runDFA binary =
    let brew = BinaryBrew binary.Handle
    let lifter = SSALifterFactory.Create binary.Handle

    (* Run DFA on single function, construct FunctionDFAResult *)
    let constrFunDFA (func: Function) =
      let cfg = lifter.Lift func.CFG
      let dfaResult = FunctionDFA.create binary.Handle cfg

      func.EntryPoint,
      { Address = func.EntryPoint
        Name = func.Name
        CFG = cfg
        DFAResult = dfaResult
        Callees = callees func }

    let functionMap =
      (*
        ToDo
          Functions.Sequence misses some functions.
          Check rechability, and modify B2R2 as submodule.
      *)
      brew.Functions.Sequence
      |> Seq.filter (fun function_ -> not function_.IsExternal)
      |> Seq.map constrFunDFA
      |> Map.ofSeq

    { Binary = binary
      Functions = functionMap }
