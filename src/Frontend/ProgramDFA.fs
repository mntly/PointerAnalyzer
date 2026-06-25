module PointerAnalyzer.Frontend.ProgramDFA

open B2R2
open B2R2.MiddleEnd
open B2R2.MiddleEnd.ControlFlowAnalysis
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.FunctionDFA

type FunctionDFAResult =
  { Address: Addr
    Name: string
    CFG: SSACFG
    DFAResult: FunctionDFA
    Callees: Map<Addr, Set<Addr>> }

type ProgramDFAResult =
  { Binary: LoadedBinary
    Functions: Map<Addr, FunctionDFAResult> }

module ProgramRecovery =
  let private callees (function_: Function) =
    if isNull function_.Callees then
      Map.empty
    else
      function_.Callees
      |> Seq.fold
        (fun acc (KeyValue (callSite, callee)) ->
          let targets =
            match callee with
            | RegularCallee target -> Set.singleton target
            | IndirectCallees targets -> targets
            | SyscallCallee _ // Maybe handle syscall?
            | UnresolvedIndirectCallees
            | NullCallee -> Set.empty

          if Set.isEmpty targets then
            acc
          else
            Map.add callSite.CallSiteAddress targets acc)
        Map.empty

  let recover binary =
    let brew = BinaryBrew binary.Handle
    let lifter = SSALifterFactory.Create binary.Handle

    let functionMap =
      brew.Functions.Sequence
      |> Seq.filter (fun function_ -> not function_.IsExternal)
      |> Seq.map (fun function_ ->
        let cfg = lifter.Lift function_.CFG
        let dfaResult = FunctionDFA.create binary.Handle cfg

        function_.EntryPoint,
        { Address = function_.EntryPoint
          Name = function_.Name
          CFG = cfg
          DFAResult = dfaResult
          Callees = callees function_ })
      |> Map.ofSeq

    { Binary = binary
      Functions = functionMap }
