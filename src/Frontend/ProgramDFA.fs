module PointerAnalyzer.Frontend.ProgramDFA

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd
open B2R2.MiddleEnd.ControlFlowAnalysis
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Frontend.B2R2Diagnostics
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
/// <c>B2R2Diagnostics</c> propagate the unsupported instruction by B2R2.
/// </remarks>
type ProgramDFAResult =
  { Binary: LoadedBinary
    Functions: Map<Addr, FunctionDFAResult>
    VisitOrder: Addr list
    B2R2Diagnostics: UnsupportedInstInfo list }

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

  /// For each functions in binary, process DFA and integrate them
  let runDFA binary =
    (* Raise with B2R2AnalysisException *)
    let raiseWithContext stage function_ cause =
      (* Extract the source of exception *)
      let address, name =
        match function_ with
        | Some (func: Function) -> Some func.EntryPoint, Some func.Name
        | None -> None, None

      raise (
        B2R2AnalysisException (
          binary.Path,
          stage,
          address,
          name,
          cause
        )
      )

    let brew =
      try
        BinaryBrew binary.Handle
      with cause ->
        raiseWithContext CFGRecovery None cause

    let lifter = SSALifterFactory.Create binary.Handle

    (* Used for represent the assembly of unsurpported instruction *)
    let instructionLifter = binary.Handle.NewLiftingUnit ()
    let instructionText (address: Addr) =
      match instructionLifter.TryParseInstruction address with
      | Ok instruction ->
        try
          instruction.Disasm ()
        with _ ->
          sprintf "<disassembly unavailable at 0x%08x>" address
      | Error error -> sprintf "<instruction unavailable: %A>" error

    (* Extract B2R2 unsupported instruction *)
    let unsupportedInstructions (func: Function) (cfg: SSACFG) =
      cfg.Vertices
      |> Seq.collect (fun vertex -> vertex.VData.Internals.Statements)
      |> Seq.choose (fun (programPoint, statement) ->
        match statement with
        | SideEffect UnsupportedInstruction when programPoint <> ProgramPoint.Fake ->
          Some
            { FunctionAddress = func.EntryPoint
              FunctionName = func.Name
              ProgramPoint = programPoint
              Instruction = instructionText programPoint.Address }
        | _ -> None)
      |> Seq.distinctBy (fun diagnostic ->
        diagnostic.FunctionAddress, diagnostic.ProgramPoint.Address)
      |> Seq.toList

    (* Run DFA on single function, construct FunctionDFAResult *)
    (* This also extract unsupported instruction by B2R2 *)
    let constrFunDFA (func: Function) =
      let cfg =
        try
          lifter.Lift func.CFG
        with cause ->
          raiseWithContext SSALifting (Some func) cause

      let dfaResult =
        try
          FunctionDFA.create binary.Handle cfg
        with cause ->
          raiseWithContext DataFlowAnalysis (Some func) cause

      (func.EntryPoint,
       { Address = func.EntryPoint
         Name = func.Name
         CFG = cfg
         RetAddresses = extractRetAddr func
         DFAResult = dfaResult
         Callees = callees func }),
      unsupportedInstructions func cfg

    let functionResults =
      brew.Functions.Sequence
      |> Seq.filter (fun function_ -> not function_.IsExternal)
      |> Seq.map constrFunDFA
      |> Seq.toList

    let functionMap = functionResults |> List.map fst |> Map.ofList

    let diagnostics =
      functionResults
      |> List.collect snd
      |> List.sortBy (fun diagnostic ->
        diagnostic.FunctionAddress, diagnostic.ProgramPoint.Address)

    let visitOrder = revDFS functionMap

    { Binary = binary
      Functions = functionMap
      VisitOrder = visitOrder
      B2R2Diagnostics = diagnostics }
