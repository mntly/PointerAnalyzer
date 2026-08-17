module PointerAnalyzer.Analysis.Analyzer

open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.Analysis.StmtEval

/// <summary>
/// Result of each modular analysis.
/// </summary>
/// <remarks>
/// <c>FinalState</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.AbsDom.AnalysisState.AnalysisState" />.
/// <c>LeafStates</c> stores the final register states at each leaf node of CFG.
/// <c>TypeConstraints</c> is set of
/// <see cref="PointerAnalyzer.AbsDom.TypeConstraint.TypeConstraint" />.
/// <c>TypeConflicts</c> is set of Type Ids to indicates which type Ids are
/// inferred to both address and value.
/// </remarks>
type AnalysisResult =
  { FinalState: AnalysisState
    LeafStates: Map<int, AnalysisState>
    TypeConstraints: ConstraintSet
    TypeConflicts: Set<TypeId> }

/// <summary>
/// Cumulative analysis state during iterating basic blocks.
/// </summary>
/// <remarks>
/// <c>State</c> is cumulative analysis state.
/// <c>Visitied</c> keeps visited blocks to do not re-evaluate blocks.
/// <c>LeafStates</c> tracks the final register states at each leaf node of CFG.
/// </remarks>
type private RunResult =
  { State: AnalysisState
    Visited: Set<int>
    LeafStates: Map<int, AnalysisState> }

type AnalyzerModule
  (platform: Platform, startTypeId: TypeId, config: StmtEvalConfig) =
  let stateDom =
    AnalysisStateDomain.createWithProvenance
      platform
      startTypeId
      config.TrackTypeProvenance

  let stmtEval = StmtEvalDomain.createWithConfig platform config

  new (platform: Platform) = AnalyzerModule (platform, 0, StmtEvalConfig.empty)

  new (platform: Platform, config: StmtEvalConfig) =
    AnalyzerModule (platform, 0, config)

  member __.InitialState =
    match config.InitialStackPointer with
    | Some stackPointer ->
      stateDom.initializeStackPointer stackPointer stateDom.bot
    | None -> stateDom.bot

  /// Analyze one block. Transfer the statements in given block and collect
  /// type constraints.
  member private _.runBlock state (block: IVertex<SSABasicBlock>) =
    (* Get ReturnStatus of given block *)
    (* ReturnStatus is used for detecting `ret` of FunctionAbstraction *)
    let context =
      if block.VData.Internals.IsAbstract then
        AbstractBlock block.VData.Internals.AbstractContent.ReturningStatus
      else
        NormalBlock

    let statements = block.VData.Internals.Statements

    let rec runBlockInner index state =
      if index < statements.Length then
        let programPoint, stmt = statements[index]

        match stmtEval.Eval context programPoint stmt state with
        | [ { Target = Next; State = nextState } ] ->
          runBlockInner (index + 1) nextState
        | results -> results
      else
        [ { Target = Next; State = state } ]

    runBlockInner 0 state

  /// According to next target, resolve the next instruction; jump target,
  /// right next instruction, ...
  member private _.TryResolveTarget
    (cfg: SSACFG)
    (block: IVertex<SSABasicBlock>)
    transferTarget
    : IVertex<SSABasicBlock> option =
    let successors = cfg.GetSuccs block

    let tryFindAddress address =
      successors
      |> Array.tryFind (fun successor ->
        successor.VData.Internals.BlockAddress = address)

    (*
      Among sucessed block, select one block with same address and same edge
      kind
    *)
    let tryFindEdge edgeKind address =
      cfg.GetSuccEdges block
      |> Array.tryFind (fun edge -> edge.Label = edgeKind)
      |> Option.map (fun edge -> edge.Second)
      |> Option.filter (fun successor ->
        successor.VData.Internals.BlockAddress = address)

    let tryFindTarget edgeKind address =
      match edgeKind with
      | Some edgeKind -> tryFindEdge edgeKind address
      | None -> tryFindAddress address

    match transferTarget with
    | LabelTarget (label, edgeKind) -> tryFindTarget edgeKind label.Address
    | CallTarget address ->
      (* Jump to Fake Node that represents callee *)
      successors
      |> Array.tryFind (fun successor ->
        successor.VData.Internals.IsAbstract
        && successor.VData.Internals.AbstractContent.EntryPoint = address)
    | InterTarget (value, edgeKind) ->
      stateDom.AbsVal.tryGetUInt64 value |> Option.bind (tryFindTarget edgeKind)
    | AbstractionReturn ->
      (* Return from callee: Back to caller, not just jump target *)
      (* Sometimes, jump target is out of caller. *)
      cfg.GetSuccEdges block
      |> Array.tryFind (fun edge -> edge.Label = CFGEdgeKind.RetEdge)
      |> Option.map (fun edge -> edge.Second)
    | Terminated -> None
    | Next ->
      match successors with
      | [| successor |] -> Some successor
      | _ ->
        cfg.GetSuccEdges block
        |> Array.tryFind (fun edge -> edge.Label = CFGEdgeKind.FallThroughEdge)
        |> Option.map (fun edge -> edge.Second)

  /// Check whether the given leaf block ends in a return instruction that
  /// B2R2 identified in the original function CFG.
  member private _.IsReturnLeaf
    (cfg: SSACFG)
    retAddresses
    (block: IVertex<SSABasicBlock>)
    =
    let internals = block.VData.Internals

    let terminalAddress =
      internals.Statements
      |> Array.tryLast
      |> Option.map (fun (programPoint, _) -> programPoint.Address)

    not internals.IsAbstract
    && (cfg.GetSuccs block |> Array.isEmpty)
    && Option.exists
      (fun address -> Set.contains address retAddresses)
      terminalAddress

  /// Merge leaf states keyed by B2R2 CFG block id.
  /// Since one block evaluated only once, there does not exist the evaluation
  /// result from same block.
  member private _.MergeLeafStates left right =
    Map.fold (fun acc blockId state -> Map.add blockId state acc) left right

  /// Join analysis state by keeping TypeState, since TypeState is passed
  /// during analysis
  member private _.JoinNormal left right types =
    (* Helper for joining CurrentRegisters and CurrentStackSlots *)
    let joinCurrentTypeIds left right =
      let addRight result key rightTypeId =
        (*
          Only track the entry that
          1. Appears only on type Map
          2. Same type Id with same key
        *)
        match Map.tryFind key result with
        | None -> Map.add key rightTypeId result
        | Some leftTypeId when leftTypeId = rightTypeId -> result
        | Some _ -> Map.remove key result

      Map.fold addRight left right

    { RegMap = stateDom.RegMap.join left.RegMap right.RegMap
      Memory = stateDom.AbsMem.join left.Memory right.Memory
      Types = types
      CurrentRegisters =
        joinCurrentTypeIds left.CurrentRegisters right.CurrentRegisters
      CurrentRegisterValues =
        right.CurrentRegisterValues
        |> Map.fold
          (fun acc registerId rightValue ->
            match Map.tryFind registerId acc with
            | Some leftValue ->
              Map.add
                registerId
                (stateDom.AbsVal.join leftValue rightValue)
                acc
            | None -> Map.add registerId rightValue acc)
          left.CurrentRegisterValues
      CurrentStackSlots =
        joinCurrentTypeIds left.CurrentStackSlots right.CurrentStackSlots
      PendingRegisterOutputs =
        right.PendingRegisterOutputs
        |> Map.fold
          (fun acc regId typeId -> Map.add regId typeId acc)
          left.PendingRegisterOutputs
      StackPointer = StackPointerState.join left.StackPointer right.StackPointer }

  /// Analyze given CFG (entire binary) and return AnalysisState
  /// collected TypeConstraint
  member this.analyze (cfg: SSACFG) retAddresses =
    (* Recursively analyze from given block *)
    let rec run (block: IVertex<SSABasicBlock>) inputState visited =
      if Set.contains block.ID visited then
        (* Already evalutated. Do not evaluate more *)
        { State = inputState
          Visited = visited
          LeafStates = Map.empty }
      else
        (*
          Check given target address in transfer is valid and move to evaluate
          next block. The validity was given by B2R2.
        *)
        let transNext (result: RunResult) (transfer: TransferResult) =
          (*
            The Analysis State used for evaluating next block is in `transfer`.
            However, the type Id is monotonly increased and it should be handle
            global, only Type State is proppagated to next evaluation.
          *)
          let newState =
            { transfer.State with
                Types = result.State.Types }

          (*
            Check given successor has ret instruction when current jmp
            instruction is ret of FunctionAbstraction
          *)
          let isNoRetCall (successor: IVertex<SSABasicBlock>) =
            match transfer.Target with
            | CallTarget _ when successor.VData.Internals.IsAbstract ->
              successor.VData.Internals.AbstractContent.ReturningStatus = NoRet
            | _ -> false

          (*
            If current block is leaf node, ends up evaluation.
            If not, evaluate to next successed block.
            Setting return register as final register of leaf node is done by
            out of this function.
          *)
          let transResult =
            match this.TryResolveTarget cfg block transfer.Target with
            | Some successor when isNoRetCall successor ->
              (*
                The call summary has already connected the arguments. A
                non-returning abstraction has no continuation to evaluate.
              *)
              { State = newState
                Visited = result.Visited
                LeafStates = Map.empty }
            | Some successor -> run successor newState result.Visited
            | None ->
              (* Next target is invalid: Successor of leaf node *)
              { State = newState
                Visited = result.Visited
                LeafStates = Map.empty }

          (*
            Merge analysis result of successed block to tracked DS(result.State)
          *)
          let retState =
            this.JoinNormal
              result.State
              transResult.State
              transResult.State.Types

          (*
            Merge Leaf states induced from successed block and tracked D
            (result.LeafStates)
          *)
          let retLeafState =
            this.MergeLeafStates result.LeafStates transResult.LeafStates

          { State = retState
            Visited = transResult.Visited
            LeafStates = retLeafState }

        (* Insert current block to visited block *)
        let visited = Set.add block.ID visited

        (*
          Evaluate given block and get next instruction to evalute and
          AnalysisState used for evaluation
        *)
        let transfers = this.runBlock inputState block

        (* Extract updated TypeState *)
        (* TypeState is managed globally, so it must same among all results *)
        let typeState =
          transfers |> List.head |> (fun transfer -> transfer.State.Types)

        (* Store final register states as return register states. *)
        let leafStates =
          if this.IsReturnLeaf cfg retAddresses block then
            match transfers with
            | transfer: TransferResult :: _ ->
              (*
                If current block is leaf node, then there exist only one entry
                in the result of evaluation: No more than return address to
                execute.
              *)
              Map.empty |> Map.add block.ID transfer.State
            | [] -> Map.empty
          else
            Map.empty

        (* Evaluate next blocks successed from current block *)
        let result =
          List.fold
            transNext
            { State = { inputState with Types = typeState }
              Visited = visited
              LeafStates = leafStates }
            (transfers: TransferResult list)

        result

    (* Start evaluating from entry block *)
    let runRoot (result: RunResult) root =
      let rootStateInput =
        { this.InitialState with
            Types = result.State.Types }

      let rootResult = run root rootStateInput result.Visited

      let state =
        this.JoinNormal result.State rootResult.State rootResult.State.Types

      { State = state
        Visited = rootResult.Visited
        LeafStates =
          this.MergeLeafStates result.LeafStates rootResult.LeafStates }

    let initialResult =
      { State = this.InitialState
        Visited = Set.empty
        LeafStates = Map.empty }

    let result = Array.fold runRoot initialResult cfg.Roots
    result.State, result.LeafStates

module AnalyzerDomain =
  let createWithStart platform startTypeId config =
    AnalyzerModule (platform, startTypeId, config)

  let createWithConfig platform config = createWithStart platform 0 config

  let create platform = AnalyzerModule platform

  let createFromString platform =
    PointerAnalyzer.Platform.Platform.ofString platform |> create

  /// Main-Analysis
  let analyzeRawWithStart platform startTypeId config cfg retAddresses =
    let analyzer = createWithStart platform startTypeId config
    let finalState, leafStates = analyzer.analyze cfg retAddresses

    { FinalState = finalState
      LeafStates = leafStates
      TypeConstraints = finalState.Types.Constraints
      TypeConflicts = finalState.Types.Conflicts }

  /// Main-Analysis and Constraint Solving process
  let analyzeWithStart platform startTypeId config cfg retAddresses =
    (* Currently, this function is not used *)
    let result =
      analyzeRawWithStart platform startTypeId config cfg retAddresses

    let stateDomain =
      AnalysisStateDomain.createWithProvenance
        platform
        startTypeId
        config.TrackTypeProvenance

    (* Solve type constraints *)
    let solvedState =
      { result.FinalState with
          Types = stateDomain.TypeState.solve result.FinalState.Types }

    { FinalState = solvedState
      LeafStates = result.LeafStates
      TypeConstraints = solvedState.Types.Constraints
      TypeConflicts = solvedState.Types.Conflicts }

  let analyze platform config cfg retAddresses =
    analyzeWithStart platform 0 config cfg retAddresses
