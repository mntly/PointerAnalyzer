module PointerAnalyzer.Analysis.Analyzer

open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.Analysis.StmtEval

type AnalysisResult =
  { FinalState: AnalysisState
    TypeConstraints: ConstraintSet
    TypeConflicts: Set<TypeId> }

  member this.ConstraintsToString = ConstraintSet.toString this.TypeConstraints

  member this.ConflictToString =
    let header = "  Conflicts:\n"

    let content =
      if Set.isEmpty this.TypeConflicts then
        "    <empty>"
      else
        this.TypeConflicts |> Set.map (sprintf "    t%d\n") |> String.concat ""

    header + content

type AnalyzerModule
  (platform: Platform, startTypeId: TypeId, config: StmtEvalConfig) =
  let stateDom = AnalysisStateDomain.create platform startTypeId
  let stmtEval = StmtEvalDomain.createWithConfig platform config

  new (platform: Platform) = AnalyzerModule (platform, 0, StmtEvalConfig.empty)

  new (platform: Platform, config: StmtEvalConfig) =
    AnalyzerModule (platform, 0, config)

  member __.InitialState = stateDom.bot

  member private _.runBlock state (stmts: (B2R2.ProgramPoint * Stmt) array) =
    let rec runBlockInner state (lst: (B2R2.ProgramPoint * Stmt) list) =
      match lst with
      | (programPoint, stmt) :: tl ->
        match stmtEval.Eval programPoint stmt state with
        | [ { Target = Next; State = nextState } ] -> runBlockInner nextState tl
        | results -> results
      | [] -> [ { Target = Next; State = state } ]

    let stmtsLst = Array.toList stmts

    runBlockInner state stmtsLst

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

    match transferTarget with
    | LabelTarget label -> tryFindAddress label.Address
    | InterTarget value ->
      stateDom.AbsVal.tryGetUInt64 value |> Option.bind tryFindAddress
    | Next ->
      match successors with
      | [| successor |] -> Some successor
      | _ ->
        cfg.GetSuccEdges block
        |> Array.tryFind (fun edge -> edge.Label = CFGEdgeKind.FallThroughEdge)
        |> Option.map (fun edge -> edge.Second)

  member private _.JoinNormal left right types =
    { RegMap = stateDom.RegMap.join left.RegMap right.RegMap
      Memory = stateDom.AbsMem.join left.Memory right.Memory
      Types = types
      PendingReturns =
        right.PendingReturns
        |> Map.fold
          (fun acc regId typeId -> Map.add regId typeId acc)
          left.PendingReturns
      StackDelta =
        match left.StackDelta, right.StackDelta with
        | Some left, Some right when left = right -> Some left
        | Some delta, None
        | None, Some delta -> Some delta
        | _ -> None }

  member this.analyze (cfg: SSACFG) =
    let rec run (block: IVertex<SSABasicBlock>) inputState visited =
      if Set.contains block.ID visited then
        inputState, visited
      else
        let transNext (absState, visited) transfer =
          let newState =
            { transfer.State with
                Types = absState.Types }

          let transStateRet, transVisitedRet =
            match this.TryResolveTarget cfg block transfer.Target with
            | Some successor -> run successor newState visited
            | None -> newState, visited

          let retState =
            this.JoinNormal absState transStateRet transStateRet.Types

          retState, transVisitedRet

        let visited = Set.add block.ID visited

        let transfers =
          this.runBlock inputState block.VData.Internals.Statements

        let typeState =
          transfers |> List.head |> (fun transfer -> transfer.State.Types)

        let resultState, resultVisited =
          List.fold
            transNext
            ({ inputState with Types = typeState }, visited)
            transfers

        resultState, resultVisited

    let runRoot (state, visited) root =
      let rootStateInput =
        { this.InitialState with
            Types = state.Types }

      let rootResult, visited = run root rootStateInput visited
      this.JoinNormal state rootResult rootResult.Types, visited

    Array.fold runRoot (this.InitialState, Set.empty) cfg.Roots |> fst

module AnalyzerDomain =
  let createWithStart platform startTypeId config =
    AnalyzerModule (platform, startTypeId, config)

  let createWithConfig platform config = createWithStart platform 0 config

  let create platform = AnalyzerModule platform

  let createFromString platform =
    PointerAnalyzer.Platform.Platform.ofString platform |> create

  let analyzeRawWithStart platform startTypeId config cfg =
    let analyzer = createWithStart platform startTypeId config
    let finalState = analyzer.analyze cfg

    { FinalState = finalState
      TypeConstraints = finalState.Types.Constraints
      TypeConflicts = finalState.Types.Conflicts }

  let analyzeWithStart platform startTypeId config cfg =
    let result = analyzeRawWithStart platform startTypeId config cfg
    let stateDomain = AnalysisStateDomain.create platform startTypeId

    let solvedState =
      { result.FinalState with
          Types = stateDomain.TypeState.solve result.FinalState.Types }

    { FinalState = solvedState
      TypeConstraints = solvedState.Types.Constraints
      TypeConflicts = solvedState.Types.Conflicts }

  let analyze platform config cfg = analyzeWithStart platform 0 config cfg
