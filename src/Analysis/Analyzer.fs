module PointerAnalyzer.Analysis.Analyzer

open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.Analysis.StmtEval

type AnalysisResult =
  { FinalState: AnalysisState
    TypeConstraints: ConstraintSet
    TypeConflicts: Set<TypeId> }

type AnalyzerModule (architecture: Architecture, config: StmtEvalConfig) =
  let stateDom = AnalysisStateDomain.createDefault architecture
  let stmtEval = StmtEvalDomain.createWithConfig architecture config

  new (architecture: Architecture) =
    AnalyzerModule (architecture, StmtEvalConfig.empty)

  member __.InitialState = stateDom.bot

  member __.EvalStmt state stmt = stmtEval.Eval stmt state

  member private _.runBlock state stmts =
    let rec runBlockInner state lst =
      match lst with
      | (_, stmt) :: tl ->
        match stmtEval.Eval stmt state with
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
      Types = types }

  member this.analyze (cfg: SSACFG) =
    let rec run (block: IVertex<SSABasicBlock>) inputState visited =
      if Set.contains block.ID visited then
        inputState, visited
      else
        let visited = Set.add block.ID visited

        let transfers =
          this.runBlock inputState block.VData.Internals.Statements

        let typeState =
          transfers |> List.head |> (fun transfer -> transfer.State.Types)

        let resultState, resultVisited =
          List.fold
            (fun (absState, visited) transfer ->
              let newState =
                { transfer.State with
                    Types = absState.Types }

              let transStateRet, transVisitedRet =
                match this.TryResolveTarget cfg block transfer.Target with
                | Some successor -> run successor newState visited
                | None -> newState, visited

              let retState =
                this.JoinNormal absState transStateRet transStateRet.Types

              retState, transVisitedRet)
            ({ stateDom.bot with Types = typeState }, visited)
            transfers

        resultState, resultVisited

    ((this.InitialState, Set.empty), cfg.Roots)
    ||> Array.fold (fun (state, visited) root ->
      let rootStateInput =
        { stateDom.bot with
            Types = state.Types }

      let rootResult, visited = run root rootStateInput visited
      this.JoinNormal state rootResult rootResult.Types, visited)
    |> fst

module AnalyzerDomain =
  let createWithConfig architecture config =
    AnalyzerModule (architecture, config)

  let create architecture = AnalyzerModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create

  let analyze architecture config cfg =
    let analyzer = createWithConfig architecture config
    let finalState = analyzer.analyze cfg
    let stateDomain = AnalysisStateDomain.createDefault architecture

    let solvedState =
      { finalState with
          Types = stateDomain.TypeState.solve finalState.Types }

    { FinalState = solvedState
      TypeConstraints = solvedState.Types.Constraints
      TypeConflicts = solvedState.Types.Conflicts }
