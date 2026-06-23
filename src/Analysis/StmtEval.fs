module PointerAnalyzer.Analysis.StmtEval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.Analysis.ExprEval

type TransferTarget =
  | Next
  | LabelTarget of Label
  | InterTarget of AbsVal

type TransferResult =
  { Target: TransferTarget
    State: AnalysisState }

type PointerUse = Variable -> bool

type ConstValue = Variable -> BitVector option

type StmtEvalConfig =
  { PointerUse: PointerUse
    ConstValue: ConstValue
    ClassifyConstant: BitVector -> ConstantType }

module StmtEvalConfig =
  let empty =
    { PointerUse = fun _ -> false
      ConstValue = fun _ -> None
      ClassifyConstant = fun _ -> UnknownConstant }

type StmtEvalModule (architecture: Architecture, config: StmtEvalConfig) =

  let absVal = AbsValDomain.create architecture
  let stateDom = AnalysisStateDomain.createDefault architecture

  let exprEval =
    ExprEvalDomain.createWithConfig
      architecture
      { ClassifyConstant = config.ClassifyConstant }

  new (architecture: Architecture) =
    StmtEvalModule (architecture, StmtEvalConfig.empty)

  member private _.ensureTypeId typeId state =
    match typeId with
    | Some typeId -> typeId, state
    | None -> stateDom.freshTypeId state

  member private _.applyPointerHint variable typeId state =
    if config.PointerUse variable then
      stateDom.addAddress typeId state
    else
      state

  member private this.defReg variable value typeId state =
    let typeId, state = this.ensureTypeId typeId state
    let state = stateDom.setRegister variable value typeId state
    state |> this.applyPointerHint variable typeId

  member private this.evalDefinition variable expr state =
    let evaluatedValue, typeId, state = exprEval.Eval state expr

    let value =
      match config.ConstValue variable with
      | Some constant -> absVal.ofBitVector constant
      | None -> evaluatedValue

    this.defReg variable value typeId state

  member private _.evalMemoryDefinition newMem expr state =
    match expr with
    | Store (prevMem, _, addressExpr, valueExpr) ->
      let _, _, state =
        exprEval.EvalStore
          state
          newMem.Identifier
          prevMem.Identifier
          addressExpr
          valueExpr

      state
    | _ ->
      let _, _, state = exprEval.Eval state expr
      state

  member private _.phiSource destVar sourceId state =
    let sourceVar = { destVar with Identifier = sourceId }

    let typeId, state = stateDom.getOrFreshTypeId sourceVar state

    match stateDom.tryFindRegister sourceVar state with
    | Some value -> value, typeId, state
    | None ->
      match config.ConstValue sourceVar with
      | Some constant ->
        let value = absVal.ofBitVector constant
        value, typeId, stateDom.setRegister sourceVar value typeId state
      | None -> absVal.bot, typeId, state

  member private this.evalPhi variable sourceIds state =
    let values, sourceTypeIds, state =
      (([], [], state), sourceIds)
      ||> Array.fold (fun (values, typeIds, state) sourceId ->
        let value, typeId, state = this.phiSource variable sourceId state
        value :: values, typeId :: typeIds, state)

    let value = List.fold absVal.join absVal.bot values
    let destinationTypeId, state = stateDom.getOrFreshTypeId variable state

    state
    |> stateDom.addSame (destinationTypeId :: sourceTypeIds)
    |> stateDom.setRegister variable value destinationTypeId
    |> this.applyPointerHint variable destinationTypeId

  member this.Eval (stmt: B2R2.BinIR.SSA.Stmt) state : TransferResult list =
    printfn "Stmt: %s" (PrettyPrinter.ToString [| stmt |])

    let results =
      match stmt with
      | LMark _ -> [ { Target = Next; State = state } ]

      | Def ({ Kind = MemVar } as resultMem, expr) ->
        [ { Target = Next
            State = this.evalMemoryDefinition resultMem expr state } ]

      | Def (variable, expr) ->
        [ { Target = Next
            State = this.evalDefinition variable expr state } ]

      // ToDo! Need to Check
      | Phi ({ Kind = MemVar }, _) -> [ { Target = Next; State = state } ]

      | Phi (variable, sourceIds) ->
        [ { Target = Next
            State = this.evalPhi variable sourceIds state } ]

      | Jmp (IntraJmp label) ->
        [ { Target = LabelTarget label
            State = state } ]

      | Jmp (IntraCJmp (conditionExpr, trueLabel, falseLabel)) ->
        let _, conditionTypeId, state = exprEval.Eval state conditionExpr

        let state =
          match conditionTypeId with
          | Some typeId -> stateDom.addValue typeId state
          | None -> state

        [ { Target = LabelTarget trueLabel
            State = state }
          { Target = LabelTarget falseLabel
            State = state } ]

      | Jmp (InterJmp targetExpr) ->
        let target, targetTypeId, state = exprEval.Eval state targetExpr

        let state =
          match targetTypeId with
          | Some typeId -> stateDom.addAddress typeId state
          | None -> state

        [ { Target = InterTarget target
            State = state } ]

      | Jmp (InterCJmp (conditionExpr, trueExpr, falseExpr)) ->
        let _, conditionTypeId, state = exprEval.Eval state conditionExpr
        let trueTarget, trueTypeId, state = exprEval.Eval state trueExpr
        let falseTarget, falseTypeId, state = exprEval.Eval state falseExpr

        let state =
          state
          |> (fun state ->
            match conditionTypeId with
            | Some typeId -> stateDom.addValue typeId state
            | None -> state)
          |> (fun state ->
            match trueTypeId with
            | Some typeId -> stateDom.addAddress typeId state
            | None -> state)
          |> (fun state ->
            match falseTypeId with
            | Some typeId -> stateDom.addAddress typeId state
            | None -> state)

        [ { Target = InterTarget trueTarget
            State = state }
          { Target = InterTarget falseTarget
            State = state } ]

      // Need to Apply Callee Function
      | ExternalCall (calleeExpr, _, _) ->
        let _, calleeTypeId, state = exprEval.Eval state calleeExpr

        let state =
          match calleeTypeId with
          | Some typeId -> stateDom.addAddress typeId state
          | None -> state

        [ { Target = Next; State = state } ]

      | SideEffect _ -> [ { Target = Next; State = state } ]

    let addedConstraints =
      results
      |> Seq.collect (fun result ->
        Set.difference result.State.Types.Constraints state.Types.Constraints)
      |> Set.ofSeq

    if Set.isEmpty addedConstraints then
      printfn "  Added constraints: <none>"
    else
      printfn "  Added constraints:"

      addedConstraints
      |> Set.iter (stateDom.TypeState.constraintToString >> printfn "    %s")

    results

module StmtEvalDomain =
  let createWithConfig architecture config =
    StmtEvalModule (architecture, config)

  let create architecture = StmtEvalModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
