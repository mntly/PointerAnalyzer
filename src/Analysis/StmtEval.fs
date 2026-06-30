module PointerAnalyzer.Analysis.StmtEval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.Analysis.ExprEval
open PointerAnalyzer.Frontend.FunctionDFA

type TransferTarget =
  | Next
  | LabelTarget of Label
  | InterTarget of AbsVal

type TransferResult =
  { Target: TransferTarget
    State: AnalysisState }

type PointerUse = Variable -> bool

type ConstValue = Variable -> BitVector option

type ApplyCallSummary =
  ProgramPoint
    -> Variable list
    -> Variable list
    -> AnalysisState
    -> AnalysisState option

type StmtEvalConfig =
  { PointerUse: PointerUse
    ConstValue: ConstValue
    ClassifyConstant: BitVector -> ConstantType
    StackPointer: RegisterID option
    ApplyCallSummary: ApplyCallSummary
    Debug: bool }

module StmtEvalConfig =
  let empty =
    { PointerUse = fun _ -> false
      ConstValue = fun _ -> None
      ClassifyConstant = fun _ -> UnknownConstant
      StackPointer = None
      ApplyCallSummary = fun _ _ _ _ -> None
      Debug = false }

  let construct
    (funDFAResult: FunctionDFA)
    classifyConst
    sp
    applyCallee
    isDebug
    =
    { PointerUse = funDFAResult.PointerUse
      ConstValue = funDFAResult.ConstValue
      ClassifyConstant = classifyConst
      StackPointer = Some sp
      ApplyCallSummary = applyCallee
      Debug = isDebug }

type StmtEvalModule (platform: Platform, config: StmtEvalConfig) =

  let absVal = AbsValDomain.create platform
  let stateDom = AnalysisStateDomain.createDefault platform

  let exprEval =
    ExprEvalDomain.createWithConfig
      platform
      { ClassifyConstant = config.ClassifyConstant }

  new (platform: Platform) = StmtEvalModule (platform, StmtEvalConfig.empty)

  member private _.applyPointerHint variable typeId state =
    if config.PointerUse variable then
      stateDom.addAddress typeId state
    else
      state

  member private _.isStackPointer (variable: Variable) =
    match config.StackPointer, variable.Kind with
    | Some stackPointer, RegVar (_, registerId, _) -> registerId = stackPointer
    | _ -> false

  member private _.tryInt value =
    try
      let value = BitVector.ToUInt64 value

      if value <= uint64 System.Int32.MaxValue then
        Some (int value)
      else
        None
    with _ ->
      None

  member private this.tryStackDeltaChange (expr: Expr) =
    let isStackPointerExpr =
      function
      | Var variable when this.isStackPointer variable -> true
      | _ -> false

    let tryNum =
      function
      | Num value -> this.tryInt value
      | _ -> None

    match expr with
    | BinOp (BinOpType.SUB, _, left, right) when isStackPointerExpr left ->
      tryNum right
    | BinOp (BinOpType.ADD, _, left, right) when isStackPointerExpr left ->
      match tryNum right with
      | Some del -> Some -del
      | None -> None
    | BinOp (BinOpType.ADD, _, left, right) when isStackPointerExpr right ->
      match tryNum left with
      | Some del -> Some -del
      | None -> None
    | _ -> None

  member private this.updateStackDelta (variable: Variable) expr state =
    if this.isStackPointer variable then
      match this.tryStackDeltaChange expr with
      | Some delta -> stateDom.adjustStackDelta delta state
      | None -> { state with StackDelta = None }
    else
      state

  member private this.defReg (variable: Variable) value exprTypeId state =
    let typeId, state = stateDom.getOrFreshTypeId variable state

    let state =
      match exprTypeId with
      | Some exprTypeId -> stateDom.addSame [ typeId; exprTypeId ] state
      | None -> state

    let pendingReturn, state = stateDom.consumePendingReturn variable state

    let state =
      match pendingReturn with
      | Some returnTypeId -> stateDom.addSame [ typeId; returnTypeId ] state
      | None -> state

    let state = stateDom.setRegister variable value typeId state
    state |> this.applyPointerHint variable typeId

  member private this.evalDefinition (variable: Variable) expr state =
    let evaluatedValue, typeId, state = exprEval.Eval state expr

    let value =
      match config.ConstValue variable with
      | Some constant -> absVal.ofBitVector constant
      | None -> evaluatedValue

    state
    |> this.defReg variable value typeId
    |> this.updateStackDelta variable expr

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

  member private _.phiSource destVar srcId state =
    let srcVar = { destVar with Identifier = srcId }

    let srcTypeId, state = stateDom.getOrFreshTypeId srcVar state

    match stateDom.tryFindRegister srcVar state with
    | Some value -> value, srcTypeId, state
    | None ->
      match config.ConstValue srcVar with
      | Some constant ->
        let value = absVal.ofBitVector constant
        value, srcTypeId, stateDom.setRegister srcVar value srcTypeId state
      | None -> absVal.bot, srcTypeId, state

  member private this.evalPhi variable srcIds state =
    let getSrcValTyp (values, typeIds, state) sourceId =
      let value, typeId, state = this.phiSource variable sourceId state
      value :: values, typeId :: typeIds, state

    let values, sourceTypeIds, state =
      Array.fold getSrcValTyp ([], [], state) srcIds

    let valueJoined = List.fold absVal.join absVal.bot values
    let destTypeId, state = stateDom.getOrFreshTypeId variable state

    state
    |> stateDom.addSame (destTypeId :: sourceTypeIds)
    |> stateDom.setRegister variable valueJoined destTypeId
    |> this.applyPointerHint variable destTypeId

  member this.Eval
    (programPoint: ProgramPoint)
    (stmt: B2R2.BinIR.SSA.Stmt)
    state
    : TransferResult list =
    if config.Debug then
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

        match config.ApplyCallSummary programPoint [] [] state with
        | Some state -> [ { Target = Next; State = state } ]
        | None ->
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

        // How to distinguish real call target VS ambiguity
        // match config.ApplyCallSummary programPoint [] [] state with
        // | Some state -> [ { Target = Next; State = state } ]
        // | None ->
        [ { Target = InterTarget trueTarget
            State = state }
          { Target = InterTarget falseTarget
            State = state } ]

      // Need to Apply Callee Function
      | ExternalCall (calleeExpr, inputs, outputs) ->
        let _, calleeTypeId, state = exprEval.Eval state calleeExpr

        let state =
          match calleeTypeId with
          | Some typeId -> stateDom.addAddress typeId state
          | None -> state

        let state =
          match config.ApplyCallSummary programPoint inputs outputs state with
          | Some appliedState -> appliedState
          | None -> state

        [ { Target = Next; State = state } ]

      | SideEffect _ -> [ { Target = Next; State = state } ]

    if config.Debug then
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
  let createWithConfig platform config = StmtEvalModule (platform, config)

  let create platform = StmtEvalModule platform

  let createFromString platform =
    PointerAnalyzer.Platform.Platform.ofString platform |> create
