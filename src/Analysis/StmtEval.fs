module PointerAnalyzer.Analysis.StmtEval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.Analysis.ExprEval
open PointerAnalyzer.Frontend.FunctionDFA

/// <summary>
/// Type of next instructions.
/// </summary>
/// <remarks>
/// <c>Next</c> indicates normal next instruction.
/// <c>LabelTarget</c> indicates jump target with Label.
/// <c>InterTarget</c> indicates jump target with address, not function call.
/// <c>ReturnTarget</c> indicates the instruction after a function call.
/// </remarks>
type TransferTarget =
  | Next
  | LabelTarget of Label
  | InterTarget of AbsVal
  | ReturnTarget of Addr

/// <summary>
/// The transfer result of each statement.
/// </summary>
/// <remarks>
/// <c>Target</c> is next instruction to evaluate.
/// <c>State</c> is result analysis result after evaluating current statement.
/// </remarks>
type TransferResult =
  { Target: TransferTarget
    State: AnalysisState }

/// <summary>
/// Type definition of function to detect whether given variable will be used
/// as pointer or not.
/// </summary>
type PointerUse = Variable -> bool

/// <summary>
/// Type definition of function to extract saturated constant value from B2R2
/// DFA.
/// </summary>
type ConstValue = Variable -> BitVector option

/// <summary>
/// Type definition of function for applying function summary to caller.
/// This function also returns return address to handle inlined function by
/// B2R2.
/// </summary>
type ApplyCallSummary =
  ProgramPoint
    -> Addr option
    -> Variable list
    -> Variable list
    -> AnalysisState
    -> (AnalysisState * Addr option) option

/// <summary>
/// Some information used by evaluation, passed from
/// <see cref="PointerAnalyzer.Interproc.ModularAnalyzer.ModularAnalyzer.analyzeWithTimer" />.
/// </summary>
type StmtEvalConfig =
  { PointerUse: PointerUse
    ConstValue: ConstValue
    ClassifyConstant: BitVector -> ConstantType
    StackPointer: RegisterID option
    InitialStackPointer: Addr option
    ApplyCallSummary: ApplyCallSummary
    Debug: bool }

module StmtEvalConfig =
  let empty =
    { PointerUse = fun _ -> false
      ConstValue = fun _ -> None
      ClassifyConstant = fun _ -> UnknownConstant
      StackPointer = None
      InitialStackPointer = None
      ApplyCallSummary = fun _ _ _ _ _ -> None
      Debug = false }

  /// Get UInt64 value of given BitVector
  let private tryUInt64 (value: BitVector) =
    try
      Some (value.ToUInt64 ())
    with _ ->
      None

  /// Get initial stack pointer value from B2R2 constant propagation.
  let private tryInitialStackPointer
    (handle: BinHandle)
    stackPointer
    constValue
    =
    let regType = handle.RegisterFactory.GetRegType stackPointer
    let regName = handle.RegisterFactory.GetRegisterName stackPointer

    let stackPointerZero =
      { Kind = RegVar (regType, stackPointer, regName)
        Identifier = 0 }

    constValue stackPointerZero |> Option.bind tryUInt64

  /// Construct config used for analyzing.
  let construct
    handle
    (funDFAResult: FunctionDFA)
    classifyConst
    sp
    applyCallee
    isDebug
    =
    let initialStackPointer =
      tryInitialStackPointer handle sp funDFAResult.ConstValue

    { PointerUse = funDFAResult.PointerUse
      ConstValue = funDFAResult.ConstValue
      ClassifyConstant = classifyConst
      StackPointer = Some sp
      InitialStackPointer = initialStackPointer
      ApplyCallSummary = applyCallee
      Debug = isDebug }

/// Main logic of evaluating Statement
type StmtEvalModule (platform: Platform, config: StmtEvalConfig) =

  let absVal = AbsValDomain.create platform
  let stateDom = AnalysisStateDomain.createDefault platform

  let exprEval =
    ExprEvalDomain.createWithConfig
      platform
      { ClassifyConstant = config.ClassifyConstant }

  new (platform: Platform) = StmtEvalModule (platform, StmtEvalConfig.empty)

  /// If given variable is used as pointer, add Address type constraint of
  /// given variable
  member private _.applyPointerHint variable typeId state =
    if config.PointerUse variable then
      stateDom.addAddress typeId state
    else
      state

  /// Check whether given variable is the stack pointer or not
  member private _.isStackPointer (variable: Variable) =
    match config.StackPointer, variable.Kind with
    | Some stackPointer, RegVar (_, registerId, _) -> registerId = stackPointer
    | _ -> false

  /// If the definition target is stack pointer, update it from B2R2 constProp.
  member private this.updateStackPointerFromConst (variable: Variable) state =
    if this.isStackPointer variable then
      (* Only set constant value if it can be recovered from B2R2 constProp *)
      (* If not, set None by default *)
      match config.ConstValue variable with
      | Some constant ->
        try
          stateDom.setCurrentStackPointer (constant.ToUInt64 ()) state
        with _ ->
          stateDom.forgetCurrentStackPointer state
      | None -> stateDom.forgetCurrentStackPointer state
    else
      state

  /// If the definition target is a stack variable, remember the latest type Id
  /// for that stack slot offset.
  member private _.updateCurrentStackSlot (variable: Variable) typeId state =
    match variable.Kind with
    | VariableKind.StackVar (_, offset) ->
      stateDom.setCurrentStackSlot offset typeId state
    | _ -> state

  /// Assign evaluated value to target variable and connect type constraint.
  /// The type Id of expression and target variable are connected with Same
  /// type constraint.
  member private this.defReg (variable: Variable) value exprTypeId state =
    let typeId, state = stateDom.getOrFreshTypeId variable state

    let state =
      match exprTypeId with
      | Some exprTypeId -> stateDom.addSame [ typeId; exprTypeId ] state
      | None -> state

    let _, state = stateDom.consumePendingReturn variable state

    let state = stateDom.setRegister variable value typeId state

    state
    |> this.updateCurrentStackSlot variable typeId
    |> this.applyPointerHint variable typeId

  /// Handle variable definition by evaluating the expression and assign it to
  /// target variable
  member private this.evalDefinition (variable: Variable) expr state =
    let evaluatedValue, typeId, state = exprEval.Eval state expr

    let value =
      match config.ConstValue variable with
      | Some constant -> absVal.ofBitVector constant
      | None -> evaluatedValue

    state
    |> this.defReg variable value typeId
    |> this.updateStackPointerFromConst variable

  /// Handle memory definition by evaluating the expression. Memory definition
  /// occurs only when store expression, this evaluate store expression and
  /// update the memory. For this reason, the expression evaluation process
  /// does not handle about store expression.
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

  /// Load appropriate variable of phi source
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

  /// Get type Ids of phi source and connect them with target variable as Same
  /// type constraint.
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
    |> this.updateCurrentStackSlot variable destTypeId
    |> this.applyPointerHint variable destTypeId

  /// Statement evaluation
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

      (*
        ToDo
          Need to check when and how should treat memory phi
      *)
      | Phi ({ Kind = MemVar }, _) -> [ { Target = Next; State = state } ]

      | Phi (variable, sourceIds) ->
        [ { Target = Next
            State = this.evalPhi variable sourceIds state } ]

      | Jmp (IntraJmp label) ->
        [ { Target = LabelTarget label
            State = state } ]

      | Jmp (IntraCJmp (conditionExpr, trueLabel, falseLabel)) ->
        let _, conditionTypeId, state = exprEval.Eval state conditionExpr

        // let state =
        //   match conditionTypeId with
        //   | Some typeId -> stateDom.addValue typeId state
        //   | None -> state

        [ { Target = LabelTarget trueLabel
            State = state }
          { Target = LabelTarget falseLabel
            State = state } ]

      | Jmp (InterJmp targetExpr) ->
        let target, targetTypeId, state = exprEval.Eval state targetExpr
        let targetAddr = absVal.tryGetUInt64 target

        let state =
          match targetTypeId with
          | Some typeId -> stateDom.addAddress typeId state
          | None -> state

        match config.ApplyCallSummary programPoint targetAddr [] [] state with
        | Some (state, returnAddr) ->
          let target =
            match returnAddr with
            | Some address -> ReturnTarget address
            | None -> Next

          [ { Target = target; State = state } ]
        | None ->
          [ { Target = InterTarget target
              State = state } ]

      | Jmp (InterCJmp (conditionExpr, trueExpr, falseExpr)) ->
        let _, conditionTypeId, state = exprEval.Eval state conditionExpr
        let trueTarget, trueTypeId, state = exprEval.Eval state trueExpr
        let falseTarget, falseTypeId, state = exprEval.Eval state falseExpr

        let state =
          state
          // |> (fun state ->
          //   match conditionTypeId with
          //   | Some typeId -> stateDom.addValue typeId state
          //   | None -> state)
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

      (*
        ToDo
          If the analyzer can figure out ExternalCall, it is possible to apply
          the information of it
      *)
      | ExternalCall (calleeExpr, inputs, outputs) ->
        let calleeValue, calleeTypeId, state = exprEval.Eval state calleeExpr
        let targetAddr = absVal.tryGetUInt64 calleeValue

        let state =
          match calleeTypeId with
          | Some typeId -> stateDom.addAddress typeId state
          | None -> state

        let state =
          match
            config.ApplyCallSummary programPoint targetAddr inputs outputs state
          with
          | Some (appliedState, _) -> appliedState
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
