module PointerAnalyzer.Analysis.StmtEval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.Analysis.ExprEval
open PointerAnalyzer.Frontend.FunctionDFA
open PointerAnalyzer.PreAnalysis.PreAnalysisTypes

/// <summary>
/// Type of next instructions.
/// </summary>
/// <remarks>
/// <c>Next</c> indicates normal next instruction.
/// <c>LabelTarget</c> indicates jump target with Label. Its optional
/// CFGEdgeKind distinguishes conditional successors that share the same
/// address.
/// <c>InterTarget</c> indicates jump target with address, not function call.
/// Its optional CFGEdgeKind distinguishes true and false conditional
/// successors.
/// <c>CallTarget</c> indicates B2R2's function-abstraction node for a call.
/// This represents the callee.
/// <c>AbstractionReturn</c> indicates the terminal jump (return) in B2R2's
/// function-abstraction node.
/// <c>Terminated</c> indicates that the current execution path cannot continue.
/// </remarks>
type TransferTarget =
  | Next
  | LabelTarget of Label * CFGEdgeKind option
  | InterTarget of AbsVal * CFGEdgeKind option
  | CallTarget of Addr
  | AbstractionReturn
  | Terminated

/// <summary>
/// Indicates the context of block containing current statement.
/// </summary>
/// <remarks>
/// <c>NormalBlock</c> indicates corresponding block is the block of target
/// function.
/// <c>AbstractBlock</c> indicates corresponding block is FunctionAbstraction
/// representing callee function. This stores the ReturningStatus of
/// corresponding FunctionAbstraction.
/// </remarks>
type StmtContext =
  | NormalBlock
  | AbstractBlock of NonReturningStatus

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
/// as pointer or not. This tells the evidence that given variable is used as
/// pointer if it is used as pointer.
/// </summary>
type PointerUse = Variable -> PointerUseEvidence option

/// <summary>
/// Type definition of function to extract saturated constant value from B2R2
/// DFA.
/// </summary>
type ConstValue = Variable -> BitVector option

/// <summary>
/// Type definition of function for applying function summary to caller.
/// This function also returns the resolved callee address so the analyzer can
/// enter B2R2's function-abstraction node.
/// </summary>
type ApplyCallSummary =
  ProgramPoint
    -> Addr option
    -> Variable list
    -> Variable list
    -> AnalysisState
    -> (AnalysisState * Addr) option

/// <summary>
/// Some information used by evaluation, passed from
/// <see cref="PointerAnalyzer.Interproc.ModularAnalyzer.ModularAnalyzer.analyzeWithTimer" />.
/// </summary>
type StmtEvalConfig =
  { PointerUse: PointerUse
    ConstValue: ConstValue
    ClassifyConstant: BitVector -> ConstantType
    IsLive: Variable -> bool
    StackPointer: RegisterID option
    InitialStackPointer: Addr option
    ApplyCallSummary: ApplyCallSummary
    FunctionAddress: Addr
    FunctionName: string
    TrackTypeProvenance: bool
    Debug: bool }

module StmtEvalConfig =
  let empty =
    { PointerUse = fun _ -> None
      ConstValue = fun _ -> None
      ClassifyConstant = fun _ -> UnknownConstant
      IsLive = fun _ -> true
      StackPointer = None
      InitialStackPointer = None
      ApplyCallSummary = fun _ _ _ _ _ -> None
      FunctionAddress = 0UL
      FunctionName = ""
      TrackTypeProvenance = false
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
    functionPreResult
    classifyConst
    sp
    applyCallee
    trackTypeProvenance
    isDebug
    =
    let funDFAResult = functionPreResult.FunctionDFA.DFAResult
    let preAnalysis = functionPreResult.PreAnalysis

    let initialStackPointer =
      tryInitialStackPointer handle sp funDFAResult.ConstValue

    { PointerUse = funDFAResult.PointerUse
      ConstValue = funDFAResult.ConstValue
      ClassifyConstant = classifyConst
      // IsLive = fun variable -> PreAnalysisResult.isLive variable preAnalysis
      IsLive = fun variable -> true
      StackPointer = Some sp
      InitialStackPointer = initialStackPointer
      ApplyCallSummary = applyCallee
      FunctionAddress = functionPreResult.FunctionDFA.Address
      FunctionName = functionPreResult.FunctionDFA.Name
      TrackTypeProvenance = trackTypeProvenance
      Debug = isDebug }

/// Main logic of evaluating Statement
type StmtEvalModule (platform: Platform, config: StmtEvalConfig) =

  let absVal = AbsValDomain.create platform
  let stateDom = AnalysisStateDomain.createDefault platform

  let exprEval =
    ExprEvalDomain.createWithConfig
      platform
      { ClassifyConstant = config.ClassifyConstant
        IsLive = config.IsLive }

  new (platform: Platform) = StmtEvalModule (platform, StmtEvalConfig.empty)

  /// If given variable is used as pointer, add Address type constraint of
  /// given variable. The debug annotation is updated by setting its statement
  /// as the statement that target register is used as address.
  member private _.applyPointerHint variable typeId state =
    match config.PointerUse variable with
    | Some evidence ->
      let origin =
        { FunctionName = config.FunctionName
          Location =
            sprintf
              "0x%08x+%d"
              evidence.ProgramPoint.Address
              evidence.ProgramPoint.Position
          Statement = (PrettyPrinter.ToString [| evidence.Statement |]).Trim ()
          Annotation = "Address Sink" }

      let types =
        state.Types
        |> stateDom.TypeState.beginOrigin origin
        |> stateDom.TypeState.addAddressWithAnnotation origin.Annotation typeId
        |> stateDom.TypeState.endOrigin (* Invalidate analyzed stmt *)

      { state with Types = types }
    | None -> state

  /// Check given SSA variable's type is Trivial
  member private _.isTrivialVariable variable =
    platform.IsTrivialAddress variable || platform.IsTrivialValue variable

  member private this.isLive variable =
    config.IsLive variable || this.isTrivialVariable variable

  member private this.allLive variables = variables |> Seq.forall this.isLive

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
      (*
        Add type constraint only it consists with the type id of  live SSA
        variables
      *)
      match exprTypeId with
      | Some exprTypeId when this.isLive variable ->
        stateDom.addSame [ typeId; exprTypeId ] state
      | None -> state
      | Some _ -> state

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

  /// Bind a callee output to the fresh SSA register defined by B2R2's
  /// FunctionAbstraction.
  member private this.evalAbstractOutputDefinition variable state =
    (* Assign new type id for corresponding SSA variable *)
    let typeId, state = stateDom.getOrFreshTypeId variable state
    (* Get the corresponding register-output type id of the callee. *)
    let pendingOutput, state =
      stateDom.consumePendingRegisterOutput variable state

    (* Connect the callee output and fresh caller-side SSA register. *)
    let state =
      match pendingOutput with
      | Some calleeTypeId ->
        stateDom.addSameWithAnnotation
          "Register Output Binding At Function Abstraction"
          [ typeId; calleeTypeId ]
          state
      | None -> state

    state
    |> stateDom.setRegister variable absVal.bot typeId
    |> this.updateCurrentStackSlot variable typeId
    |> this.applyPointerHint variable typeId

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
    let sourceVariables =
      srcIds
      |> Array.map (fun sourceId -> { variable with Identifier = sourceId })

    let getSrcValTyp (values, typeIds, state) sourceId =
      let value, typeId, state = this.phiSource variable sourceId state
      value :: values, typeId :: typeIds, state

    let values, sourceTypeIds, state =
      Array.fold getSrcValTyp ([], [], state) srcIds

    let valueJoined = List.fold absVal.join absVal.bot values
    let destTypeId, state = stateDom.getOrFreshTypeId variable state

    let state =
      if this.allLive (variable :: Array.toList sourceVariables) then
        stateDom.addSame (destTypeId :: sourceTypeIds) state
      else
        state

    state
    |> stateDom.setRegister variable valueJoined destTypeId
    |> this.updateCurrentStackSlot variable destTypeId
    |> this.applyPointerHint variable destTypeId

  /// Statement evaluation
  member this.Eval
    context
    (programPoint: ProgramPoint)
    (stmt: B2R2.BinIR.SSA.Stmt)
    state
    : TransferResult list =
    (*
      Set current statement to track type constraint update.
      The update only processed when the debug mode is enabled.
    *)
    let state =
      if config.TrackTypeProvenance then
        let origin =
          { FunctionName = config.FunctionName
            Location =
              sprintf "0x%08x+%d" programPoint.Address programPoint.Position
            Statement = (PrettyPrinter.ToString [| stmt |]).Trim ()
            Annotation = "" }

        { state with
            Types = stateDom.TypeState.beginOrigin origin state.Types }
      else
        state

    if config.Debug then
      printfn "ProgramPoint: %A" programPoint
      printfn "Stmt: %s" (PrettyPrinter.ToString [| stmt |])

    let results =
      match stmt with
      | LMark _ -> [ { Target = Next; State = state } ]

      | Def ({ Kind = MemVar } as resultMem, expr) ->
        [ { Target = Next
            State = this.evalMemoryDefinition resultMem expr state } ]

      | Def (variable, Undefined (_, reason)) when
        reason = "ret" || reason = "caller-saved"
        ->
        [ { Target = Next
            State = this.evalAbstractOutputDefinition variable state } ]

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
            // State = this.evalPhi variable sourceIds state } ]
            State = state } ]

      | Jmp (IntraJmp label) ->
        [ { Target = LabelTarget (label, None)
            State = state } ]

      | Jmp (IntraCJmp (conditionExpr, trueLabel, falseLabel)) ->
        let _, conditionTypeId, state = exprEval.Eval state conditionExpr

        // let state =
        //   match conditionTypeId with
        //   | Some typeId -> stateDom.addValue typeId state
        //   | None -> state

        [ { Target = LabelTarget (trueLabel, Some IntraCJmpTrueEdge)
            State = state }
          { Target = LabelTarget (falseLabel, Some IntraCJmpFalseEdge)
            State = state } ]

      | Jmp (InterJmp targetExpr) ->
        let target, targetTypeId, state = exprEval.Eval state targetExpr
        let targetAddr = absVal.tryGetUInt64 target

        (* Mark indirect jump target as Address with debug history *)
        let state =
          match targetTypeId with
          | Some typeId ->
            stateDom.addAddressWithAnnotation
              "Indirect Jump Target"
              typeId
              state
          | None -> state

        match context with
        | AbstractBlock _ ->
          (* InterJmp from FunctionAbstraction: Return to caller *)
          [ { Target = AbstractionReturn
              State = stateDom.clearPendingRegisterOutputs state } ]
        | NormalBlock ->
          (* InterJmp from Normal Block: Normal jmp/call *)
          match config.ApplyCallSummary programPoint targetAddr [] [] state with
          | Some (appliedState, calleeAddress) ->
            [ { Target = CallTarget calleeAddress
                State = appliedState } ]
          | None ->
            [ { Target = InterTarget (target, None)
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
            (* Mark true jump target as Address with debug history *)
            match trueTypeId with
            | Some typeId ->
              stateDom.addAddressWithAnnotation
                "Conditional Indirect-Jump True Target"
                typeId
                state
            | None -> state)
          |> (fun state ->
            match falseTypeId with
            | Some typeId ->
              (* Mark false jump target as Address with debug history *)
              stateDom.addAddressWithAnnotation
                "Conditional Indirect-Jump False Target"
                typeId
                state
            | None -> state)

        [ { Target = InterTarget (trueTarget, Some InterCJmpTrueEdge)
            State = state }
          { Target = InterTarget (falseTarget, Some InterCJmpFalseEdge)
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
          | Some typeId ->
            (* Mark external call target as Address with debug history *)
            stateDom.addAddressWithAnnotation
              "External Call Target"
              typeId
              state
          | None -> state

        let state =
          match
            config.ApplyCallSummary programPoint targetAddr inputs outputs state
          with
          | Some (appliedState, _) -> appliedState
          // state
          | None -> state

        [ { Target = Next; State = state } ]

      | SideEffect Terminate -> [ { Target = Terminated; State = state } ]

      (* Some exception may be handled by SW *)
      | SideEffect (Exception _)
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

    (* Unset current statement used for tracking to prevent population *)
    results
    |> List.map (fun result ->
      { result with
          State =
            { result.State with
                Types = stateDom.TypeState.endOrigin result.State.Types } })

module StmtEvalDomain =
  let createWithConfig platform config = StmtEvalModule (platform, config)

  let create platform = StmtEvalModule platform

  let createFromString platform =
    PointerAnalyzer.Platform.Platform.ofString platform |> create
