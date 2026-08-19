module PointerAnalyzer.Summary.SyscallSummaryApplicator

open B2R2
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Summary

type private CallArgument = { TypeId: TypeId; Value: AbsVal }

/// Applies syscall summaries without using the ordinary function-call ABI.
type SyscallSummaryApplicatorModule (platform: Platform) =
  let stateDom = AnalysisStateDomain.createDefault platform

  let addExpectedType annotation expectedType typeId state =
    match expectedType with
    | SyscallAddress ->
      stateDom.addAddressWithAnnotation annotation typeId state
    | SyscallValue //->
    // stateDom.addSameWithAnnotation
    //   annotation
    //   [ TypeIds.value; typeId ]
    //   state
    | SyscallUnknown -> state

  let trySyscallNumber abi state =
    stateDom.tryFindCurrentRegisterValue abi.NumberRegister state
    |> Option.bind stateDom.AbsVal.tryGetUInt64

  let connectParameters abi signature state =
    let state =
      match Map.tryFind abi.NumberRegister state.CurrentRegisters with
      | Some typeId ->
        addExpectedType "Syscall Number Register" SyscallValue typeId state
      | None -> state

    signature.Arguments
    |> Map.fold
      (fun state registerId expectedType ->
        match Map.tryFind registerId state.CurrentRegisters with
        | Some typeId ->
          addExpectedType
            (sprintf "Syscall Argument Binding: %s" signature.Name)
            expectedType
            typeId
            state
        | None -> state)
      state

  let setPendingOutputs abi signatureOpt abstractionOutputs state =
    let declaredReturns =
      signatureOpt
      |> Option.map (fun signature -> signature.Returns)
      |> Option.defaultValue Map.empty

    let modeledRegisters =
      match signatureOpt with
      | Some _ ->
        let returnRegisters =
          declaredReturns |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let clobberedOutputs =
          Set.intersect abi.ClobberedRegisters abstractionOutputs

        Set.union returnRegisters clobberedOutputs
      | None -> abstractionOutputs

    modeledRegisters
    |> Set.fold
      (fun state registerId ->
        let typeId, state = stateDom.freshTypeId state

        let state =
          match Map.tryFind registerId declaredReturns, signatureOpt with
          | Some expectedType, Some signature ->
            addExpectedType
              (sprintf "Syscall Return Binding: %s" signature.Name)
              expectedType
              typeId
              state
          | _ -> state

        stateDom.setPendingRegisterOutput registerId typeId state)
      state

  /// Construct CallSiteStackContext
  let dispatcherContext
    (dispatcher: SyscallDispatcherSummary)
    (state: AnalysisState)
    : CallSiteStackContext =
    let parameterCount =
      dispatcher.ForwardedParameters |> Set.maxElement |> (+) 1

    { StackPointer = state.StackPointer
      ParameterCount = parameterCount }

  /// Map RegVar and StackVar used as parameters
  let dispatcherArguments
    (context: CallSiteStackContext)
    (state: AnalysisState)
    =
    (* Get value of RegVar and its parameter index based on current SP *)
    let registerArguments =
      state.CurrentRegisters
      |> Map.toSeq
      |> Seq.choose (fun (registerId, typeId) ->
        platform.TryCallRegisterArgumentIndex context registerId
        |> Option.map (fun index ->
          index,
          { TypeId = typeId
            Value =
              Map.tryFind registerId state.CurrentRegisterValues
              |> Option.defaultValue stateDom.AbsVal.bot }))

    (* Get value of StackVar and its parameter index based on current SP *)
    let stackArguments =
      match context.StackPointer.TryDelta with
      | None -> Seq.empty
      | Some _ ->
        state.CurrentStackSlots
        |> Map.toSeq
        |> Seq.choose (fun (offset, typeId) ->
          platform.TryCallStackSlotArgumentIndex context offset
          |> Option.map (fun index ->
            index,
            { TypeId = typeId
              Value =
                Map.tryFind offset state.CurrentStackSlotValues
                |> Option.defaultValue stateDom.AbsVal.bot }))

    Seq.append registerArguments stackArguments |> Map.ofSeq

  /// Connect type of syscall number and syscall arugments
  let connectDispatcherParameters dispatcher signature arguments state =
    (* Bind syscall number as Value type *)
    let state =
      match Map.tryFind dispatcher.NumberParameter arguments with
      | Some argument ->
        addExpectedType
          "Syscall Dispatcher Number Binding"
          SyscallValue
          argument.TypeId
          state
      | None -> state

    (* Bind syscall arguments *)
    signature.Arguments
    |> Map.fold
      (fun state registerId expectedType ->
        match
          Map.tryFind registerId dispatcher.ArgumentParameters
          |> Option.bind (fun parameterIndex ->
            Map.tryFind parameterIndex arguments)
        with
        | Some argument ->
          addExpectedType
            (sprintf "Syscall Dispatcher Argument Binding: %s" signature.Name)
            expectedType
            argument.TypeId
            state
        | None -> state)
      state

  /// Apply one syscall summary to the state at its syscall instruction.
  member _.apply summary state =
    let state = stateDom.clearPendingRegisterOutputs state

    match platform.SyscallABI with
    | Some abi ->
      let signatureOpt =
        trySyscallNumber abi state |> Option.bind abi.TryFindSignature

      let state =
        match signatureOpt with
        | Some signature -> connectParameters abi signature state
        | None -> state

      let isNoReturn =
        summary.IsExit
        || (signatureOpt
            |> Option.exists (fun signature -> signature.IsNoReturn))

      if isNoReturn then
        state, NoRet
      else
        setPendingOutputs abi signatureOpt summary.AbstractionOutputs state,
        NotNoRet
    | None ->
      if summary.IsExit then
        state, NoRet
      else
        let state =
          summary.AbstractionOutputs
          |> Set.fold
            (fun state registerId ->
              let typeId, state = stateDom.freshTypeId state
              stateDom.setPendingRegisterOutput registerId typeId state)
            state

        state, NotNoRet

  /// Specialize a call to a syscall wrapper with caller-side argument values
  /// and types. Wrapper parameter TypeId is not shared.
  member _.applyDispatcher dispatcher abstractionOutputs state =
    let state = stateDom.clearPendingRegisterOutputs state

    match platform.SyscallABI with
    | None ->
      (* No ABI information, just set fresh ID of Return registers *)
      let state =
        abstractionOutputs
        |> Set.fold
          (fun state registerId ->
            let typeId, state = stateDom.freshTypeId state
            stateDom.setPendingRegisterOutput registerId typeId state)
          state

      stateDom.forgetCurrentStackPointer state, UnknownNoRet
    | Some abi ->
      let context = dispatcherContext dispatcher state
      let arguments = dispatcherArguments context state

      (* Add ReturnAddress Type *)
      let state =
        match platform.TryCallReturnAddressStackSlot context with
        | Some offset when platform.IsStack0Return ->
          match Map.tryFind offset state.CurrentStackSlots with
          | Some typeId ->
            stateDom.addAddressWithAnnotation
              "Syscall Dispatcher Return Address Stack Slot"
              typeId
              state
          | None -> state
        | _ -> state

      (* Extract syscall function signature *)
      let signatureOpt =
        Map.tryFind dispatcher.NumberParameter arguments
        |> Option.bind (fun argument ->
          stateDom.AbsVal.tryGetUInt64 argument.Value)
        |> Option.bind abi.TryFindSignature

      (* Connect syscall number and arguments type *)
      let state =
        match signatureOpt with
        | Some signature ->
          connectDispatcherParameters dispatcher signature arguments state
        | None -> state

      (* Set return registers of syscall *)
      let returningStatus =
        if
          signatureOpt |> Option.exists (fun signature -> signature.IsNoReturn)
        then
          NoRet
        else
          NotNoRet

      if returningStatus = NoRet then
        stateDom.forgetCurrentStackPointer state, NoRet
      else
        let state =
          setPendingOutputs abi signatureOpt abstractionOutputs state
          |> stateDom.forgetCurrentStackPointer

        state, NotNoRet

module SyscallSummaryApplicator =
  let create platform = SyscallSummaryApplicatorModule platform
