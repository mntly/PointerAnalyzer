module PointerAnalyzer.Summary.SyscallSummaryApplicator

open B2R2
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Summary

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

  let setPendingOutputs abi signatureOpt summary state =
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
          Set.intersect abi.ClobberedRegisters summary.AbstractionOutputs

        Set.union returnRegisters clobberedOutputs
      | None -> summary.AbstractionOutputs

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
        setPendingOutputs abi signatureOpt summary state, NotNoRet
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

module SyscallSummaryApplicator =
  let create platform = SyscallSummaryApplicatorModule platform
