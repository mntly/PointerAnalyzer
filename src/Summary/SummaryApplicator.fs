module PointerAnalyzer.Summary.SummaryApplicator

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA

open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.Summary

/// <summary>
/// Define how to apply callee summary to caller.
/// </summary>
type SummaryApplicatorModule (platform: Platform) =
  let stateDom = AnalysisStateDomain.createDefault platform

  /// Represent the information got by caller right before moving to callee.
  let callSiteContext summary state =
    { ReturnAddressOffset = state.StackDelta
      ParameterCount = Map.count summary.Parameters }

  /// Connect the same type relationship between arguments and parameters of
  /// specific callee
  let connectVariables getCalleeParamTid variables state =
    let mapSameArgIdx state variable =
      match
        getCalleeParamTid variable, stateDom.tryFindTypeId variable state
      with
      | Some calleeTypeId, Some callerTypeId ->
        stateDom.addSame [ calleeTypeId; callerTypeId ] state
      | _, _ -> state

    List.fold mapSameArgIdx state variables

  /// Based on calling convention, extract the arugments of specific callee
  let inferArguments context state =
    let filterArg (reg, _regVal) =
      match platform.TryCallArgumentIndex context reg with
      | Some idx -> Some (idx, reg)
      | None -> None

    (* Latest defined from call instruction *)
    let getLastReg (_argIdx, sameArgIdx) =
      let sameArgs = Seq.map snd sameArgIdx
      let lastRegArg = Seq.maxBy (fun reg -> reg.Identifier) sameArgs
      lastRegArg

    let argSeq = state.RegMap |> Map.toSeq |> Seq.choose filterArg
    let groupedByArgIdx = argSeq |> Seq.groupBy fst

    groupedByArgIdx |> Seq.map getLastReg |> Seq.toList

  /// Based on calling convetion, map the return register and corrsponding
  /// register of caller. Some intrinsic functions are handled with specific
  /// register as return register.
  let setPendingReturns summary state =
    let setPendingReturnsInner state retIdx calleeRetTypId =
      match List.tryItem retIdx platform.ReturnRegisters with
      | Some retRegId -> stateDom.setPendingReturn retRegId calleeRetTypId state
      | None -> state

    summary.Returns |> Map.fold setPendingReturnsInner state

  /// Check given callee is get_pc_thunk function.
  /// If it is get_pc_thunk, set corresponding return register and type
  /// constraint
  let getPcThunkHandle handle calleeAddr state =
    match platform.CheckIntrinsic PCThunk handle calleeAddr with
    | Some outputRegId ->
      Some (stateDom.setPendingReturn outputRegId TypeIds.address state)
    | None -> None

  /// Applying the analysis result of callee to caller's analysis state
  member _.apply handle calleeAddr summary inputs outputs state =
    let context = callSiteContext summary state

    (* According to calling convention, get argument index of given variable *)
    let inVarType variable =
      platform.TryCallArgumentIndex context variable
      |> Option.bind (fun index -> Map.tryFind index summary.Parameters)

    (* According to calling convention, get return index of given variable *)
    let outVarType variable =
      platform.TryReturnIndex variable
      |> Option.bind (fun index -> Map.tryFind index summary.Returns)

    (* Connect type between arguments and parameters *)
    let state =
      if List.isEmpty inputs then
        let inferredInputs = inferArguments context state
        connectVariables inVarType inferredInputs state
      else
        connectVariables inVarType inputs state

    (* Connect type or set pending returns between return registers *)
    (* If target address is get_pc_thunk, handle heuristically *)
    (* match getPcThunkHandle handle calleeAddr state with
    | Some state -> state
    | None -> *)
    if List.isEmpty outputs then
      setPendingReturns summary state
    else
      connectVariables outVarType outputs state

module SummaryApplicator =
  let create platform = SummaryApplicatorModule platform
