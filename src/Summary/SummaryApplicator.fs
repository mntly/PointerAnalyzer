module PointerAnalyzer.Summary.SummaryApplicator

open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.Summary

/// <summary>
/// Define how to apply callee summary to caller.
/// </summary>
type SummaryApplicatorModule (platform: Platform) =

  ///
  let stateDom = AnalysisStateDomain.createDefault platform

  let callSiteContext summary state =
    { ReturnAddressOffset = state.StackDelta
      ParameterCount = Map.count summary.Parameters }

  let connectVariables getCalleeTid variables state =
    let mapSameArgIdx state variable =
      match getCalleeTid variable, stateDom.tryFindTypeId variable state with
      | Some calleeTypeId, Some callerTypeId ->
        stateDom.addSame [ calleeTypeId; callerTypeId ] state
      | _, _ -> state

    List.fold mapSameArgIdx state variables

  let inferArguments context state =
    let filterArg (reg, _regVal) =
      match platform.TryCallArgumentIndex context reg with
      | Some idx -> Some (idx, reg)
      | None -> None

    let getLastReg (_argIdx, sameArgIdx) =
      let sameArgs = Seq.map snd sameArgIdx
      let lastRegArg = Seq.maxBy (fun reg -> reg.Identifier) sameArgs
      lastRegArg

    let argSeq = state.RegMap |> Map.toSeq |> Seq.choose filterArg
    let groupedByArgIdx = argSeq |> Seq.groupBy fst

    groupedByArgIdx |> Seq.map getLastReg |> Seq.toList

  let setPendingReturns summary state =
    let setPendingReturnsInner state retIdx calleeRetTypId =
      match List.tryItem retIdx platform.ReturnRegisters with
      | Some retRegId -> stateDom.setPendingReturn retRegId calleeRetTypId state
      | None -> state

    summary.Returns |> Map.fold setPendingReturnsInner state

  member _.apply summary inputs outputs state =
    let state =
      summary.Constraints
      |> Set.fold
        (fun acc constraint_ -> stateDom.addConstraint constraint_ acc)
        state

    let context = callSiteContext summary state

    let inVarType variable =
      platform.TryCallArgumentIndex context variable
      |> Option.bind (fun index -> Map.tryFind index summary.Parameters)

    let outVarType variable =
      platform.TryReturnIndex variable
      |> Option.bind (fun index -> Map.tryFind index summary.Returns)

    let state =
      if List.isEmpty inputs then
        let inferredInputs = inferArguments context state
        connectVariables inVarType inferredInputs state
      else
        connectVariables inVarType inputs state

    if List.isEmpty outputs then
      setPendingReturns summary state
    else
      connectVariables outVarType outputs state

module SummaryApplicator =
  let create platform = SummaryApplicatorModule platform
