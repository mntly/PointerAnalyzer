module PointerAnalyzer.Summary.SummaryApplicator

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.ControlFlowGraph

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
    { StackPointer = state.StackPointer
      ParameterCount = Map.count summary.Parameters }

  /// Connect the same type relationship between arguments and parameters of
  /// specific callee
  let connectVariables annotation getCalleeParamTid variables state =
    let mapSameArgIdx state variable =
      match
        getCalleeParamTid variable, stateDom.tryFindTypeId variable state
      with
      | Some (calleeTypeId: TypeId), Some callerTypeId ->
        stateDom.addSameWithAnnotation
          annotation
          [ calleeTypeId; callerTypeId ]
          state
      | _, _ -> state

    List.fold mapSameArgIdx state variables

  /// Connect callee parameter type ids to caller-side live argument type ids.
  let connectTypeIds getCalleeParamTid indexedTypeIds state =
    let mapSameArgIdx state (argumentIndex, callerTypeId) =
      match getCalleeParamTid argumentIndex with
      | Some (calleeTypeId: TypeId) ->
        stateDom.addSameWithAnnotation
          "Argument Binding At Call"
          [ calleeTypeId; callerTypeId ]
          state
      | None -> state

    List.fold mapSameArgIdx state indexedTypeIds

  /// Based on calling convention, extract the arguments of specific callee
  /// from the current caller state.
  let inferArguments context state =
    let registerArgs =
      state.CurrentRegisters
      |> Map.toSeq
      |> Seq.choose (fun (regId, typeId) ->
        platform.TryCallRegisterArgumentIndex context regId
        |> Option.map (fun index -> index, typeId))

    let stackArgs =
      state.CurrentStackSlots
      |> Map.toSeq
      |> Seq.choose (fun (offset, typeId) ->
        platform.TryCallStackSlotArgumentIndex context offset
        |> Option.map (fun index -> index, typeId))

    Seq.append registerArgs stackArgs |> Seq.toList

  /// Store callee register outputs until B2R2's function abstraction defines
  /// the corresponding fresh caller SSA variables.
  let setPendingRegisterOutputs summary state =
    let setPendingOutput state regId calleeTypeId =
      stateDom.setPendingRegisterOutput regId calleeTypeId state

    summary.RegisterOutputs |> Map.fold setPendingOutput state

  /// Applying the analysis result of callee to caller's analysis state
  member _.apply summary returningStatus inputs outputs state =
    (* New function call overwrite previous function call summary *)
    let state = stateDom.clearPendingRegisterOutputs state
    let context = callSiteContext summary state

    (* According to calling convention, get argument index of given variable *)
    let inVarType variable =
      platform.TryCallArgumentIndex context variable
      |> Option.bind (fun index -> Map.tryFind index summary.Parameters)

    (* Get callee output register type using register id of given variable. *)
    let outVarType (variable: Variable) =
      match variable.Kind with
      | VariableKind.RegVar (_, regId, _) ->
        Map.tryFind regId summary.RegisterOutputs
      | _ -> None

    (* Connect type between arguments and parameters *)
    let state =
      if List.isEmpty inputs then
        let inferredInputs = inferArguments context state

        connectTypeIds
          (fun index -> Map.tryFind index summary.Parameters)
          inferredInputs
          state
      else
        connectVariables
          "Argument Binding At Call"
          inVarType
          inputs
          state

    (*
      Connect type between stack var 0 of callee and callee if stack var 0
      used for return address slot
    *)
    let state =
      match platform.TryCallReturnAddressStackSlot context with
      | Some offset when platform.IsStack0Return ->
        match Map.tryFind offset state.CurrentStackSlots with
        | Some typeId ->
          stateDom.addAddressWithAnnotation
            "Return Address Stack Slot"
            typeId
            state
        | None -> state
      | _ -> state

    (*
      Explicitly connect output variables due to callee or store them as
      pending register outputs.
    *)
    let state =
      match returningStatus with
      | NoRet ->
        (* A non-returning call has no caller-side output state. *)
        state
      | NotNoRet
      | ConditionalNoRet _
      | UnknownNoRet ->
        (* Otherwise, wait for fresh FunctionAbstraction definitions. *)
        if List.isEmpty outputs then
          setPendingRegisterOutputs summary state
        else
          connectVariables
            "Return Value Binding At Call"
            outVarType
            outputs
            state

    (* Due to stack prologue, set current SP to None *)
    (* After before call, SP will reset *)
    stateDom.forgetCurrentStackPointer state

module SummaryApplicator =
  let create platform = SummaryApplicatorModule platform
