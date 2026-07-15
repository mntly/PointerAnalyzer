module PointerAnalyzer.Summary.FunctionSummaryBuilder

open B2R2
open B2R2.BinIR.SSA

open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Summary
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeIdMap

module FunctionSummaryBuilder =
  /// Select the type id of the SSA variable satisfying given condition in each
  /// group. This is used for extracting the type id of parameters.
  let private selectByIdentifier
    identifierCond
    (entries: (int * Variable * TypeId) seq)
    : Map<int, TypeId> =
    let extractRegId (_paramIdx: int, reg, _tid: TypeId) = reg.Identifier

    let chooseReg (paramIdx: int, regSeq) =
      let _, _, typeId = identifierCond extractRegId regSeq
      paramIdx, typeId

    let sameParamIdxSeq = Seq.groupBy (fun (index, _, _) -> index) entries

    sameParamIdxSeq |> Seq.map chooseReg |> Map.ofSeq

  /// Merge output type IDs from all leaf states according to ABI register
  /// class.
  let private mergeExitRegisterTypeIds platform groupedTypeIds =
    (* Check given register has trivial type *)
    (* Register with trivial type do not need to merge *)
    let isTrivial regId =
      Set.contains regId platform.TrivialValueRegisters
      || Set.contains regId platform.TrivialAddressRegisters

    (* Check given register is caller-saved register *)
    (*
      Caller-saved register should be selectively merged
      since it does not need to same for all candidates
    *)
    let isCallerSaved regId =
      Set.contains regId platform.CallerSavedRegisters

    (* Check given register is callee-saved register *)
    (*
      Callee-saved register should be merged
      since it must same for all candidates
    *)
    let isCalleeSaved regId =
      Set.contains regId platform.CalleeSavedRegisters
        || List.contains regId platform.ReturnRegisters

    let mergeRegister (constraints_, returns_) (regId, typeIds: Set<TypeId>) =
      if isTrivial regId || Set.isEmpty typeIds then
        constraints_, returns_
      elif isCalleeSaved regId then
        let representative = Set.minElement typeIds

        let constraints_ =
          if Set.count typeIds <= 1 then
            constraints_
          else
            Set.add (Same typeIds) constraints_

        constraints_, Map.add regId representative returns_
      elif isCallerSaved regId then
        if Set.count typeIds = 1 then
          constraints_, Map.add regId (Set.minElement typeIds) returns_
        else
          constraints_, returns_
      else
        constraints_, returns_

    groupedTypeIds |> Seq.fold mergeRegister (Set.empty, Map.empty)

  /// Construct function summary for analyzing caller
  let build address name platform (result: AnalysisResult) =
    (* If given variable is parameter, then retrieve its parameter index *)
    let filterParams (reg, tid: TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    (* Summarize parameter type information *)
    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    (* Summarize output register information from all leaf nodes. *)
    (* Group type Ids as the key with corresponding register. *)
    let groupedExitTypeIds =
      result.LeafStates
      |> Map.toSeq
      |> Seq.collect (fun (_, state) -> state.CurrentRegisters |> Map.toSeq)
      |> Seq.groupBy fst
      |> Seq.map (fun (regId, entries) ->
        regId, entries |> Seq.map snd |> Set.ofSeq)

    let mergeConstraints, returnTidMap =
      mergeExitRegisterTypeIds platform groupedExitTypeIds

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = Set.union result.TypeConstraints mergeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
