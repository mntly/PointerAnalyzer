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

  /// Merge return-register type IDs from all leaf states.
  let private mergeReturnTypeIds groupedTypeIds =
    let mergeRegister (constraints_, returns_) (regId, typeIds: Set<TypeId>) =
      if Set.isEmpty typeIds then
        constraints_, returns_
      else
        let representative = Set.minElement typeIds

        let constraints_ =
          if Set.count typeIds <= 1 then
            constraints_
          else
            Set.add (Same typeIds) constraints_

        constraints_, Map.add regId representative returns_

    groupedTypeIds |> Seq.fold mergeRegister (Set.empty, Map.empty)

  /// Construct function summary for analyzing caller
  let build address name platform returnRegisters (result: AnalysisResult) =
    (* If given variable is parameter, then retrieve its parameter index *)
    let filterParams (reg, tid: TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    (* Summarize parameter type information *)
    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    (* Summarize ABI return registers from all leaf nodes. *)
    let returnRegisters = Set.ofList returnRegisters

    let groupedReturnTypeIds =
      result.LeafStates
      |> Map.toSeq
      |> Seq.collect (fun (_, state) -> state.CurrentRegisters |> Map.toSeq)
      (* Extract only return registers *)
      |> Seq.filter (fun (regId, _) -> Set.contains regId returnRegisters)
      |> Seq.groupBy fst
      |> Seq.map (fun (regId, entries) ->
        regId, entries |> Seq.map snd |> Set.ofSeq)

    let mergeConstraints, returnTidMap = mergeReturnTypeIds groupedReturnTypeIds

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = Set.union result.TypeConstraints mergeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
