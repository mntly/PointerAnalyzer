module PointerAnalyzer.Summary.FunctionSummaryBuilder

open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Summary

module FunctionSummaryBuilder =
  /// Select type Id which the corresdonding variable satisfies given
  /// condition, and return as mapping from index to type Id. This is used for
  /// extracting the type Id of parameters and return values of each index.
  let private selectByIdentifier
    identifierCond
    (entries: (int * Variable * PointerAnalyzer.AbsDom.TypeIdMap.TypeId) seq)
    : Map<int, PointerAnalyzer.AbsDom.TypeIdMap.TypeId> =
    let extractRegId
      (_paramIdx: int, reg, _tid: PointerAnalyzer.AbsDom.TypeIdMap.TypeId)
      =
      reg.Identifier

    let chooseReg (paramIdx: int, regSeq) =
      let _, _, typeId = identifierCond extractRegId regSeq
      paramIdx, typeId

    let sameParamIdxSeq = Seq.groupBy (fun (index, _, _) -> index) entries

    sameParamIdxSeq |> Seq.map chooseReg |> Map.ofSeq

  /// Construct function summary for analyzing caller
  let build address name (platform: Platform) (result: AnalysisResult) =
    let filterParams (reg, tid: PointerAnalyzer.AbsDom.TypeIdMap.TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let filterReturns (reg, tid: PointerAnalyzer.AbsDom.TypeIdMap.TypeId) =
      match platform.TryReturnIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    let returnTidMap =
      typeIndSeq |> Seq.choose filterReturns |> selectByIdentifier Seq.maxBy

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = result.TypeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
