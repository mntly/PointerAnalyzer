module PointerAnalyzer.Summary.FunctionSummaryBuilder

open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.CallingConvention
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Summary

module FunctionSummaryBuilder =
  let private selectByIdentifier
    identifierCond
    (entries: (int * Variable * PointerAnalyzer.AbsDom.TypeMap.TypeId) seq)
    : Map<int, PointerAnalyzer.AbsDom.TypeMap.TypeId> =
    let extractRegId
      (_paramIdx: int, reg, _tid: PointerAnalyzer.AbsDom.TypeMap.TypeId)
      =
      reg.Identifier

    let chooseReg (paramIdx: int, regSeq) =
      let _, _, typeId = identifierCond extractRegId regSeq
      paramIdx, typeId

    let sameParamIdxSeq = Seq.groupBy (fun (index, _, _) -> index) entries

    sameParamIdxSeq |> Seq.map chooseReg |> Map.ofSeq

  let build
    address
    name
    (convention: CallingConvention)
    (result: AnalysisResult)
    =
    let filterParams (reg, tid: PointerAnalyzer.AbsDom.TypeMap.TypeId) =
      match convention.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let filterReturns (reg, tid: PointerAnalyzer.AbsDom.TypeMap.TypeId) =
      match convention.TryReturnIndex reg with
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
