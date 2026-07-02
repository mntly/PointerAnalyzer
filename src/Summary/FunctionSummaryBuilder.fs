module PointerAnalyzer.Summary.FunctionSummaryBuilder

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA

open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Summary
open PointerAnalyzer.AbsDom.TypeConstraint

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
  let build
    address
    name
    (handle: BinHandle)
    (platform: Platform)
    (result: AnalysisResult)
    =
    let filterParams (reg, tid: PointerAnalyzer.AbsDom.TypeIdMap.TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let filterReturns (reg, tid: PointerAnalyzer.AbsDom.TypeIdMap.TypeId) =
      match platform.TryReturnIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    let tryRegId (varaible: Variable) =
      match varaible.Kind with
      | RegVar (_, regId, _) -> Some regId
      | _ -> None

    let filterGetPcThunk outputRegId (reg, tid) =
      match tryRegId reg with
      | Some regId when regId = outputRegId -> Some (0, reg, tid)
      | _ -> None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    let returnTidMap =
      match platform.CheckIntrinsic PCThunk handle address with
      | Some outputRegId ->
        (*
          If current function is get_pc_thunk,
          then set the return register as corresponding register.
        *)
        typeIndSeq
        |> Seq.choose (filterGetPcThunk outputRegId)
        |> selectByIdentifier Seq.maxBy
      | None ->
        typeIndSeq |> Seq.choose filterReturns |> selectByIdentifier Seq.maxBy

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = result.TypeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
