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

  /// Construct function summary for analyzing caller
  let build address name platform (result: AnalysisResult) =
    (* If given variable is parameter, then retrieve its parameter index *)
    let filterParams (reg, tid: TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    (* If given modified variable is register, then retrieve its register id *)
    let filterRegisterOutput (regId: RegisterID, tid: TypeId) =
      let trivialTypes =
        Set.union
          platform.TrivialValueRegisters
          platform.TrivialAddressRegisters

      if not (Set.contains regId trivialTypes) then
        (*
          The register with trivial type does not need to track between
          caller-callee
        *)
        Some (regId, tid)
      else
        None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    (* Summarize parameter type information *)
    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    (* Summarize modified register information *)
    let returnTidMap =
      result.FinalState.CurrentRegisters
      |> Map.toSeq
      |> Seq.choose filterRegisterOutput
      |> Map.ofSeq

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = result.TypeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
