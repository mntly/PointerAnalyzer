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

  /// Select the final SSA register version for each physical register.
  let private selectRegisterOutputs entries : Map<RegisterID, TypeId> =
    let chooseReg
      (regId: RegisterID, regSeq: (RegisterID * Variable * TypeId) seq)
      =
      let _, _, typeId =
        regSeq |> Seq.maxBy (fun (_, reg: Variable, _) -> reg.Identifier)

      regId, typeId

    entries
    |> Seq.groupBy (fun (regId, _, _) -> regId)
    |> Seq.map chooseReg
    |> Map.ofSeq

  /// Construct function summary for analyzing caller
  let build address name platform (result: AnalysisResult) =
    (* If given variable is parameter, then retrieve its parameter index *)
    let filterParams (reg, tid: TypeId) =
      match platform.TryParameterIndex reg with
      | Some paramIdx -> Some (paramIdx, reg, tid)
      | None -> None

    (* If given modified variable is register, then retrieve its register id *)
    let filterRegisterOutput (reg: Variable, tid: TypeId) =
      let trivialTypes =
        Set.union
          platform.TrivialValueRegisters
          platform.TrivialAddressRegisters

      match reg.Kind with
      | VariableKind.RegVar (_, regId, _) when
        not (Set.contains regId trivialTypes)
        ->
        (*
          The register with trivial type does not need to trakc between
          caller-callee
        *)
        Some (regId, reg, tid)
      | _ -> None

    let typeIndSeq = result.FinalState.Types.TypeIndicators |> Map.toSeq

    (* Extract type Id of all variables *)
    let outputRegSeq =
      result.FinalState.RegMap
      |> Map.toSeq
      |> Seq.choose (fun (reg, _value) ->
        result.FinalState.Types.TypeIndicators
        |> Map.tryFind reg
        |> Option.map (fun typeId -> reg, typeId))

    (* Summarize parameter type information *)
    let paramIdxTidMap =
      typeIndSeq |> Seq.choose filterParams |> selectByIdentifier Seq.minBy

    (* Summarize modified register information *)
    let returnTidMap =
      outputRegSeq |> Seq.choose filterRegisterOutput |> selectRegisterOutputs

    { Address = address
      Name = name
      Parameters = paramIdxTidMap
      Returns = returnTidMap
      Constraints = result.TypeConstraints
      NextTypeId = result.FinalState.Types.NextTypeId }
