module PointerAnalyzer.TypeInference.ResolvedType

open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

/// <summary>
/// Final type of each SSA varible.
/// </summary>
type ResolvedType =
  | Address
  | Value
  | Conflict
  | Unknown

  member this.ToOutputString =
    match this with
    | Address -> "Address"
    | Value -> "Value"
    | Conflict -> "Conflict"
    | Unknown -> "Unknown"

module ResolvedType =
  /// Transform type id into corresponding resolved type
  let ofTypeId constraints conflicts typeId =
    if Set.contains typeId conflicts then
      Conflict
    else
      let isAddress = Set.contains (TypeConstraint.Address typeId) constraints

      let isValue = Set.contains (TypeConstraint.Value typeId) constraints

      match isAddress, isValue with
      | true, false -> Address
      | false, true -> Value
      | true, true -> Conflict
      | false, false -> Unknown

type ResolvedTypeInfo = { TypeId: TypeId; Type: ResolvedType }

module ResolvedTypeInfo =
  let toDebugString info =
    sprintf "%s(t%d)" info.Type.ToOutputString info.TypeId

  let ofTypeId constraints conflicts typeId =
    { TypeId = typeId
      Type = ResolvedType.ofTypeId constraints conflicts typeId }

/// <summary>
/// Represents the final type of each SSA variable
/// after solving type constraints
/// </summary>
type ResolvedTypeMap = Map<Variable, ResolvedTypeInfo>

module ResolvedTypeMap =
  /// Transform all types idof given type indicator map
  /// into corresponding resolved type
  let build constraints conflicts (typeIndicators: TypeIndicatorMap) =
    typeIndicators
    |> Map.toSeq
    |> Seq.map (fun (variable, typeId) ->
      variable, ResolvedTypeInfo.ofTypeId constraints conflicts typeId)
    |> Map.ofSeq
