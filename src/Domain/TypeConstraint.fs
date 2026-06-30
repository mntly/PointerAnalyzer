module PointerAnalyzer.AbsDom.TypeConstraint

open PointerAnalyzer.AbsDom.TypeIdMap

/// <summary>
/// Type constraints collected during main-analysis.
/// </summary>
/// <remarks>
/// <c>Address(tid)</c> indicates the expression corresponding tid is address.
/// <c>Value(tid)</c> indicates the expression corresponding tid is value.
/// <c>Same({tid1, ..., tidN})</c> indicates the all expressions corresponding
/// each tid has same type.
/// <c>AddResult(tid0, tid1, tid2)</c> indicates the binary operation, add.
/// The expression corresponding tid0 type id is the addition of expressions
/// corresponding to tid1 and tid2 type id.
/// <c>SubResult(tid0, tid1, tid2)</c> indicates the binary operation, sub.
/// The expression corresponding tid0 type id is the subtraction of expressions
/// corresponding to tid1 and tid2 type id.
/// </remarks>
type TypeConstraint =
  | Address of TypeId
  | Value of TypeId
  | Same of Set<TypeId>
  | AddResult of TypeId * TypeId * TypeId
  | SubResult of TypeId * TypeId * TypeId

  /// Return type Ids in given TypeConstraint
  member this.TypeIds =
    match this with
    | Address typeId
    | Value typeId -> Set.singleton typeId
    | Same typeIds -> typeIds
    | AddResult (result, left, right)
    | SubResult (result, left, right) -> Set.ofList [ result; left; right ]

  override this.ToString () =
    match this with
    | Address typeId -> sprintf "Address(t%d)" typeId
    | Value typeId -> sprintf "Value(t%d)" typeId
    | Same typeIds ->
      typeIds
      |> Set.toList
      |> List.map (sprintf "t%d")
      |> String.concat ", "
      |> sprintf "Same({%s})"
    | AddResult (result, left, right) ->
      sprintf "AddResult(t%d, t%d, t%d)" result left right
    | SubResult (result, left, right) ->
      sprintf "SubResult(t%d, t%d, t%d)" result left right

module TypeConstraint =
  let typeIds (constraint_: TypeConstraint) = constraint_.TypeIds

  let toString (constraint_: TypeConstraint) = constraint_.ToString ()

type ConstraintSet = Set<TypeConstraint>

module ConstraintSet =
  let toString (constSet: ConstraintSet) =
    let header = "  Constraints:\n"

    let content =
      if Set.isEmpty constSet then
        "    <empty>"
      else
        constSet
        |> Set.map (TypeConstraint.toString >> sprintf "    %s\n")
        |> String.concat ""

    header + content
