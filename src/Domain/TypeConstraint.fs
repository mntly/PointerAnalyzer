module PointerAnalyzer.AbsDom.TypeConstraint

open PointerAnalyzer.AbsDom.TypeMap

type TypeConstraint =
  | Address of TypeId
  | Value of TypeId
  | Same of Set<TypeId>
  | AddResult of TypeId * TypeId * TypeId
  | SubResult of TypeId * TypeId * TypeId

  member this.TypeIds =
    match this with
    | Address typeId
    | Value typeId -> Set.singleton typeId
    | Same typeIds -> typeIds
    | AddResult (result, left, right)
    | SubResult (result, left, right) -> Set.ofList [ result; left; right ]

  member this.ToString =
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

  let toString (constraint_: TypeConstraint) = constraint_.ToString

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
