module PointerAnalyzer.AbsDom.TypeConstraint

open PointerAnalyzer.AbsDom.TypeMap

type TypeConstraint =
  | Address of TypeId
  | Value of TypeId
  | Same of Set<TypeId>
  | AddResult of TypeId * TypeId * TypeId
  | SubResult of TypeId * TypeId * TypeId

type ConstraintSet = Set<TypeConstraint>

module TypeConstraint =
  let typeIds =
    function
    | Address typeId
    | Value typeId -> Set.singleton typeId
    | Same typeIds -> typeIds
    | AddResult (result, left, right)
    | SubResult (result, left, right) ->
      Set.ofList [ result; left; right ]
