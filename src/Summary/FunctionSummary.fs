namespace PointerAnalyzer.Summary

open B2R2
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

type FunctionSummary =
  { Address: Addr
    Name: string
    Parameters: Map<int, TypeId>
    Returns: Map<int, TypeId>
    Constraints: ConstraintSet
    NextTypeId: TypeId }

module FunctionSummary =
  let empty address name nextTypeId =
    { Address = address
      Name = name
      Parameters = Map.empty
      Returns = Map.empty
      Constraints = Set.empty
      NextTypeId = nextTypeId }
