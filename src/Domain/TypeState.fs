module PointerAnalyzer.AbsDom.TypeState

open PointerAnalyzer.AbsDom.TypeMap

type TypeConstraint =
  | Same of Set<TypePtr>
  | AddResult of TypePtr * TypePtr * TypePtr
  | SubResult of TypePtr * TypePtr * TypePtr

type TypeConstraintSet = Set<TypeConstraint>

type TypeState =
  { TypeMap: TypeMap
    Constraints: TypeConstraintSet
    NextTypePtr: int }

type TypeStateModule (startTypePtr: int) =
  let typeMap = TypeMapDomain.create ()
  let startTypePtr = max startTypePtr TypePtr.firstFreshId

  member __.bot =
    { TypeMap = typeMap.bot
      Constraints = Set.empty
      NextTypePtr = startTypePtr }

  member __.fresh state =
    let ptr = TypePtr state.NextTypePtr
    ptr, { state with NextTypePtr = state.NextTypePtr + 1 }

  member __.addConstraint constraint_ state =
    { state with
        Constraints = Set.add constraint_ state.Constraints }

  member this.addSame ptrs state =
    if Set.count ptrs <= 1 then
      state
    else
      this.addConstraint (Same ptrs) state

  member this.addAddResult result left right state =
    this.addConstraint (AddResult (result, left, right)) state

  member this.addSubResult result left right state =
    this.addConstraint (SubResult (result, left, right)) state

  member __.findType ptr state = typeMap.find ptr state.TypeMap

  member __.addType ptr typ state =
    { state with
        TypeMap = typeMap.add ptr typ state.TypeMap }

  member __.leq x y =
    typeMap.leq x.TypeMap y.TypeMap
    && Set.isSubset x.Constraints y.Constraints
    && x.NextTypePtr <= y.NextTypePtr

  member __.join x y =
    { TypeMap = typeMap.join x.TypeMap y.TypeMap
      Constraints = Set.union x.Constraints y.Constraints
      NextTypePtr = max x.NextTypePtr y.NextTypePtr }

  member __.joinTypeMap left right = typeMap.join left right

module TypeStateDomain =
  let create startTypePtr = TypeStateModule startTypePtr
