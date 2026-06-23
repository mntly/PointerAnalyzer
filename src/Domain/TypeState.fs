module PointerAnalyzer.AbsDom.TypeState

open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.TypeInfer.TypeConstraintSolver
open PointerAnalyzer.AbsDom.TypeMap

type TypeState =
  { TypeIndicators: TypeIndicatorMap
    NextTypeId: TypeId
    Constraints: ConstraintSet
    Conflicts: Set<TypeId> }

type TypeStateModule (startTypeId: TypeId) =
  let typeMap = TypeMapDomain.create ()
  let solver = TypeConstraintSolver.create ()

  member _.bot =
    { TypeIndicators = typeMap.bot
      NextTypeId = startTypeId
      Constraints = Set.empty
      Conflicts = Set.empty }

  member _.fresh state =
    state.NextTypeId,
    { state with
        NextTypeId = state.NextTypeId + 1 }

  member _.set variable typeId state =
    { state with
        TypeIndicators = typeMap.add variable typeId state.TypeIndicators }

  member this.getOrFresh variable state =
    match typeMap.tryFind variable state.TypeIndicators with
    | Some typeId -> typeId, state
    | None ->
      let typeId, state = this.fresh state

      typeId, this.set variable typeId state

  member _.tryFind variable state =
    typeMap.tryFind variable state.TypeIndicators

  member _.addConstraint constraint_ (state: TypeState) =
    { state with
        Constraints = Set.add constraint_ state.Constraints }

  member this.addAddress typeId state =
    this.addConstraint (Address typeId) state

  member this.addValue typeId state = this.addConstraint (Value typeId) state

  member this.addSame typeIds state =
    let typeIds = Set.ofSeq typeIds

    if Set.count typeIds <= 1 then
      state
    else
      this.addConstraint (Same typeIds) state

  member this.addAddResult result left right state =
    this.addConstraint (AddResult (result, left, right)) state

  member this.addSubResult result left right state =
    this.addConstraint (SubResult (result, left, right)) state

  member _.join left right =
    { TypeIndicators =
        right.TypeIndicators
        |> Map.fold
          (fun result variable typeId ->
            if Map.containsKey variable result then
              result
            else
              Map.add variable typeId result)
          left.TypeIndicators
      NextTypeId = max left.NextTypeId right.NextTypeId
      Constraints = Set.union left.Constraints right.Constraints
      Conflicts = Set.union left.Conflicts right.Conflicts }

  member _.solve state =
    let mappedTypeIds =
      state.TypeIndicators |> Map.toSeq |> Seq.map snd |> Set.ofSeq

    let constrainedTypeIds =
      state.Constraints |> Seq.map TypeConstraint.typeIds |> Set.unionMany

    let typeIds = Set.union mappedTypeIds constrainedTypeIds
    let solution = solver.solve typeIds state.Constraints

    { state with
        Constraints = solution.Constraints
        Conflicts = solution.Conflicts }

  member _.constraintToString =
    function
    | Address typeId -> sprintf "Address(t%d)" typeId
    | Value typeId -> sprintf "Value(t%d)" typeId
    | Same typeIds ->
      typeIds
      |> Set.toList
      |> List.map (sprintf "tid_%d")
      |> String.concat ", "
      |> sprintf "Same({%s})"
    | AddResult (result, left, right) ->
      sprintf "AddResult(t%d, t%d, t%d)" result left right
    | SubResult (result, left, right) ->
      sprintf "SubResult(t%d, t%d, t%d)" result left right

  member _.typeEntryToString variable typeId =
    sprintf "%s |-> tid_%d" (B2R2.BinIR.SSA.Variable.ToString variable) typeId

module TypeStateDomain =
  let create startTypeId = TypeStateModule startTypeId

  let createDefault () = create 0
