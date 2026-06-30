module PointerAnalyzer.AbsDom.TypeState

open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.TypeInfer.TypeConstraintSolver
open PointerAnalyzer.AbsDom.TypeIdMap

/// <summary>
/// Store type constraints retrieved during the main-analysis step.
/// </summary>
/// <remarks>
/// <c>TypeIndicators</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.AbsDom.TypeIdMap.TypeIdMap">.
/// <c>NextTypeId</c> is next fresh Type Id.
/// <c>Constraints</c> contains type constraints retrieved during the
/// main-analysis step.
/// <c>Conflicts</c> contains type Ids that are inferred both Address and Value
/// </remarks>
type TypeState =
  { TypeIndicators: TypeIdMap
    NextTypeId: TypeId
    Constraints: ConstraintSet
    (*
      ToDo
        In current implementation,
        Conflicts are not updated during analysis
    *)
    Conflicts: Set<TypeId> }

type TypeStateModule (startTypeId: TypeId) =
  let typeMap = TypeIdMapDomain.create ()
  let solver = TypeConstraintSolver.create ()
  let startTypeId = max startTypeId TypeIds.firstFresh

  member _.bot =
    { TypeIndicators = typeMap.bot
      NextTypeId = startTypeId
      Constraints = Set.ofList [ Address TypeIds.address; Value TypeIds.value ]
      Conflicts = Set.empty }

  /// Assign new fresh type Id
  member _.fresh state =
    state.NextTypeId,
    { state with
        NextTypeId = state.NextTypeId + 1 }

  /// Set the type Id of given variable as given Type Id
  member _.set variable typeId state =
    { state with
        TypeIndicators = typeMap.add variable typeId state.TypeIndicators }

  /// Return type Id of given variable
  member _.tryFind variable state =
    typeMap.tryFind variable state.TypeIndicators

  /// Add type constraint
  member _.addConstraint constraint_ (state: TypeState) =
    { state with
        Constraints = Set.add constraint_ state.Constraints }

  /// Add Address type constraint
  member this.addAddress typeId state =
    this.addConstraint (Address typeId) state

  /// Add Value type constraint
  member this.addValue typeId state = this.addConstraint (Value typeId) state

  /// Add Same type constraint
  member this.addSame typeIds state =
    let typeIds = Set.ofSeq typeIds

    if Set.count typeIds <= 1 then
      (* Same Single: Not Same constraint *)
      state
    else
      this.addConstraint (Same typeIds) state

  /// Add AddResult(result, left, right) type constraint
  member this.addAddResult result left right state =
    this.addConstraint (AddResult (result, left, right)) state

  /// Add SubResult(result, left, right) type constraint
  member this.addSubResult result left right state =
    this.addConstraint (SubResult (result, left, right)) state

  /// Join TypeState
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

  /// Solve type constraints
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

module TypeStateDomain =
  let create startTypeId = TypeStateModule startTypeId

  let createDefault () = create 0
