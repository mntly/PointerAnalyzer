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
/// <c>ConstraintOrigins</c> tracks the source(origin) of each type constraint.
/// It tracks only one origin per type constraint. The origin tells where and
/// why corresponding type constraint is generated.
/// <c>CurrentOrigin</c> indicates the SSA statement processed now. This
/// information will be tracked if type constraint is generated from current
/// SSA statement.
/// <c>Derivations</c> represents the type propagation history per inferred
/// concrete type.
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
    Conflicts: Set<TypeId>
    ConstraintOrigins: Map<TypeConstraint, ConstraintOrigin> option
    CurrentOrigin: ConstraintOrigin option
    Derivations: Map<TypeFact, TypeDerivation> option }

type TypeStateModule (startTypeId: TypeId, trackProvenance: bool) =
  let typeMap = TypeIdMapDomain.create ()
  let solver = TypeConstraintSolver.create ()
  let startTypeId = max startTypeId TypeIds.firstFresh

  member _.bot =
    { TypeIndicators = typeMap.bot
      NextTypeId = startTypeId
      Constraints = Set.ofList [ Address TypeIds.address; Value TypeIds.value ]
      Conflicts = Set.empty
      ConstraintOrigins = if trackProvenance then Some Map.empty else None
      CurrentOrigin = None
      Derivations = None }

  /// Set statement information to analyze. If debug mode is disabled, ignore.
  member _.beginOrigin origin state =
    match state.ConstraintOrigins with
    | Some _ ->
      { state with
          CurrentOrigin = Some origin }
    | None -> state

  /// Unset analyzed statement to prevent population
  member _.endOrigin state =
    if Option.isSome state.CurrentOrigin then
      { state with CurrentOrigin = None }
    else
      state

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

  /// Add type constraint to TypeState.
  /// If debug tracking is enabled, it tracks the source(origin) per constraint.
  member _.addConstraint constraint_ (state: TypeState) =
    let origins =
      match state.ConstraintOrigins, state.CurrentOrigin with
      | Some origins, Some origin when not (Map.containsKey constraint_ origins) ->
        Some (Map.add constraint_ origin origins)
      | origins, _ -> origins

    { state with
        Constraints = Set.add constraint_ state.Constraints
        ConstraintOrigins = origins }

  /// Add given type constraint and update debug history of current statement
  /// as given annotation.
  member this.addConstraintWithAnnotation
    annotation
    constraint_
    (state: TypeState)
    =
    let state =
      match state.CurrentOrigin with
      | Some origin ->
        { state with
            CurrentOrigin = Some { origin with Annotation = annotation } }
      | None -> state

    this.addConstraint constraint_ state

  /// Add Address type constraint
  member this.addAddress typeId state =
    this.addConstraint (Address typeId) state

  /// Mark given typeId as Address with given debug annotation
  member this.addAddressWithAnnotation annotation typeId state =
    this.addConstraintWithAnnotation annotation (Address typeId) state

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

  member this.addSameWithAnnotation annotation typeIds state =
    let typeIds = Set.ofSeq typeIds

    if Set.count typeIds <= 1 then
      state
    else
      this.addConstraintWithAnnotation annotation (Same typeIds) state

  /// Add AddResult(result, left, right) type constraint
  member this.addAddResult result left right state =
    this.addConstraint (AddResult (result, left, right)) state

  /// Add AddResult(result, left, right) type constraint with given debug
  /// annotation
  member this.addAddResultWithAnnotation annotation result left right state =
    this.addConstraintWithAnnotation
      annotation
      (AddResult (result, left, right))
      state

  /// Add SubResult(result, left, right) type constraint
  member this.addSubResult result left right state =
    this.addConstraint (SubResult (result, left, right)) state

  /// Add SubResult(result, left, right) type constraint with given debug
  /// annotation
  member this.addSubResultWithAnnotation annotation result left right state =
    this.addConstraintWithAnnotation
      annotation
      (SubResult (result, left, right))
      state

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
      Conflicts = Set.union left.Conflicts right.Conflicts
      ConstraintOrigins =
        match left.ConstraintOrigins, right.ConstraintOrigins with
        | Some leftOrigins, Some rightOrigins ->
          Some (
            Map.fold
              (fun acc key value ->
                if Map.containsKey key acc then
                  (* Only store one origin per type constraint *)
                  acc
                else
                  Map.add key value acc)
              leftOrigins
              rightOrigins
          )
        | Some origins, None
        | None, Some origins -> Some origins
        | None, None -> None
      CurrentOrigin = None
      Derivations = None }

  /// Solve type constraints
  member _.solve state =
    let mappedTypeIds =
      state.TypeIndicators |> Map.toSeq |> Seq.map snd |> Set.ofSeq

    let constrainedTypeIds =
      state.Constraints |> Seq.map TypeConstraint.typeIds |> Set.unionMany

    let typeIds = Set.union mappedTypeIds constrainedTypeIds

    let solution =
      solver.solve
        (Option.isSome state.ConstraintOrigins)
        typeIds
        state.Constraints

    { state with
        Constraints = solution.Constraints
        Conflicts = solution.Conflicts
        CurrentOrigin = None
        Derivations = solution.Derivations }

  member _.constraintToString =
    function
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

module TypeStateDomain =
  let createWithProvenance startTypeId trackProvenance =
    TypeStateModule (startTypeId, trackProvenance)

  let create startTypeId = createWithProvenance startTypeId false

  let createDefault () = create 0
