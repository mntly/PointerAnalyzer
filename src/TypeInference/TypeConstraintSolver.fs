module PointerAnalyzer.TypeInfer.TypeConstraintSolver

open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

type TypeSolution =
  { Constraints: ConstraintSet
    Conflicts: Set<TypeId> }

type private NormalizedConstraints =
  { TypeIds: Set<TypeId>
    OriginalConstraints: ConstraintSet
    Constraints: ConstraintSet
    MembersByRep: Map<TypeId, Set<TypeId>>
    Address: Set<TypeId>
    Value: Set<TypeId>
    AddResults: Set<TypeId * TypeId * TypeId>
    SubResults: Set<TypeId * TypeId * TypeId> }

type private SaturationState =
  { Address: Set<TypeId>
    Value: Set<TypeId> }

type TypeConstraintSolverModule () =
  let addMany ids set = Set.fold (fun result id -> Set.add id result) set ids

  let rec find parent typeId =
    match Map.tryFind typeId parent with
    | Some next when next <> typeId -> find parent next
    | _ -> typeId

  let union left right parent =
    let leftRoot = find parent left
    let rightRoot = find parent right

    if leftRoot = rightRoot then
      parent
    else
      let root = min leftRoot rightRoot
      let child = max leftRoot rightRoot
      Map.add child root parent

  let unionSame ids parent =
    match Set.toList ids with
    | []
    | [ _ ] -> parent
    | head :: tail -> List.fold (fun parent id -> union head id parent) parent tail

  let normalize typeIds constraints =
    let allTypeIds =
      constraints
      |> Seq.map TypeConstraint.typeIds
      |> Seq.fold Set.union typeIds

    let parent =
      allTypeIds |> Set.fold (fun parent id -> Map.add id id parent) Map.empty

    let parent =
      constraints
      |> Set.fold
        (fun parent constraint_ ->
          match constraint_ with
          | Same ids -> unionSame ids parent
          | _ -> parent)
        parent

    let rep typeId = find parent typeId

    let membersByRep =
      allTypeIds
      |> Set.fold
        (fun groups typeId ->
          let representative = rep typeId
          let members = Map.tryFind representative groups |> Option.defaultValue Set.empty
          Map.add representative (Set.add typeId members) groups)
        Map.empty

    let addSameConstraint members constraints =
      if Set.count members <= 1 then
        constraints
      else
        Set.add (Same members) constraints

    let normalized: NormalizedConstraints =
      { TypeIds = allTypeIds |> Set.map rep
        OriginalConstraints = constraints
        Constraints =
          membersByRep
          |> Map.toSeq
          |> Seq.map snd
          |> Seq.fold (fun result members -> addSameConstraint members result) Set.empty
        MembersByRep = membersByRep
        Address = Set.empty
        Value = Set.empty
        AddResults = Set.empty
        SubResults = Set.empty }

    constraints
    |> Set.fold
      (fun (normalized: NormalizedConstraints) constraint_ ->
        match constraint_ with
        | Address typeId ->
          { normalized with
              Address = Set.add (rep typeId) normalized.Address }
        | Value typeId ->
          { normalized with
              Value = Set.add (rep typeId) normalized.Value }
        | Same _ -> normalized
        | AddResult (result, left, right) ->
          { normalized with
              AddResults =
                Set.add
                  (rep result, rep left, rep right)
                  normalized.AddResults }
        | SubResult (result, left, right) ->
          { normalized with
              SubResults =
                Set.add
                  (rep result, rep left, rep right)
                  normalized.SubResults })
      normalized

  let saturateAdd (state: SaturationState) (result, left, right) =
    let isAddress typeId = Set.contains typeId state.Address
    let isValue typeId = Set.contains typeId state.Value

    let addressToAdd =
      Set.empty
      |> fun ids ->
        if isAddress left && isValue right then
          Set.add result ids
        else
          ids
      |> fun ids ->
        if isValue left && isAddress right then
          Set.add result ids
        else
          ids
      |> fun ids ->
        if isAddress result && isValue left then
          Set.add right ids
        else
          ids
      |> fun ids ->
        if isAddress result && isValue right then
          Set.add left ids
        else
          ids

    let valueToAdd =
      Set.empty
      |> fun ids ->
        if isValue left && isValue right then
          Set.add result ids
        else
          ids
      |> fun ids ->
        if isAddress result && isAddress left then
          Set.add right ids
        else
          ids
      |> fun ids ->
        if isAddress result && isAddress right then
          Set.add left ids
        else
          ids
      |> fun ids ->
        if isValue result then
          ids |> Set.add left |> Set.add right
        else
          ids

    { Address = addMany addressToAdd state.Address
      Value = addMany valueToAdd state.Value }

  let saturateSub (state: SaturationState) (result, left, right) =
    let isAddress typeId = Set.contains typeId state.Address
    let isValue typeId = Set.contains typeId state.Value

    let addressToAdd =
      Set.empty
      |> fun ids ->
        if isAddress left && isValue right then
          Set.add result ids
        else
          ids
      |> fun ids ->
        if isAddress result then
          Set.add left ids
        else
          ids

    let valueToAdd =
      Set.empty
      |> fun ids ->
        if isAddress left && isAddress right then
          Set.add result ids
        else
          ids
      |> fun ids ->
        if isValue left then
          ids |> Set.add result |> Set.add right
        else
          ids
      |> fun ids ->
        if isAddress result then
          Set.add right ids
        else
          ids

    { Address = addMany addressToAdd state.Address
      Value = addMany valueToAdd state.Value }

  let rec saturate (normalized: NormalizedConstraints) state =
    let nextFromAdd =
      normalized.AddResults |> Set.fold saturateAdd state

    let next =
      normalized.SubResults |> Set.fold saturateSub nextFromAdd

    if next = state then
      state
    else
      saturate normalized next

  let expandFacts normalized saturated =
    let expand reps =
      reps
      |> Set.fold
        (fun result rep ->
          normalized.MembersByRep
          |> Map.tryFind rep
          |> Option.defaultValue (Set.singleton rep)
          |> fun members -> Set.union members result)
        Set.empty

    let addressIds = expand saturated.Address
    let valueIds = expand saturated.Value

    let constraints =
      normalized.OriginalConstraints
      |> fun constraints ->
        addressIds
        |> Set.fold (fun result typeId -> Set.add (Address typeId) result) constraints
      |> fun constraints ->
        valueIds
        |> Set.fold (fun result typeId -> Set.add (Value typeId) result) constraints

    let conflicts = Set.intersect addressIds valueIds

    { Constraints = constraints
      Conflicts = conflicts }

  member _.solve typeIds constraints =
    let normalized = normalize typeIds constraints

    let saturated =
      saturate
        normalized
        ({ Address = normalized.Address
           Value = normalized.Value }: SaturationState)

    expandFacts normalized saturated

module TypeConstraintSolver =
  let create () = TypeConstraintSolverModule ()
