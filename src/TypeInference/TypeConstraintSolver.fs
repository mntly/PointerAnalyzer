module PointerAnalyzer.TypeInfer.TypeConstraintSolver

open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeIdMap

/// <summary>
/// Solution of Type Constraint Solving process.
/// </summary>
/// <remarks>
/// <c>Constraints</c> is set of TypeConstraints from main-analysis.
/// <c>Conflicts</c> is set of Type Ids that are inferred as both Address and
/// Value.
/// <c>Derivations</c> represents the type propagation history per inferred
/// concrete type.
/// </remarks>
type TypeSolution =
  { Constraints: ConstraintSet
    Conflicts: Set<TypeId>
    Derivations: Map<TypeFact, TypeDerivation> option }

type private NormalizedConstraints =
  { SameRootTypeIds: Set<TypeId>
    OriginalConstraints: ConstraintSet
    Constraints: ConstraintSet
    SameRootChildsMap: Map<TypeId, Set<TypeId>>
    Address: Set<TypeId>
    Value: Set<TypeId>
    AddResults: Set<TypeId * TypeId * TypeId>
    SubResults: Set<TypeId * TypeId * TypeId> }

type private SaturationState =
  { Address: Set<TypeId>
    Value: Set<TypeId> }

type TypeConstraintSolverModule () =
  (*
    These lists are intentionally shared by saturation and provenance. A rule
    absent here neither changes inference nor appears in a debug explanation.
  *)
  // let enabledAddRules: int list = [ 0; 1; 2; 3; 4 ]
  let enabledAddRules: int list = []
  // let enabledSubRules: int list = [ 0; 1; 2; 3; 4 ]
  let enabledSubRules: int list = []

  let addMany ids set =
    Set.fold (fun result id -> Set.add id result) set ids

  (* Find Root of sameRelation Map: 6 |-> 4 |-> 2 => Find 2 *)
  let rec find sameRelation typeId =
    match Map.tryFind typeId sameRelation with
    | Some parent when parent <> typeId -> find sameRelation parent
    | _ -> typeId

  let union left right sameRelation =
    let leftRoot = find sameRelation left
    let rightRoot = find sameRelation right

    if leftRoot = rightRoot then
      (* Mapping is already done *)
      sameRelation
    else
      (* Map new relationship *)
      let root = min leftRoot rightRoot
      let child = max leftRoot rightRoot
      Map.add child root sameRelation

  let unionSame ids sameRelation =
    match Set.toList ids with
    | []
    | [ _ ] ->
      (* Ignore Singleton Set *)
      sameRelation
    | head :: tail ->
      List.fold (fun sameRel id -> union head id sameRel) sameRelation tail

  let normalize typeIds constraints =
    let allTypeIds =
      constraints
      |> Seq.map TypeConstraint.typeIds
      |> Seq.fold Set.union typeIds

    (* Same(2, 4), Same(4, 5, 6) => Map: 2 |-> 2, 4 |-> 2, 5 |-> 2,6 |-> 2 *)
    let sameRelationBase =
      allTypeIds |> Set.fold (fun parent id -> Map.add id id parent) Map.empty

    let sameRelation =
      constraints
      |> Set.fold
        (fun sameRel constraint_ ->
          match constraint_ with
          | Same ids -> unionSame ids sameRel
          | _ -> sameRel)
        sameRelationBase

    let findRoot typeId = find sameRelation typeId

    (* Construct SameRoot -> SameChilds map *)
    let grouping groups typeId =
      let typeIdRoot = findRoot typeId

      let expendedChilds =
        match Map.tryFind typeIdRoot groups with
        | Some tidSet -> Set.add typeId tidSet
        | None -> Set.singleton typeId

      Map.add typeIdRoot expendedChilds groups

    let sameRootChildsMap = Set.fold grouping Map.empty allTypeIds

    (* Add new constraint with merged Same relation *)
    let addMergedSame constSet mergedSame =
      if Set.count mergedSame <= 1 then
        constSet
      else
        Set.add (Same mergedSame) constSet

    let normalizedSame: NormalizedConstraints =
      { SameRootTypeIds = allTypeIds |> Set.map findRoot
        OriginalConstraints = constraints
        Constraints =
          (* Possible Optimization: Remove old Same relations *)
          sameRootChildsMap
          |> Map.toSeq
          |> Seq.map snd
          |> Seq.fold addMergedSame Set.empty
        SameRootChildsMap = sameRootChildsMap
        Address = Set.empty
        Value = Set.empty
        AddResults = Set.empty
        SubResults = Set.empty }

    (* Change SameChilds to SameRoot *)
    let changeSameChilds2Root (normSame: NormalizedConstraints) constraint_ =
      match constraint_ with
      | Address typeId ->
        { normSame with
            Address = Set.add (findRoot typeId) normSame.Address }
      | Value typeId ->
        { normSame with
            Value = Set.add (findRoot typeId) normSame.Value }
      | Same _ -> normSame
      | AddResult (result, left, right) ->
        { normSame with
            AddResults =
              Set.add
                (findRoot result, findRoot left, findRoot right)
                normSame.AddResults }
      | SubResult (result, left, right) ->
        { normSame with
            SubResults =
              Set.add
                (findRoot result, findRoot left, findRoot right)
                normSame.SubResults }

    Set.fold changeSameChilds2Root normalizedSame constraints

  let saturateAdd (state: SaturationState) (result, left, right) =
    let isAddress typeId = Set.contains typeId state.Address
    let isValue typeId = Set.contains typeId state.Value

    (* "Addr" = Addr + Val | Val + Addr *)
    let rule1 result l r (typeAddr, typeVal) =
      // if isAddress l && isValue r then
      //   Set.add result typeAddr, typeVal
      // else if isValue l && isAddress r then
      //   Set.add result typeAddr, typeVal
      // else
      //   typeAddr, typeVal

      if isAddress l then Set.add result typeAddr, typeVal
      else if isAddress r then Set.add result typeAddr, typeVal
      else typeAddr, typeVal

    (* Addr = "Addr" + Val | Val + "Addr" *)
    let rule2 result l r (typeAddr, typeVal) =
      if isAddress result && isValue r then
        Set.add l typeAddr, typeVal
      else if isAddress result && isValue l then
        Set.add r typeAddr, typeVal
      else
        typeAddr, typeVal

    (* Addr = Addr + "Val" | "Val" + Addr *)
    let rule3 result l r (typeAddr, typeVal) =
      if isAddress result && isAddress r then
        typeAddr, Set.add l typeVal
      else if isAddress result && isAddress l then
        typeAddr, Set.add r typeVal
      else
        typeAddr, typeVal

    (* "Val" = Val + Val *)
    let rule4 result l r (typeAddr, typeVal) =
      if isValue l && isValue r then
        typeAddr, Set.add result typeVal
      else
        typeAddr, typeVal

    (* Val = "Val" + "Val" *)
    let rule5 result l r (typeAddr, typeVal) =
      if isValue result then
        typeAddr, Set.add l typeVal |> Set.add r
      else
        typeAddr, typeVal

    let allRules = [ rule1; rule2; rule3; rule4; rule5 ]
    let rules = enabledAddRules |> List.map (fun index -> allRules[index])

    let addrToAdd, valToAdd =
      List.fold
        (fun (addrNew, valNew) rule -> rule result left right (addrNew, valNew))
        (Set.empty, Set.empty)
        rules

    { Address = addMany addrToAdd state.Address
      Value = addMany valToAdd state.Value }

  let saturateSub (state: SaturationState) (result, left, right) =
    let isAddress typeId = Set.contains typeId state.Address
    let isValue typeId = Set.contains typeId state.Value

    (* "Addr" = Addr - Val *)
    let rule1 result l r (typeAddr, typeVal) =
      if isAddress l && isValue r then
        Set.add result typeAddr, typeVal
      else
        typeAddr, typeVal

    (* "Val" = Addr - Addr *)
    let rule2 result l r (typeAddr, typeVal) =
      if isAddress l && isAddress r then
        typeAddr, Set.add result typeVal
      else
        typeAddr, typeVal

    (* "Val" = Val - "Val" *)
    let rule3 result l r (typeAddr, typeVal) =
      if isValue l then
        typeAddr, Set.add result typeVal |> Set.add r
      else
        typeAddr, typeVal

    (* Addr = "Addr" - "Val" *)
    let rule4 result l r (typeAddr, typeVal) =
      if isAddress result then
        typeAddr, Set.add r typeVal
      else
        typeAddr, typeVal

    let rule5 result l r (typeAddr, typeVal) =
      if isAddress result then
        Set.add l typeAddr, typeVal
      else
        typeAddr, typeVal

    let allRules = [ rule1; rule2; rule3; rule4; rule5 ]
    let rules = enabledSubRules |> List.map (fun index -> allRules[index])

    let addrToAdd, valToAdd =
      List.fold
        (fun (addrNew, valNew) rule -> rule result left right (addrNew, valNew))
        (Set.empty, Set.empty)
        rules

    { Address = addMany addrToAdd state.Address
      Value = addMany valToAdd state.Value }

  let rec saturate (normalized: NormalizedConstraints) state =
    let nextFromAdd = normalized.AddResults |> Set.fold saturateAdd state

    let next = normalized.SubResults |> Set.fold saturateSub nextFromAdd

    if next = state then state else saturate normalized next

  (* Propagate SameRoot Type to its Childs *)
  let expandFacts normalized saturated =
    let expEachRoot expResultSet sameRoot =
      let childSet =
        match Map.tryFind sameRoot normalized.SameRootChildsMap with
        | Some childSet -> childSet
        | None -> Set.singleton sameRoot

      Set.union childSet expResultSet

    let expand expResultSet =
      Set.fold expEachRoot Set.empty expResultSet

    let addressIds = expand saturated.Address
    let valueIds = expand saturated.Value

    (* Update ConstraintSet *)
    let updateConstSet constraints addrSet valSet =
      let updateAddr =
        Set.fold
          (fun constSet typeId ->
            if typeId = TypeIds.value then
              constSet
            else
              Set.add (Address typeId) constSet)
          constraints
          addrSet

      let updateVal =
        Set.fold
          (fun constSet typeId ->
            if typeId = TypeIds.address then
              constSet
            else
              Set.add (Value typeId) constSet)
          updateAddr
          valSet

      updateVal

    let constraints =
      updateConstSet normalized.OriginalConstraints addressIds valueIds

    let conflicts = Set.intersect addressIds valueIds

    constraints, conflicts

  /// Represent given typeId is Addrres type
  let addressFact typeId =
    { FactType = AddressFact
      TypeId = typeId }

  /// Represent given typeId is Value type
  let valueFact typeId =
    { FactType = ValueFact
      TypeId = typeId }

  /// Build one compact causal proof per inferred fact. The work list visits a
  /// fact only once, so provenance memory is bounded by two facts per type ID.
  let buildDerivations constraints =
    (* Construct mapping from type id to constraint containing it *)
    let addIndex typeId constraint_ index =
      let previous = Map.tryFind typeId index |> Option.defaultValue []
      Map.add typeId (constraint_ :: previous) index

    (* Construct mapping from type id to constraint containing it *)
    let index =
      constraints
      |> Set.fold
        (fun index constraint_ ->
          TypeConstraint.typeIds constraint_
          |> Set.fold (fun acc typeId -> addIndex typeId constraint_ acc) index)
        Map.empty

    (* Check inferred type is already stored *)
    let hasFact fact proofs = Map.containsKey fact proofs

    (* Store one newly inferred concrete type and its reason *)
    (*
      constraint_: type constraint used to infer
      premises: already known inferred concrete type to infer current type
      result: newly inferred type
      proofs: mapping conclusion to premises and constraint
      queue: newly inferred types needed to be propagated through related
            constraints
    *)
    let addCandidate constraint_ premises (result: TypeFact) (proofs, queue) =
      if hasFact result proofs then
        proofs, queue
      else
        Map.add
          result
          { Constraint = constraint_
            Premises = premises }
          proofs,
        result :: queue

    (* Propagate inferred type(fact) accross Same(constraint_, ids) *)
    (* Return: current constraint, premises, conclusion *)
    let sameCandidates constraint_ fact ids =
      ids
      |> Set.remove fact.TypeId
      |> Set.toList
      |> List.map (fun typeId ->
        constraint_, [ fact ], { fact with TypeId = typeId })

    (*
      Propagate inferred type accross AddResult by checking already inferred
      types
    *)
    (* Return: current constraint, premises, conclusion *)
    let addCandidates constraint_ proofs (result, left, right) =
      (* Check given typeId is inferred as Address *)
      let a id = hasFact (addressFact id) proofs
      (* Check given typeId is inferred as Value *)
      let v id = hasFact (valueFact id) proofs

      (* Propagate inferred type accross several AddRules *)
      let candidate rule =
        match rule with
        | 0 when a left ->
          [ constraint_, [ addressFact left ], addressFact result ]
        | 0 when a right ->
          [ constraint_, [ addressFact right ], addressFact result ]
        | 1 when a result && v right ->
          [ constraint_,
            [ addressFact result; valueFact right ],
            addressFact left ]
        | 1 when a result && v left ->
          [ constraint_,
            [ addressFact result; valueFact left ],
            addressFact right ]
        | 2 when a result && a right ->
          [ constraint_,
            [ addressFact result; addressFact right ],
            valueFact left ]
        | 2 when a result && a left ->
          [ constraint_,
            [ addressFact result; addressFact left ],
            valueFact right ]
        | 3 when v left && v right ->
          [ constraint_, [ valueFact left; valueFact right ], valueFact result ]
        | 4 when v result ->
          [ constraint_, [ valueFact result ], valueFact left
            constraint_, [ valueFact result ], valueFact right ]
        | _ -> []

      enabledAddRules |> List.collect candidate

    (*
      Propagate inferred type accross SubResult by checking already inferred
      types
    *)
    (* Return: current constraint, premises, conclusion *)
    let subCandidates constraint_ proofs (result, left, right) =
      (* Check given typeId is inferred as Address *)
      let a id = hasFact (addressFact id) proofs
      (* Check given typeId is inferred as Value *)
      let v id = hasFact (valueFact id) proofs

      (* Propagate inferred type accross several AddRules *)
      let candidate rule =
        match rule with
        | 0 when a left && v right ->
          [ constraint_,
            [ addressFact left; valueFact right ],
            addressFact result ]
        | 1 when a left && a right ->
          [ constraint_,
            [ addressFact left; addressFact right ],
            valueFact result ]
        | 2 when v left ->
          [ constraint_, [ valueFact left ], valueFact result
            constraint_, [ valueFact left ], valueFact right ]
        | 3 when a result ->
          [ constraint_, [ addressFact result ], valueFact right ]
        | 4 when a result ->
          [ constraint_, [ addressFact result ], addressFact left ]
        | _ -> []

      enabledSubRules |> List.collect candidate

    (* Store initial type history from concrete inferred type *)
    let seed (proofs, queue) constraint_ =
      match constraint_ with
      | Address typeId ->
        addCandidate constraint_ [] (addressFact typeId) (proofs, queue)
      | Value typeId ->
        addCandidate constraint_ [] (valueFact typeId) (proofs, queue)
      | _ -> proofs, queue

    let initialProofs, initialQueue = Set.fold seed (Map.empty, []) constraints

    (* Given inferred type queue to propgate, propaget inferred type *)
    let rec processQueue proofs queue =
      match queue with
      | [] -> proofs
      | fact :: rest ->
        (* Extract constraint containing current inferred concrete type *)
        let related = Map.tryFind fact.TypeId index |> Option.defaultValue []

        (* Propagate inferred concrete type accorss type constraints *)
        (* [(current constraint, premises, conclusion)] *)
        let candidates: (TypeConstraint * TypeFact list * TypeFact) list =
          related
          |> List.collect (fun constraint_ ->
            match constraint_ with
            | Same ids -> sameCandidates constraint_ fact ids
            | AddResult (result, left, right) ->
              addCandidates constraint_ proofs (result, left, right)
            | SubResult (result, left, right) ->
              subCandidates constraint_ proofs (result, left, right)
            | Address _
            | Value _ -> [])

        (* Propagate inferred type, and update mapping and queue to propate *)
        let proofs, queue =
          List.fold
            (fun acc (constraint_, premises, result) ->
              addCandidate constraint_ premises result acc)
            (proofs, rest)
            candidates

        processQueue proofs queue

    processQueue initialProofs initialQueue

  /// Solve given type constraint
  member _.solve trackProvenance typeIds constraints =
    let normalized = normalize typeIds constraints

    let initialSatuState =
      { Address = normalized.Address
        Value = normalized.Value }

    let saturated = saturate normalized initialSatuState

    let expandedConstraints, conflicts = expandFacts normalized saturated

    { Constraints = expandedConstraints
      Conflicts = conflicts
      Derivations =
        if trackProvenance then
          (*
            If debug history is enabled, propagate type from concrete inferred
            type to type rules
          *)
          Some (buildDerivations constraints)
        else
          None }

module TypeConstraintSolver =
  let create () = TypeConstraintSolverModule ()
