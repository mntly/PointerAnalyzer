module PointerAnalyzer.AbsDom.SymbolConstraint

open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsInt
open PointerAnalyzer.AbsDom.AbsLoc

type SymbolConstraintMap =
  { IntConstraints: Map<SymbolInt, AbsInt>
    LocConstraints: Map<SymbolLoc, AbsLocSet> }

type SymbolConstraintModule (architecture: Architecture) =
  let absInt = AbsIntDomain.create architecture
  let absLocSet = AbsLocDomain.createSet architecture

  member __.bot =
    { IntConstraints = Map.empty
      LocConstraints = Map.empty }

  member __.tryFindInt sym constraints =
    Map.tryFind sym constraints.IntConstraints

  member __.findInt sym constraints =
    match Map.tryFind sym constraints.IntConstraints with
    | Some aint -> aint
    | None -> absInt.bot

  member __.tryFindLoc sym constraints =
    Map.tryFind sym constraints.LocConstraints

  member __.findLoc sym constraints =
    match Map.tryFind sym constraints.LocConstraints with
    | Some lint -> lint
    | None -> absLocSet.bot

  member __.addInt sym value constraints =
    let value =
      match Map.tryFind sym constraints.IntConstraints with
      | Some old -> absInt.join old value
      | None -> value

    { constraints with
        IntConstraints = Map.add sym value constraints.IntConstraints }

  member __.addIntBot sym constraints = __.addInt sym absInt.bot constraints

  member __.addLocSet sym value constraints =
    let value =
      match Map.tryFind sym constraints.LocConstraints with
      | Some old -> absLocSet.join old value
      | None -> value

    { constraints with
        LocConstraints = Map.add sym value constraints.LocConstraints }

  member __.addLoc sym loc constraints =
    __.addLocSet sym (absLocSet.make [ loc ]) constraints

  member __.join x y =
    let intConstraints =
      y.IntConstraints
      |> Map.fold
        (fun acc sym value ->
          let oldInt =
            match Map.tryFind sym acc with
            | Some old -> old
            | None -> absInt.bot

          Map.add sym (absInt.join oldInt value) acc)
        x.IntConstraints

    let locConstraints =
      y.LocConstraints
      |> Map.fold
        (fun acc sym value ->
          let oldLoc =
            match Map.tryFind sym acc with
            | Some old -> old
            | None -> absLocSet.bot

          Map.add sym (absLocSet.join oldLoc value) acc)
        x.LocConstraints

    { IntConstraints = intConstraints
      LocConstraints = locConstraints }

module SymbolConstraintDomain =
  let create architecture = SymbolConstraintModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
