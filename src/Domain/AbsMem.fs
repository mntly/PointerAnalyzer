module PointerAnalyzer.AbsDom.AbsMem

open PointerAnalyzer
open PointerAnalyzer.AbsDom.Functor
open PointerAnalyzer.AbsDom.AbsLoc
open PointerAnalyzer.AbsDom.AbsVal

type AbsMem = Map<AbsLoc, AbsVal>

type AbsMemModule (architecture: Architecture) =
  inherit
    MapDomain<AbsLoc, AbsVal> (
      AbsLocDomain.create architecture,
      AbsValDomain.create architecture
    )

  let absVal = AbsValDomain.create architecture

  member __.findSet locs mem =
    Set.fold (fun acc loc -> absVal.join acc (__.find loc mem)) absVal.bot locs

  member __.weakStore loc value mem =
    let old = __.find loc mem
    __.add loc (absVal.join old value) mem

  member __.strongStore loc value mem = __.add loc value mem

  member __.store locs value mem =
    // Need to handle Unkonwn
    if Set.count locs = 1 then
      __.strongStore (Set.minElement locs) value mem
    else
      Set.fold (fun acc loc -> __.weakStore loc value acc) mem locs

module AbsMemDomain =
  let create architecture = AbsMemModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
