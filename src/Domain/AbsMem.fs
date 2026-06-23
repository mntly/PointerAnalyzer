module PointerAnalyzer.AbsDom.AbsMem

open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.TypeMap

/// L = <memory SSA version, concrete address>
[<StructuralEquality; StructuralComparison>]
type MemLoc = { Version: int; Address: uint64 }

type MemVal = { Value: AbsVal; TypeId: TypeId }

type AbsMem = Map<MemLoc, MemVal>

type AbsMemModule (architecture: Architecture) =
  let absVal = AbsValDomain.create architecture

  member _.bot: AbsMem = Map.empty

  member _.location version address =
    { Version = version; Address = address }

  member _.tryFind location (memory: AbsMem) = Map.tryFind location memory

  member _.add location cell (memory: AbsMem) = Map.add location cell memory

  member _.updateVersion prevVersion newVersion (memory: AbsMem) =
    if prevVersion = newVersion then
      memory
    else
      memory
      |> Map.fold
        (fun acc location value ->
          if location.Version <> prevVersion then
            acc
          else
            let newLoc = { location with Version = newVersion }

            if Map.containsKey newLoc acc then
              acc
            else
              Map.add newLoc value acc)
        memory

  member _.join left (right: AbsMem) =
    right
    |> Map.fold
      (fun acc location value ->
        match Map.tryFind location acc with
        | None -> Map.add location value acc
        | Some old ->
          let typeId =
            if old.TypeId = value.TypeId then old.TypeId else old.TypeId
          // ToDo! Add constraint Same TypeId

          Map.add
            location
            { Value = absVal.join old.Value value.Value
              TypeId = typeId }
            acc)
      left

  member _.toString memory =
    memory
    |> Map.toList
    |> List.map (fun (location, value) ->
      sprintf
        "<MEM_%d, 0x%x> |-> <%s, t%d>"
        location.Version
        location.Address
        (absVal.toString value.Value)
        value.TypeId)
    |> String.concat "\n"

module AbsMemDomain =
  let create architecture = AbsMemModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
