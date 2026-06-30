module PointerAnalyzer.AbsDom.AbsMem

open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.TypeIdMap

[<StructuralEquality; StructuralComparison>]
type MemLoc = { Version: int; Address: uint64 }

type MemVal = { Value: AbsVal; TypeId: TypeId }

/// <summary>
/// Tracks abstract value of specific address and memory version.
/// </summary>
type AbsMem = Map<MemLoc, MemVal>

type AbsMemModule (platform: Platform) =
  let absVal = AbsValDomain.create platform

  member _.bot: AbsMem = Map.empty

  /// Given memory version and address, construct MemLoc
  member _.location version address =
    { Version = version; Address = address }

  /// Load from given location
  member _.tryFind location (memory: AbsMem) = Map.tryFind location memory

  /// Store to given location as given value
  member _.add location cell (memory: AbsMem) = Map.add location cell memory

  /// Update memory version from specific version to new version.
  /// This keeps values of memory even the memory version is changed
  member _.updateVersion prevVersion newVersion (memory: AbsMem) =
    let updateVersionInner absMem loc value =
      if loc.Version <> prevVersion then
        (* Version mismatch -> Do not update *)
        absMem
      else
        let newLoc = { loc with Version = newVersion }

        if Map.containsKey newLoc absMem then
          (* Already updated *)
          absMem
        else
          Map.add newLoc value absMem

    if prevVersion = newVersion then
      memory
    else
      memory |> Map.fold updateVersionInner memory

  /// Join AbsMem
  member _.join left (right: AbsMem) =
    right
    |> Map.fold
      (fun acc location value ->
        match Map.tryFind location acc with
        | None -> Map.add location value acc
        | Some old ->
          let typeId =
            if old.TypeId = value.TypeId then old.TypeId else old.TypeId
          (*
            ToDo
              Handling about the type constraint
              between the values from same addresses
          *)
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
  let create platform = AbsMemModule platform
