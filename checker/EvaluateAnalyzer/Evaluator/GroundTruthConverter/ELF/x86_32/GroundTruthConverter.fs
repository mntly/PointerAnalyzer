module EvaluateAnalyzer.Evaluator.GroundTruthConverter.ELF.X86_32

open EvaluateAnalyzer.Evaluator.Types

let private wordSlotCount wordSize byteSize =
  (byteSize + wordSize - 1) / wordSize

/// Transform RawGT type into GT type. Type field of structure does not used.
let private evalTypeOfRaw =
  function
  | RawAddress -> Address
  | RawValue -> Value
  | RawUnknown -> Unknown
  | RawStructure _ -> Value

let private appendPath parent child =
  if System.String.IsNullOrWhiteSpace parent then child
  elif System.String.IsNullOrWhiteSpace child then parent
  else parent + "." + child

let private mergePaths left right =
  if System.String.IsNullOrWhiteSpace left then right
  elif System.String.IsNullOrWhiteSpace right then left
  elif left = right then left
  else left + "," + right

let private addLeafSlots wordSize baseOffset size typ path slots =
  if size <= 0 then
    (* This does not be held *)
    let slotIndex = max 0 (baseOffset / wordSize)

    Map.add
      slotIndex
      { Index = slotIndex
        Size = 0
        Type = typ
        Path = path }
      slots
  else
    let firstSlot = baseOffset / wordSize
    let lastSlot = (baseOffset + size - 1) / wordSize

    [ firstSlot..lastSlot ]
    |> List.fold
      (fun slots slotIndex ->
        (*
          Overlapping calculation for indicates when the fields smaller than
          WordSize are used in one slot
        *)
        (*
          ToDo!
            Need to check when the fields with smaller than WordSize are given
        *)
        let slotStart = slotIndex * wordSize
        let overlapStart = max baseOffset slotStart
        let overlapEnd = min (baseOffset + size) (slotStart + wordSize)
        let overlapSize = max 0 (overlapEnd - overlapStart)

        match Map.tryFind slotIndex slots with
        | None ->
          Map.add
            slotIndex
            { Index = slotIndex
              Size = overlapSize
              Type = typ
              Path = path }
            slots
        | Some existing ->
          (*
            Multiple fields sharing one machine-word slot are transferred and
            inferred as one scalar word. Keep one Value slot for evaluation.
          *)
          Map.add
            slotIndex
            { existing with
                Size = max existing.Size overlapSize
                Type = Value
                Path = mergePaths existing.Path path }
            slots)
      slots

/// Iterate structure fields and accumulate on Stack same as x86-32 convention
let rec private flattenType wordSize baseOffset size path rawType slots =
  match rawType with
  | RawStructure fields ->
    (* Structure field. Recursively accumulate to handle structure type field *)
    fields
    |> List.fold
      (fun slots field ->
        flattenType
          wordSize
          (baseOffset + field.Offset)
          field.Size
          (appendPath path field.Name)
          field.Type
          slots)
      slots
  | other ->
    (* Reached to base type. Store its type divided into WordSize. *)
    addLeafSlots wordSize baseOffset size (evalTypeOfRaw other) path slots

/// Convert RawGT type except structure to GT.
/// This divides the element into WordSize and construct slots.
let private normalElement wordSize (raw: RawGTElement) =
  let slotCount = wordSlotCount wordSize raw.Size
  let typ = evalTypeOfRaw raw.Type

  let slots =
    [ 0 .. slotCount - 1 ]
    |> List.map (fun index ->
      { Index = index
        Size = min wordSize (raw.Size - index * wordSize)
        Type = typ
        Path = "" })

  { Size = raw.Size
    Type = typ
    Kind = NormalElement
    OccupiedSlotCount = slotCount
    Slots = slots }

/// Convert RawGT structure type at parameter to GT.
let private structureArgument wordSize (raw: RawGTElement) =
  let slots =
    flattenType wordSize 0 raw.Size "" raw.Type Map.empty
    |> Map.toList
    |> List.map snd

  { Size = raw.Size
    Type = Value
    Kind = StructureElement
    OccupiedSlotCount = wordSlotCount wordSize raw.Size
    Slots = slots }

let private structureReturn wordSize (raw: RawGTElement) =
  { Size = raw.Size
    Type = Address
    Kind = StructureElement
    OccupiedSlotCount = 1
    Slots =
      [ { Index = 0
          Size = wordSize
          Type = Address
          Path = "Return-Buffer" } ] }

/// Convert RawGT function parameter types to x86-32 specific low-level GT
let private convertArgument wordSize (raw: RawGTElement) : GTElement =
  match raw.Type with
  | RawStructure _ -> structureArgument wordSize raw
  | _ -> normalElement wordSize raw

/// Convert RawGT return value type if it is structure to address
let private convertReturn wordSize (raw: RawGTElement) : GTElement =
  match raw.Type with
  | RawStructure _ -> structureReturn wordSize raw
  | _ -> normalElement wordSize raw

/// Convert RawGT function signature to x86-32 specific low-level GT.
/// x86-32 recives structure parameters through STACK. If the function returns
/// structure, caller constructs memory for structure and pass the address of
/// it to callee as the first argument. The callee modified caller's memory and
/// return given address.
let convert wordSize (raw: RawGTFunction) =
  (* Check given function returns structure or not *)
  let hasStructureReturn =
    raw.Return
    |> List.exists (fun (element: RawGTElement) ->
      match element.Type with
      | RawStructure _ -> true
      | _ -> false)

  let args = raw.Args |> List.map (convertArgument wordSize)

  (*
    If given function returns structure, x86-32 gets the return address as a
    first argument
  *)
  let args =
    if hasStructureReturn then
      let hiddenArg =
        { Size = wordSize
          Type = Address
          Kind = NormalElement
          OccupiedSlotCount = 1
          Slots =
            [ { Index = 0
                Size = wordSize
                Type = Address
                Path = "Hidden-Return-Buffer" } ] }

      hiddenArg :: args
    else
      args

  let result: GTFunction =
    { Function = raw.Function
      Args = args
      Return = raw.Return |> List.map (convertReturn wordSize) }

  result
