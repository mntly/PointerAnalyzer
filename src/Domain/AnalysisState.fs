module PointerAnalyzer.AbsDom.AnalysisState

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsMem
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.RegMap
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.AbsDom.TypeState

/// <summary>
/// Analysis state of main-analysis step.
/// </summary>
/// <remarks>
/// <c>RegMap</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.AbsDom.RegMap" />.
/// <c>Memory</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.AbsDom.AbsMem.AbsMem" />.
/// <c>Types</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.AbsDom.TypeState.TypeState" />.
/// <c>PendingReturns</c> tracks the type of return registers after applying
/// callee. The stored register is eliminated when it is used.
/// <c>StackDelta</c> tracks offset of current stack pointer to find out the
/// arguments passed by stack.
/// </remarks>
type AnalysisState =
  { RegMap: RegMap
    Memory: AbsMem
    Types: TypeState
    PendingReturns: Map<RegisterID, TypeId>
    StackDelta: int option }

/// <summary>
/// Updates Analysis State.
/// </summary>
type AnalysisStateModule (platform: Platform, startTypeId: TypeId) =
  let absVal = AbsValDomain.create platform
  let regMap = RegMapDomain.create platform
  let memory = AbsMemDomain.create platform
  let types = TypeStateDomain.create startTypeId

  member _.AbsVal = absVal
  member _.RegMap = regMap
  member _.AbsMem = memory
  member _.TypeState = types

  member _.bot =
    { RegMap = regMap.bot
      Memory = memory.bot
      Types = types.bot
      PendingReturns = Map.empty
      StackDelta = None }

  /// Return new type Id
  member _.freshTypeId state =
    let typeId, typeState = types.fresh state.Types
    typeId, { state with Types = typeState }

  /// Return new type Id only if given SSA variable assigned first
  member _.getOrFreshTypeId variable state =
    match types.tryFind variable state.Types with
    | Some typeId -> typeId, state
    | None ->
      let typeId, typeState = types.fresh state.Types
      let typeState = types.set variable typeId typeState

      let newTypeState =
        if platform.IsTrivialAddress variable then
          types.addAddress typeId typeState
        else if platform.IsTrivialValue variable then
          types.addValue typeId typeState
        else
          typeState

      typeId, { state with Types = newTypeState }

  member _.tryFindTypeId variable state = types.tryFind variable state.Types

  /// Return the Abstract Value of given register
  member _.findRegister variable state = regMap.find variable state.RegMap

  /// Return the Abstract Value of given register
  member _.tryFindRegister variable state = regMap.tryFind variable state.RegMap

  /// Set the abstract value of given register as given abstract value
  member _.setRegister variable value typeId state =
    { state with
        RegMap = regMap.add variable value state.RegMap
        Types = types.set variable typeId state.Types }

  /// Add new type constraint
  member _.addConstraint constraint_ state =
    { state with
        Types = types.addConstraint constraint_ state.Types }

  /// Add new Address type constraint
  member _.addAddress typeId state =
    { state with
        Types = types.addAddress typeId state.Types }

  /// Add new Value type constraint
  member _.addValue typeId state =
    { state with
        Types = types.addValue typeId state.Types }

  /// Add new Same type constraint
  member _.addSame typeIds state =
    (*
      ToDo
        Current it does not use global Type Id for Address, Value.
        If the global Type Id is removed, then change to Normal Same
    *)
    let containAddr = Seq.contains TypeIds.address typeIds
    let containVal = Seq.contains TypeIds.value typeIds

    let newTypes =
      match containAddr, containVal with
      | false, true ->
        (* All become Value *)
        Seq.fold
          (fun acc tid ->
            if tid = TypeIds.value then acc else types.addValue tid acc)
          state.Types
          typeIds
      | true, false ->
        (* All become Address *)
        Seq.fold
          (fun acc tid ->
            if tid = TypeIds.address then
              acc
            else
              types.addAddress tid acc)
          state.Types
          typeIds
      | false, false ->
        (* Normal Same *)
        types.addSame typeIds state.Types
      | true, true ->
        (* All become Conflict: This must error *)
        (* To notify error occur by including trivial types *)
        types.addSame typeIds state.Types

    { state with Types = newTypes }

  /// Add new AddResult(result, left, right) type constraint
  member _.addAddResult result left right state =
    { state with
        Types = types.addAddResult result left right state.Types }

  /// Add new SubResult(result, left, right) type constraint
  member _.addSubResult result left right state =
    { state with
        Types = types.addSubResult result left right state.Types }

  /// Set the type of return register as given type Id
  member _.setPendingReturn retRegId calleeRetTypId state =
    { state with
        PendingReturns = Map.add retRegId calleeRetTypId state.PendingReturns }

  /// Update offset of stack pointer
  member _.adjustStackDelta delta state =
    let stackDelta =
      state.StackDelta |> Option.defaultValue 0 |> (+) delta |> Some

    { state with StackDelta = stackDelta }

  /// If return register is used, remove it from pending return
  member _.consumePendingReturn (variable: Variable) state =
    match variable.Kind with
    | RegVar (_, registerId, _) ->
      match Map.tryFind registerId state.PendingReturns with
      | Some typeId ->
        Some typeId,
        { state with
            PendingReturns = Map.remove registerId state.PendingReturns }
      | None -> None, state
    | _ -> None, state

  /// Load value of given address. If the address is not in AbsMem,
  /// return AbsVal Top
  member _.load memoryVersion address state =
    match absVal.tryGetUInt64 address with
    | None ->
      (* Address is not integer -> Return AbsVal Top *)
      absVal.top, None, state
    | Some address ->
      let location = memory.location memoryVersion address

      match memory.tryFind location state.Memory with
      | Some value ->
        (* Already stored -> Normal Load *)
        value.Value, Some value.TypeId, state
      | None ->
        (* New Location -> Add new Value *)
        let typeId, typeState = types.fresh state.Types
        let newState = { state with Types = typeState }
        let value = { Value = absVal.top; TypeId = typeId }

        absVal.top,
        Some typeId,
        { newState with
            Memory = memory.add location value newState.Memory }

  /// Store value to given address. Only if given address is integer,
  /// the value is tracked
  member _.store prevVersion newVersion address value valueTypeId state =
    match absVal.tryGetUInt64 address with
    | None ->
      (* Address is not integer -> Do not store *)
      valueTypeId, state
    | Some address ->
      (* Address is integer -> Store *)
      let typeId, typeState =
        match valueTypeId with
        | None -> types.fresh state.Types
        | Some id -> id, state.Types

      let updated = memory.updateVersion prevVersion newVersion state.Memory
      let location = memory.location newVersion address

      let valueMem = { Value = value; TypeId = typeId }

      Some typeId,
      { state with
          Memory = memory.add location valueMem updated
          Types = typeState }

  /// Join analysis state
  member _.join left right =
    { RegMap = regMap.join left.RegMap right.RegMap
      Memory = memory.join left.Memory right.Memory
      Types = types.join left.Types right.Types
      PendingReturns =
        right.PendingReturns
        |> Map.fold
          (fun result registerId typeId -> Map.add registerId typeId result)
          left.PendingReturns
      StackDelta =
        match left.StackDelta, right.StackDelta with
        | Some left, Some right when left = right -> Some left
        | Some delta, None
        | None, Some delta -> Some delta
        | _ -> None }

module AnalysisStateDomain =
  /// Create analysis state handler starting with given type id
  let create platform startTypeId =
    AnalysisStateModule (platform, startTypeId)

  /// Create analysis state handler starting with type Id 0
  let createDefault platform = create platform 0
