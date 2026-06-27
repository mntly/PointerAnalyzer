module PointerAnalyzer.AbsDom.AnalysisState

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsMem
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.RegMap
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.AbsDom.TypeState

type AnalysisState =
  { RegMap: RegMap
    Memory: AbsMem
    Types: TypeState
    PendingReturns: Map<RegisterID, TypeId>
    StackDelta: int option }

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

  member _.freshTypeId state =
    let typeId, typeState = types.fresh state.Types
    typeId, { state with Types = typeState }

  member _.getOrFreshTypeId variable state =
    match types.tryFind variable state.Types with
    | Some typeId -> typeId, state
    | None ->
      let trivialTypeId =
        if platform.IsTrivialAddress variable then
          Some TypeIds.address
        elif platform.IsTrivialValue variable then
          Some TypeIds.value
        else
          None

      match trivialTypeId with
      | Some typeId ->
        typeId,
        { state with
            Types = types.set variable typeId state.Types }
      | None ->
        let typeId, typeState = types.fresh state.Types
        typeId, { state with Types = types.set variable typeId typeState }

  member _.tryFindTypeId variable state = types.tryFind variable state.Types

  member _.findRegister variable state = regMap.find variable state.RegMap

  member _.tryFindRegister variable state = regMap.tryFind variable state.RegMap

  member _.setRegister variable value typeId state =
    { state with
        RegMap = regMap.add variable value state.RegMap
        Types = types.set variable typeId state.Types }

  member _.addConstraint constraint_ state =
    { state with
        Types = types.addConstraint constraint_ state.Types }

  member _.addAddress typeId state =
    { state with
        Types = types.addAddress typeId state.Types }

  member _.addValue typeId state =
    { state with
        Types = types.addValue typeId state.Types }

  member _.addSame typeIds state =
    { state with
        Types = types.addSame typeIds state.Types }

  member _.addAddResult result left right state =
    { state with
        Types = types.addAddResult result left right state.Types }

  member _.addSubResult result left right state =
    { state with
        Types = types.addSubResult result left right state.Types }

  member _.setPendingReturn retRegId calleeRetTypId state =
    { state with
        PendingReturns = Map.add retRegId calleeRetTypId state.PendingReturns }

  member _.adjustStackDelta delta state =
    let stackDelta =
      state.StackDelta |> Option.defaultValue 0 |> (+) delta |> Some

    { state with StackDelta = stackDelta }

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

  member _.load memoryVersion address state =
    match absVal.tryGetUInt64 address with
    | None -> absVal.top, None, state
    | Some address ->
      let location = memory.location memoryVersion address

      match memory.tryFind location state.Memory with
      | Some value -> value.Value, Some value.TypeId, state
      | None ->
        // New Location -> Add new Value
        let typeId, typeState = types.fresh state.Types
        let newState = { state with Types = typeState }
        let value = { Value = absVal.top; TypeId = typeId }

        absVal.top,
        Some typeId,
        { newState with
            Memory = memory.add location value newState.Memory }

  member _.store prevVersion newVersion address value valueTypeId state =
    match absVal.tryGetUInt64 address with
    | None -> valueTypeId, state
    | Some address ->
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
  let create platform startTypeId =
    AnalysisStateModule (platform, startTypeId)

  let createDefault platform = create platform 0
