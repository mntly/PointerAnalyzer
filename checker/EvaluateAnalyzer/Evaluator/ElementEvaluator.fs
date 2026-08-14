module EvaluateAnalyzer.Evaluator.ElementEvaluator

open EvaluateAnalyzer.Evaluator.Types

/// Check GT function signature has Unknown type.
/// This kinds of funcition not used for evaluation.
let private hasUnknownGT (gt: GTFunction) =
  let hasUnknown (element: GTElement) =
    element.Type = Unknown
    || (element.Slots |> List.exists (fun slot -> slot.Type = Unknown))

  List.exists hasUnknown gt.Args || List.exists hasUnknown gt.Return

/// A zero size indicates the need of manual correction.
/// Evaluator does not consider the type with zero size as GT.
let private hasInvalidGTSize (gt: GTFunction) =
  let hasInvalidSize (element: GTElement) =
    element.Size <= 0
    || element.OccupiedSlotCount <= 0
    || (element.Slots |> List.exists (fun slot -> slot.Size <= 0))

  List.exists hasInvalidSize gt.Args || List.exists hasInvalidSize gt.Return

/// Classify the evaluate result type of given pair of GT and inferred type
let private classify gt inferred =
  match gt, inferred with
  | Address, Address
  | Value, Value -> Correct
  | Address, Value
  | Value, Address -> MisInferred
  | Address, Conflict
  | Value, Conflict -> ConflictResult
  | Address, Unknown
  | Value, Unknown -> Fail
  | _ -> Fail

/// Join Type.
let private joinType left right =
  match left, right with
  | Unknown, other
  | other, Unknown -> other
  | Conflict, _
  | _, Conflict -> Conflict
  | Address, Address -> Address
  | Value, Value -> Value
  | Address, Value
  | Value, Address -> Conflict

/// Compare the normal type parameter. The inferred slot is extracted from
/// slotCursor. `paramIdx` indicates the index of parameter for logging. If
/// normal parameter has more than one slot, it should be merged type before
/// evaluation.
let private normalArgEvalResult
  slotCursor
  fn
  paramIdx
  (gt: GTElement)
  inferredArgs
  =
  let inferredSlots =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.map (fun slot -> Map.tryFind (slotCursor + slot.Index) inferredArgs)

  let sources =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.choose (fun slot ->
      let index = slotCursor + slot.Index
      if Map.containsKey index inferredArgs then Some (ArgumentSource index)
      else None)

  let inferred =
    if inferredSlots |> List.exists Option.isNone then
      Unknown
    else
      inferredSlots |> List.choose id |> List.fold joinType Unknown

  { Function = fn
    Target = Argument paramIdx
    GT = gt.Type
    Inferred = inferred
    Sources = sources
    Category = classify gt.Type inferred }

/// Compare the structure type parameter. The inferred slot is extracted from
/// slotCursor. `paramIdx` indicates the index of parameter for logging. The
/// structure will be evaluated after decomposing its fields into WordSize
/// slots. If at least one slot is inferred, all GT slots are evaluated and a
/// missing inferred slot is treated as Unknown.
let private structureArgEvalResults
  slotCursor
  fn
  paramIdx
  (gt: GTElement)
  inferredArgs
  =
  (* Match all GT slot with its inferred type without removing missing slots. *)
  let matchedSlots =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.map (fun slot ->
      slot, Map.tryFind (slotCursor + slot.Index) inferredArgs)

  (* Count the number of inferred slots *)
  let observedSlots =
    matchedSlots
    |> List.sumBy (fun (_, inferred) -> if Option.isSome inferred then 1 else 0)

  (* Matching GT and inferred type and classify the result *)
  let results =
    if observedSlots = 0 then
      []
    else
      matchedSlots
      |> List.map (fun (slot, inferredOpt) ->
        let inferred = Option.defaultValue Unknown inferredOpt

        { Function = fn
          Target = ArgumentSlot (paramIdx, slot.Index, slot.Path)
          GT = slot.Type
          Inferred = inferred
          Sources =
            if Option.isSome inferredOpt then
              [ ArgumentSource (slotCursor + slot.Index) ]
            else
              []
          Category = classify slot.Type inferred })

  let coverage =
    { Function = fn
      Target = Argument paramIdx
      ExpectedSlots = List.length gt.Slots
      ObservedSlots = observedSlots }

  results, coverage

/// Evaluate each parameters in function signature.
/// If inferred parameter was missing, assume its type as Unknown.
let private argResults fn gtArgs inferredArgs =
  (*
    From given InferArgIdx(slotCursor), merge(join) all inferred types
    corresponding to GT by assigning same size
  *)
  let evaluateArgument
    (slotCursor, results, coverages)
    (paramIdx, gt: GTElement)
    =
    (* Construct result DS for calculating metric *)
    let currentResults, currentCoverages =
      match gt.Kind with
      | NormalElement ->
        [ normalArgEvalResult slotCursor fn paramIdx gt inferredArgs ], []
      | StructureElement ->
        let results, coverage =
          structureArgEvalResults slotCursor fn paramIdx gt inferredArgs

        results, [ coverage ]

    (* Move to next GT element *)
    let nextSlotCursor = slotCursor + gt.OccupiedSlotCount

    let results =
      currentResults |> List.fold (fun acc result -> result :: acc) results

    let coverages =
      currentCoverages
      |> List.fold (fun acc coverage -> coverage :: acc) coverages

    nextSlotCursor, results, coverages

  let _, results, coverages =
    gtArgs |> List.indexed |> List.fold evaluateArgument (0, [], [])

  List.rev results, List.rev coverages

/// Evaluate one normal return element from the consecutive ABI return slots
/// occupied by that element.
let private normalReturnResult
  slotCursor
  fn
  returnIndex
  (gt: GTElement)
  inferredReturns
  =
  let inferredSlots =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.map (fun slot ->
      inferredReturns |> List.tryItem (slotCursor + slot.Index))

  let sources =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.choose (fun slot ->
      let index = slotCursor + slot.Index
      if index < List.length inferredReturns then Some (ReturnSource index)
      else None)

  let inferred =
    if inferredSlots |> List.exists Option.isNone then
      Unknown
    else
      inferredSlots |> List.choose id |> List.fold joinType Unknown

  { Function = fn
    Target = Return returnIndex
    GT = gt.Type
    Inferred = inferred
    Sources = sources
    Category = classify gt.Type inferred }

/// If at least one return slot is inferred, evaluate every GT structure slot
/// and treat each missing inferred slot as Unknown.
let private structureReturnResults
  slotCursor
  fn
  returnIndex
  (gt: GTElement)
  inferredReturns
  =
  (* Map GT slot and inferred slot *)
  let matchedSlots =
    gt.Slots
    |> List.sortBy (fun slot -> slot.Index)
    |> List.map (fun slot ->
      slot, (inferredReturns |> List.tryItem (slotCursor + slot.Index)))

  (* Count the number of inferred slots *)
  let observedSlots =
    matchedSlots
    |> List.sumBy (fun (_, inferred) -> if Option.isSome inferred then 1 else 0)

  (* Matching GT and inferred type and classify the result *)
  let results =
    if observedSlots = 0 then
      []
    else
      matchedSlots
      |> List.map (fun (slot, inferredOpt) ->
        let inferred = Option.defaultValue Unknown inferredOpt

        { Function = fn
          Target = ReturnSlot (returnIndex, slot.Index, slot.Path)
          GT = slot.Type
          Inferred = inferred
          Sources =
            if Option.isSome inferredOpt then
              [ ReturnSource (slotCursor + slot.Index) ]
            else
              []
          Category = classify slot.Type inferred })

  let coverage =
    { Function = fn
      Target = Return returnIndex
      ExpectedSlots = List.length gt.Slots
      ObservedSlots = observedSlots }

  results, coverage

/// Evaluate each return value in function signature.
/// If inferred return value was missing, assume its type as Unknown.
let private returnResults fn gtReturns inferredReturns =
  let evaluateReturn
    (slotCursor, results, coverages)
    (returnIndex, gt: GTElement)
    =
    let currentResults, currentCoverages =
      match gt.Kind with
      | NormalElement ->
        [ normalReturnResult
            slotCursor
            fn
            returnIndex
            gt
            inferredReturns ],
        []
      | StructureElement ->
        let results, coverage =
          structureReturnResults
            slotCursor
            fn
            returnIndex
            gt
            inferredReturns

        results, [ coverage ]

    let nextSlotCursor = slotCursor + gt.OccupiedSlotCount

    let results =
      currentResults |> List.fold (fun acc result -> result :: acc) results

    let coverages =
      currentCoverages
      |> List.fold (fun acc coverage -> coverage :: acc) coverages

    nextSlotCursor, results, coverages

  let _, results, coverages =
    gtReturns |> List.indexed |> List.fold evaluateReturn (0, [], [])

  List.rev results, List.rev coverages

/// From evaluation result of each element, log detail
let private addFunctionCategory fn hasUnobservedStructure results logState =
  (*
    Used for checking the evaluation result of current function signature has
    specific evaluate result type: Correct, MisInferred, ConflictResult, Fail
  *)
  let has category =
    results |> List.exists (fun result -> result.Category = category)

  (*
    Log function as Correct only when all elements in function signature was
    correctly inferred
  *)
  let logState =
    if
      not hasUnobservedStructure
      && not (List.isEmpty results)
      && (results |> List.forall (fun result -> result.Category = Correct))
    then
      { logState with
          Correct = Set.add fn logState.Correct }
    else
      logState

  (*
    Add MisInferred log if at least one element in function signature
    misinferred
  *)
  let logState =
    if has MisInferred then
      { logState with
          MisInferred = Set.add fn logState.MisInferred }
    else
      logState

  (*
    Add ConflictResult log if at least one element in function signature
    inferred as both Address and Value
  *)
  let logState =
    if has ConflictResult then
      { logState with
          Conflict = Set.add fn logState.Conflict }
    else
      logState

  (* Add Fail log if at least one element in function signature fail to infer *)
  if has Fail || hasUnobservedStructure then
    { logState with
        Fail = Set.add fn logState.Fail }
  else
    logState

/// Converge the function signature when its PointerAnalyzer missed at least
/// one field of structure in function signature
let private addStructureCoverage
  (coverages: StructureCoverage list)
  (logState: EvalLogState)
  =
  coverages
  |> List.fold
    (fun (logState: EvalLogState) coverage ->
      if coverage.ObservedSlots <> coverage.ExpectedSlots then
        { logState with
            StructureCoverage = Set.add coverage logState.StructureCoverage }
      else
        logState)
    logState

/// Calculate total number of slots and the parameters. PointerAnalyzer can
/// miss some field of structure, but it must find all parameters.
/// `normIdx` stores the slot idx of parameters with normal type that
/// PointerAnalyzer must infer. `structIdxGroup` stores the idx per structure.
/// This is used to check the PointerAnalyzer can infer at least one field per
/// structure.
let private argumentLayout (gt: GTFunction) =
  let folder (cursor, normIdx, structIdxGroup) (element: GTElement) =
    let normIdx, structIdxGroup =
      match element.Kind with
      | NormalElement ->
        (* Add all slot index if current element is not structure *)
        let normIdx =
          element.Slots
          |> List.fold
            (fun indices slot -> Set.add (cursor + slot.Index) indices)
            normIdx

        normIdx, structIdxGroup
      | StructureElement ->
        (* At least one inferred slot must exist for each structure argument *)
        let structIdxGroup' =
          element.Slots
          |> List.map (fun slot -> cursor + slot.Index)
          |> Set.ofList

        normIdx, structIdxGroup' :: structIdxGroup

    cursor + element.OccupiedSlotCount, normIdx, structIdxGroup

  let occupiedSlots, normIdx, structIdxGroup =
    List.fold folder (0, Set.empty, []) gt.Args

  occupiedSlots, normIdx, structIdxGroup

let evaluate
  (gtMap: Map<string, GTFunction>)
  (inferredMap: Map<string, InferredFunction>)
  =
  let evalEachFunc (results, coverages, logState) (address, gt) =
    let unknownGT = hasUnknownGT gt
    let invalidGTSize = hasInvalidGTSize gt

    if unknownGT || invalidGTSize then
      (*
        If Unknown Type exists in GT Signature or GT type is invalid,
        do not use to evaluate. Add corresponding log
      *)
      let logState =
        if unknownGT then
          { logState with
              GTUnknown = Set.add gt.Function logState.GTUnknown }
        else
          logState

      let logState =
        if invalidGTSize then
          { logState with
              InvalidGTSize = Set.add gt.Function logState.InvalidGTSize }
        else
          logState

      results, coverages, logState
    else
      (* For each GT function signature,find corresponding inferred signature *)
      match Map.tryFind address inferredMap with
      | None ->
        (* Function Detection Error *)
        (* PoitnerAnalyzer do not handle this, but for analysis, logging *)
        let logState =
          { logState with
              MissedDetect = Set.add gt.Function logState.MissedDetect }

        results, coverages, logState
      | Some inferred ->
        let fn = gt.Function

        (* Count all slots, and extract all parameter slot idxs *)
        (* Struct parameter will represented as slot idx set of each field *)
        let occupiedSlots, normIdxSet, structIdxGroup = argumentLayout gt

        (* Construct slot idx list *)
        let occupiedIndices =
          if occupiedSlots = 0 then
            Set.empty
          else
            Set.ofList [ 0 .. occupiedSlots - 1 ]

        let inferredIndices = inferred.Args |> Map.keys |> Set.ofSeq

        (* Find out the missed parameters *)
        let missingNormal = Set.difference normIdxSet inferredIndices

        let missingStructure =
          structIdxGroup
          |> List.exists (fun structSlots ->
            Set.intersect structSlots inferredIndices |> Set.isEmpty)

        (* Find out how many slots are more inferred *)
        let extraIndices = Set.difference inferredIndices occupiedIndices
        let inferMoreParams = not (Set.isEmpty extraIndices)

        let logState =
          if inferMoreParams then
            { logState with
                InferMoreParams = Set.add fn logState.InferMoreParams }
          elif not (Set.isEmpty missingNormal) || missingStructure then
            { logState with
                CountMismatch = Set.add fn logState.CountMismatch }
          else
            logState

        (* Log returns that exceed the ABI slots published by the analyzer. *)
        let requiredReturnSlots =
          gt.Return |> List.sumBy (fun ret -> ret.OccupiedSlotCount)

        let hasLargeReturn =
          requiredReturnSlots > List.length inferred.Return

        let logState =
          if hasLargeReturn then
            { logState with
                LargeReturn = Set.add fn logState.LargeReturn }
          else
            logState

        if inferMoreParams then
          (* Evaluate function only when the inferred params are less than GT *)
          results, coverages, logState
        else
          (* Evaluate with parameter/return value-wise type checking *)
          (* If GT has no return value,returnResults naturally evaluates none *)
          let argElementResults, argCoverages =
            argResults fn gt.Args inferred.Args

          let returnElementResults, returnCoverages =
            returnResults fn gt.Return inferred.Return

          let elementResults = argElementResults @ returnElementResults
          let structureCoverages = argCoverages @ returnCoverages

          (*
            If at least one field of structure parameter is found, then assume
            parameter is found. If not, parameter is unknown
          *)
          let hasUnobservedStructure =
            structureCoverages
            |> List.exists (fun coverage -> coverage.ObservedSlots = 0)

          (* Log function detail *)
          let logState =
            logState
            |> addFunctionCategory fn hasUnobservedStructure elementResults
            |> addStructureCoverage structureCoverages

          elementResults @ results, structureCoverages @ coverages, logState

  gtMap
  |> Map.toList
  |> List.fold evalEachFunc ([], [], EvalLogState.empty)
  |> fun (results, coverages, logState) ->
      List.rev results, List.rev coverages, logState

/// Count all elements in valid GT function signatures
let countValidGTElements (gtMap: Map<string, GTFunction>) =
  let countElement (element: GTElement) =
    match element.Kind with
    | NormalElement -> 1
    | StructureElement -> List.length element.Slots

  gtMap
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.filter (fun gt -> not (hasUnknownGT gt || hasInvalidGTSize gt))
  |> Seq.sumBy (fun gt ->
    List.sumBy countElement gt.Args + List.sumBy countElement gt.Return)
