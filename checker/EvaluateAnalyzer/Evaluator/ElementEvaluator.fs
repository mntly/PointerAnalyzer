module EvaluateAnalyzer.Evaluator.ElementEvaluator

open EvaluateAnalyzer.Evaluator.Types

/// Check GT function signature has Unknown type.
/// This kinds of funcition not used for evaluation.
let private hasUnknownGT (gt: GTFunction) =
  let hasUnknown elements =
    elements |> List.exists (fun element -> element.Type = Unknown)

  hasUnknown gt.Args || hasUnknown gt.Return

/// A zero size indicates the need of manual correction.
/// Evaluator does not consider the type with zero size as GT.
let private hasInvalidGTSize (gt: GTFunction) =
  let hasInvalidSize elements =
    elements |> List.exists (fun element -> element.Size <= 0)

  hasInvalidSize gt.Args || hasInvalidSize gt.Return

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

/// From given byteSize, calculate the # of Word Slots
let private wordSlotCount wordSize byteSize =
  (byteSize + wordSize - 1) / wordSize

/// Calculate total # of Word Slots to represent given GT args
let private expectedArgSlots wordSize args =
  args |> List.sumBy (fun arg -> wordSlotCount wordSize arg.Size)

/// Evaluate each parameters in function signature.
/// If inferred parameter was missing, assume its type as Unknown.
let private argResults wordSize fn gtArgs inferredArgs =
  (*
    From given InferArgIdx(slotCursor), merge(join) all inferred types
    corresponding to GT by assigning same size
  *)
  let evaluateArgument (slotCursor, results) (paramIdx, gt: GTElement) =
    (* Transform the byte size of GT into # of word slot *)
    let slotCount = wordSlotCount wordSize gt.Size

    (* Extract all corresponding inferred types and Join *)
    let inferred =
      [ slotCursor .. slotCursor + slotCount - 1 ]
      |> List.map (fun slotIndex ->
        Map.tryFind slotIndex inferredArgs |> Option.defaultValue Unknown)
      |> List.fold joinType Unknown

    (* Construct result DS for calculating metric *)
    let result =
      { Function = fn
        Target = Argument paramIdx
        GT = gt.Type
        Inferred = inferred
        Category = classify gt.Type inferred }

    (* Update next InferArgIdx *)
    slotCursor + slotCount, result :: results

  gtArgs
  |> List.indexed
  |> List.fold evaluateArgument (0, [])
  |> snd
  |> List.rev

/// Evaluate return value in function signature
let private returnResults fn gtReturns inferredReturns =
  gtReturns
  |> List.mapi (fun index (gt: GTElement) ->
    let inferred =
      inferredReturns |> List.tryItem index |> Option.defaultValue Unknown

    { Function = fn
      Target = Return index
      GT = gt.Type
      Inferred = inferred
      Category = classify gt.Type inferred })

/// From evaluation result of each element, log detail
let private addFunctionCategory fn results logState =
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
    if results |> List.forall (fun result -> result.Category = Correct) then
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
  if has Fail then
    { logState with
        Fail = Set.add fn logState.Fail }
  else
    logState

let evaluate
  wordSize
  (gtMap: Map<string, GTFunction>)
  (inferredMap: Map<string, InferredFunction>)
  =
  let evalEachFunc (results, logState) (address, gt) =
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

      results,
      logState
    else
      (* For each GT function signature,find corresponding inferred signature *)
      match Map.tryFind address inferredMap with
      | None ->
        (* Function Detection Error *)
        (* PoitnerAnalyzer do not handle this, but for analysis, logging *)
        results,
        { logState with
            MissedDetect = Set.add gt.Function logState.MissedDetect }
      | Some inferred ->
        let fn = gt.Function

        (*
          Calculate expected Word-Sized Stack Slots to log misinferred the # of
          parameters
        *)
        let expectedSlots = expectedArgSlots wordSize gt.Args
        let expectedIndices = Set.ofList [ 0 .. expectedSlots - 1 ]
        let inferredIndices = inferred.Args |> Map.keys |> Set.ofSeq
        let missingIndices = Set.difference expectedIndices inferredIndices
        let extraIndices = Set.difference inferredIndices expectedIndices
        let inferMoreParams = not (Set.isEmpty extraIndices)

        let logState =
          if inferMoreParams then
            { logState with
                InferMoreParams = Set.add fn logState.InferMoreParams }
          else if not (Set.isEmpty missingIndices) then
            { logState with
                CountMismatch = Set.add fn logState.CountMismatch }
          else
            logState

        let logState =
          if
            (not (List.isEmpty gt.Return))
            && List.length gt.Return <> List.length inferred.Return
          then
            { logState with
                CountMismatch = Set.add fn logState.CountMismatch }
          else
            logState

        (* Log for there exist more than 1 return Word-Sized Slot *)
        (* ToDo: Handle large size return value *)
        let logState =
          if gt.Return |> List.exists (fun ret -> ret.Size > wordSize) then
            { logState with
                LargeReturn = Set.add fn logState.LargeReturn }
          else
            logState

        if inferMoreParams then
          results, logState
        else
          (* Evaluate with parameter/return value-wise type checking *)
          (* If GT has no return value, returnResults naturally evaluates none. *)
          let elementResults =
            argResults wordSize fn gt.Args inferred.Args
            @ returnResults fn gt.Return inferred.Return

          (* Log function detail *)
          elementResults @ results,
          addFunctionCategory fn elementResults logState

  gtMap
  |> Map.toList
  |> List.fold evalEachFunc ([], EvalLogState.empty)
  |> fun (results, logState) -> List.rev results, logState

/// Count all elements in valid GT function signatures
let countValidGTElements (gtMap: Map<string, GTFunction>) =
  gtMap
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.filter (fun gt -> not (hasUnknownGT gt || hasInvalidGTSize gt))
  |> Seq.sumBy (fun gt -> List.length gt.Args + List.length gt.Return)
