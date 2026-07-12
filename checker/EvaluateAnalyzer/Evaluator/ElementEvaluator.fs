module EvaluateAnalyzer.Evaluator.ElementEvaluator

open EvaluateAnalyzer.Evaluator.Types

/// Check GT function signature has Unknown type.
/// This kinds of funcition not used for evaluation.
let private hasUnknownGT (gt: GTFunction) =
  List.exists ((=) Unknown) gt.Args || List.exists ((=) Unknown) gt.Return

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

/// Evaluate each parameters in function signature.
/// If inferred parameter was missing, assume its type as Unknown.
let private argResults fn gtArgs inferredArgs =
  gtArgs
  |> List.mapi (fun paramIdx gt ->
    let inferred =
      Map.tryFind paramIdx inferredArgs |> Option.defaultValue Unknown

    { Function = fn
      Target = Argument paramIdx
      GT = gt
      Inferred = inferred
      Category = classify gt inferred })

/// Evaluate return value in function signature
let private returnResults fn gtReturns inferredReturns =
  gtReturns
  |> List.mapi (fun index gt ->
    let inferred =
      inferredReturns |> List.tryItem index |> Option.defaultValue Unknown

    { Function = fn
      Target = Return index
      GT = gt
      Inferred = inferred
      Category = classify gt inferred })

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
  (gtMap: Map<string, GTFunction>)
  (inferredMap: Map<string, InferredFunction>)
  =
  let evalEachFunc (results, logState) (address, gt) =
    if hasUnknownGT gt then
      (* If Unknown Type exists in GT Signature, do not use to evaluate *)
      (* Add GTUnknown Log *)
      results,
      { logState with
          GTUnknown = Set.add gt.Function logState.GTUnknown }
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

        let logState =
          if List.length gt.Args < Map.count inferred.Args then
            { logState with
                InferMoreParams = Set.add fn logState.InferMoreParams }
          else if List.length gt.Args <> Map.count inferred.Args then
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

        if List.length gt.Args < Map.count inferred.Args then
          results, logState
        else
          (* Evaluate with parameter/return value-wise type checking *)
          (* If GT has no return value, returnResults naturally evaluates none. *)
          let elementResults =
            argResults fn gt.Args inferred.Args
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
  |> Seq.filter (hasUnknownGT >> not)
  |> Seq.sumBy (fun gt -> List.length gt.Args + List.length gt.Return)
