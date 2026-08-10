module Checker.Return64Detection.Evaluator.ElementEvaluator

open B2R2
open PointerAnalyzer.Return64Detection.Return64Types
open Checker.Return64Detection.Evaluator.Types

/// Compare GT and inferred result and construct one of TP|TN|FP|FN
let private category expectation detection =
  (* Check Return64Detector predicts function returns 64 bit value or not *)
  let predicts64 = detection.Status = DetectionStatus.Return64

  match expectation, predicts64 with
  | GTExpectation.Return64, true -> TruePositive
  | GTExpectation.Return32, false -> TrueNegative
  | GTExpectation.Return32, true -> FalsePositive
  | GTExpectation.Return64, false -> FalseNegative
  | InvalidReturn _, _ ->
    invalidArg (nameof expectation) "invalid GT cannot be evaluated"

/// According to comparision result, log specific result
let private addCategory
  (function_: FunctionKey)
  category
  (logState: EvalLogState)
  =
  match category with
  | TruePositive ->
    { logState with
        TruePositive = Set.add function_ logState.TruePositive }
  | TrueNegative ->
    { logState with
        TrueNegative = Set.add function_ logState.TrueNegative }
  | FalsePositive ->
    { logState with
        FalsePositive = Set.add function_ logState.FalsePositive }
  | FalseNegative ->
    { logState with
        FalseNegative = Set.add function_ logState.FalseNegative }

/// Compare GT and inferred, and construct TP|TN|FP|FN result
let evaluate
  (groundTruth: Map<Addr, GTFunction>)
  (detections: Map<Addr, FunctionDetection>)
  : FunctionResult list * EvalLogState =
  (* Store Invalid GT return value *)
  let initialLog: EvalLogState =
    groundTruth
    |> Map.values
    |> Seq.fold
      (fun (logState: EvalLogState) gt ->
        match gt.Expectation with
        | InvalidReturn reason ->
          { logState with
              InvalidGT = Set.add (gt.Function, reason) logState.InvalidGT }
        | Return32
        | Return64 -> logState)
      EvalLogState.empty

  (* Evaluate the function with given address by comparing correponding GT *)
  let folder
    (results: FunctionResult list, logState: EvalLogState)
    address
    (detection: FunctionDetection)
    =
    (* Extract GT type of given function *)
    match Map.tryFind address groundTruth with
    | None ->
      (* Current function does not exist in GT Json => Log as MissingGt *)
      let function_ =
        { Address = detection.Address
          Name = detection.Name }

      results,
      { logState with
          MissingGT = Set.add function_ logState.MissingGT }
    | Some gt ->
      match gt.Expectation with
      | InvalidReturn _ ->
        (* Do not evaluate with Invalid GT *)
        results, logState
      | expectation ->
        (* Only when return value is valid, process evaluation *)
        let resultCategory = category expectation detection

        { Function = gt.Function
          Category = resultCategory }
        :: results,
        addCategory gt.Function resultCategory logState

  let results, logState = Map.fold folder ([], initialLog) detections

  List.rev results, logState
