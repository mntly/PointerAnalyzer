module Checker.Return64Detection.Evaluator.Metric

open System.Text.Json
open Checker.Return64Detection.Evaluator.Types

let private divide numerator denominator =
  if denominator = 0 then
    0.0
  else
    float numerator / float denominator

/// Given classification result, calculate metric
let build
  (gtAll: int)
  (results: FunctionResult list)
  (logState: EvalLogState)
  : EvalMetric =
  (* Count total number of given category (TP|TN|FP|FN) *)
  let countCategory target =
    results
    |> List.sumBy (fun result -> if result.Category = target then 1 else 0)

  let tp = countCategory TruePositive
  let tn = countCategory TrueNegative
  let fp = countCategory FalsePositive
  let fn = countCategory FalseNegative

  let evaluated = tp + tn + fp + fn

  let acc = divide (tp + tn) evaluated
  let precision = divide tp (tp + fp)
  let recall = divide tp (tp + fn)

  let f1 =
    if precision + recall = 0.0 then
      0.0
    else
      2.0 * precision * recall / (precision + recall)

  { Count =
      { GTAll = gtAll
        Evaluated = evaluated
        TP = tp
        TN = tn
        FP = fp
        FN = fn
        InvalidGT = Set.count logState.InvalidGT
        MissingGT = Set.count logState.MissingGT }
    Metric =
      { Accuracy = acc
        Precision = precision
        Recall = recall
        F1 = f1 } }

let toJson metric =
  let options = JsonSerializerOptions (WriteIndented = true)
  JsonSerializer.Serialize (metric, options)
