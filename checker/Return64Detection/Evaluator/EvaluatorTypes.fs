module Checker.Return64Detection.Evaluator.Types

open B2R2

/// <summary>
/// Represent the identity of each function.
/// </summary>
type FunctionKey = { Address: Addr; Name: string }

/// <summary>
/// Represent the GT return value.
/// </summary>
/// <remarks>
/// <c>Return32</c> indicates corresponding function returns less or equal to 32
/// bit value.
/// <c>Return64</c> indicates corresponding function returns 64 bit value.
/// <c>InvalidReturn</c> indicates that the size of GT return value is invalid
/// such as 0 with non-void value, etc. The string indicates the reason why
/// corresponding function classified to InvalidReturn..
/// </remarks>
type GTExpectation =
  | Return32
  | Return64
  | InvalidReturn of string

/// <summary>
/// Represent the GT return value size of each function.
/// </summary>
type GTFunction =
  { Function: FunctionKey
    Expectation: GTExpectation }

/// <summary>
/// Represent the kind of evaluating result. The evaluation is done without
/// InvalidReturn GT.
/// </summary>
type EvalCategory =
  | TruePositive
  | TrueNegative
  | FalsePositive
  | FalseNegative

/// <summary>
/// Represent the evaluating result of each function.
/// </summary>
type FunctionResult =
  { Function: FunctionKey
    Category: EvalCategory }

/// <summary>
/// Tracks entire evaluation result.
/// </summary>
type EvalLogState =
  { TruePositive: Set<FunctionKey>
    TrueNegative: Set<FunctionKey>
    FalsePositive: Set<FunctionKey>
    FalseNegative: Set<FunctionKey>
    InvalidGT: Set<FunctionKey * string>
    MissingGT: Set<FunctionKey> }

module EvalLogState =
  let empty =
    { TruePositive = Set.empty
      TrueNegative = Set.empty
      FalsePositive = Set.empty
      FalseNegative = Set.empty
      InvalidGT = Set.empty
      MissingGT = Set.empty }

/// <summary>
/// Records evaluation metric.
/// </summary>
/// <remarks>
/// <c>GTAll</c> indicates the number of functions in GT Json file.
/// <c>Evaluated</c> indicates the number of functions who have valid return
/// value in GT Json file.
/// <c>TP</c> indicates the number of True Positives.
/// <c>TN</c> indicates the number of True Negatives.
/// <c>FP</c> indicates the number of False Positives.
/// <c>FN</c> indicates the number of False Negatives.
/// <c>InvalidGt</c> indicates the number of functions who have invalid return
/// value in GT Json file.
/// <c>MissingGt</c> indicates the number of functions that are not in GT Json.
/// </remarks>
type CountResult =
  { GTAll: int
    Evaluated: int
    TP: int
    TN: int
    FP: int
    FN: int
    InvalidGT: int
    MissingGT: int }

/// <summary>
/// Records evaluation result metric.
/// </summary>
type MetricResult =
  { Accuracy: float
    Precision: float
    Recall: float
    F1: float }

/// <summary>
/// Represents total metrics to store as Json.
/// </summary>
type EvalMetric =
  { Count: CountResult
    Metric: MetricResult }
