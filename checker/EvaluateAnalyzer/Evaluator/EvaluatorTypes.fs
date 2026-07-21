module EvaluateAnalyzer.Evaluator.Types

/// <summary>
/// Represent the type of the PointerAnalyzer consider.
/// </summary>
type EvalType =
  | Address
  | Value
  | Unknown
  | Conflict

type EvalTarget =
  | Argument of int
  | Return of int

/// <summary>
/// Represent the result of evaluation.
/// This evaluation is only valid when GT type is not Unknown.
/// </summary>
/// <remarks>
/// <c>Correct</c> indicates that GT type and inferred type are same.
/// <c>MisInferred</c> indicates that GT type and inferred type are different
/// when inferred sucess(not Unknown).
/// <c>ConflictResult</c> indicates that PointerAnalyzer inferred as both
/// Address and Value type.
/// <c>Fail</c> indicates that PointerAnalyzer can not infer to one concreate
/// type, i.e PointerAnalyzer inferred as Unknown type.
/// </remarks>
type EvalCategory =
  | Correct
  | MisInferred
  | ConflictResult
  | Fail

/// <summary>
/// Represent key for identify each function.
/// </summary>
type FunctionKey = { Address: string; Name: string }

/// <summary>
/// Ground-truth type and its source-level size in bytes.
/// </summary>
type GTElement =
  { Size: int
    Type: EvalType }

/// <summary>
/// Information passed from PointerAnalyzer used for Evaluation.
/// </summary>
type AnalysisConfig = { WordSize: int }

/// <summary>
/// Represent per-function function signature.
/// </summary>
type GTFunction =
  { Function: FunctionKey
    Args: GTElement list
    Return: GTElement list }

/// <summary>
/// Represent per-function inferred function signature.
/// </summary>
type InferredFunction =
  { Function: FunctionKey
    Args: Map<int, EvalType>
    Return: EvalType list }

/// <summary>
/// Represent per-variable evaluation result.
/// </summary>
type ElementResult =
  { Function: FunctionKey
    Target: EvalTarget
    GT: EvalType
    Inferred: EvalType
    Category: EvalCategory }

/// <summary>
/// Used for tracking the detail of evaluation.
/// </summary>
type EvalLogState =
  { GTUnknown: Set<FunctionKey>
    InvalidGTSize: Set<FunctionKey>
    LargeReturn: Set<FunctionKey>
    MissedDetect: Set<FunctionKey>
    CountMismatch: Set<FunctionKey>
    Correct: Set<FunctionKey>
    MisInferred: Set<FunctionKey>
    Conflict: Set<FunctionKey>
    Fail: Set<FunctionKey>
    InferMoreParams: Set<FunctionKey> }

module EvalLogState =
  let empty =
    { GTUnknown = Set.empty
      InvalidGTSize = Set.empty
      LargeReturn = Set.empty
      MissedDetect = Set.empty
      CountMismatch = Set.empty
      Correct = Set.empty
      MisInferred = Set.empty
      Conflict = Set.empty
      Fail = Set.empty
      InferMoreParams = Set.empty }
