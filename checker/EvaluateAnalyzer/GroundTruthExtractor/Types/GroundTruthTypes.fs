module EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// Ground-truth type category used by PointerAnalyzer evaluation.
type GTType =
  | Address
  | Value
  | Unknown

module GTType =
  let toString =
    function
    | Address -> "Address"
    | Value -> "Value"
    | Unknown -> "Unknown"

/// Function signature stored in the ground-truth DB.
type FunctionGroundTruth =
  { Name: string
    Args: string list
    Return: string list }

/// Ground-truth DB keyed by normalized function address.
type GroundTruthDb = Map<string, FunctionGroundTruth>
