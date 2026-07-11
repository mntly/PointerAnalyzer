module EvaluateAnalyzer.Evaluator.Evaluator

open EvaluateAnalyzer.Evaluator

/// <summary>
/// Options used for Evaluator.
/// </summary>
/// <remarks>
/// <c>GroundTruthJsonPath</c> is path of ground truth function signature. This
/// file is the result of GroundTruthExtractor.
/// <c>InferredJsonPath</c> is path of the inferred function signature. This
/// file is the result of PointerAnalyzer.
/// </remarks>
type EvalOptions =
  { GroundTruthJsonPath: string
    InferredJsonPath: string }

/// <summary>
/// Store result of evaluator. Json stores the metrics and Log stores log
/// generated during evaluating.
/// </summary>
type EvalOutput = { Json: string; Log: string }

/// Generate new output file name
let evalResultJsonFileName suffix = suffix + "_evalResult.json"

/// Generate new output log file name
let evalResultLogFileName suffix = suffix + "_evalResult.log"

/// Evaluate PointerAnalyzer using given 1 binary with its ground truth
let run options =
  (* Parse GroundTruth Json file *)
  let gt = ParseJSON.loadGroundTruth options.GroundTruthJsonPath

  (* Parse Inferred Result Jsin file *)
  let inferred = ParseJSON.loadInferred options.InferredJsonPath

  (* Evaluate the result *)
  (* 1. Classify the type of result: Correct, MisInferred, Conflict, Fail *)
  let elements, logState = ElementEvaluator.evaluate gt inferred
  (* 2. Measure each metric *)
  let metric = Metric.build elements

  { Json = Metric.toJson metric
    Log = Log.toText logState }
