module EvaluateAnalyzer.Evaluator.Evaluator

open System.IO
open EvaluateAnalyzer.Evaluator

/// <summary>
/// Options used for Evaluator.
/// </summary>
/// <remarks>
/// <c>GroundTruthJsonPath</c> is path of ground truth function signature. This
/// file is the result of GroundTruthExtractor.
/// <c>InferredJsonPath</c> is path of the inferred function signature. This
/// file is the result of PointerAnalyzer. The evaluator loads
/// `analysisConfig.json` from the same directory.
/// <c>FalsePositiveDebug</c> indicates whether the evaluator should print out
/// constraint propagation history of FP cases.
/// </remarks>
type EvalOptions =
  { GroundTruthJsonPath: string
    InferredJsonPath: string
    FalsePositiveDebug: bool }

/// <summary>
/// Store result of evaluator. Json stores the metrics and Log stores log
/// generated during evaluating.
/// </summary>
type EvalOutput =
  { ConvertedGroundTruth: string
    Json: string
    Log: string
    FalsePositiveLog: string option }

/// Generate new output file name
let evalResultJsonFileName suffix = suffix + "_evalResult.json"

/// Generate new output log file name
let evalResultLogFileName suffix = suffix + "_evalResult.log"

/// Generate converted ground-truth output file name.
let convertedGroundTruthFileName suffix = suffix + "_ConvertedGT.json"

/// Generate type constraint propagation history of FP cases
let falsePositiveLogFileName suffix = suffix + "_FPReason.log"

/// Get path of analysisConfig file.
/// PointerAnalyzer generates config file at the same path with result file.
let private analysisConfigPath inferredJsonPath =
  let fullPath = Path.GetFullPath inferredJsonPath
  let directory = Path.GetDirectoryName fullPath
  Path.Combine (directory, "analysisConfig.json")

/// Construct typeProvenance.json based on inferred json path
let private provenancePath inferredJsonPath =
  let fullPath = Path.GetFullPath inferredJsonPath
  let directory = Path.GetDirectoryName fullPath
  Path.Combine (directory, "typeProvenance.json")

/// Evaluate PointerAnalyzer using given 1 binary with its ground truth
let run options =
  (* Parse RawGroundTruth Json file *)
  let rawGT = ParseJSON.loadGroundTruth options.GroundTruthJsonPath

  (* Parse Inferred Result Jsin file *)
  let inferred = ParseJSON.loadInferred options.InferredJsonPath

  (* Extract ABI information to construct low-level GT function signature *)
  let config =
    options.InferredJsonPath
    |> analysisConfigPath
    |> ParseJSON.loadAnalysisConfig

  (* Convert RawGT into ABI-specific low-level GT *)
  let gt = GroundTruthConverter.GroundTruthConverter.convert config rawGT

  (* Evaluate the result *)
  (* 1. Classify the type of result: Correct, MisInferred, Conflict, Fail *)
  let elements, structureCoverages, logState =
    ElementEvaluator.evaluate gt inferred

  (* 2. Measure each metric *)
  let gtAll = ElementEvaluator.countValidGTElements gt
  let metric = Metric.build gtAll elements structureCoverages

  let convertedGroundTruth =
    GroundTruthConverter.GroundTruthConverter.toJson config gt

  let falsePositiveLog =
    if options.FalsePositiveDebug then
      options.InferredJsonPath
      |> provenancePath
      |> ParseJSON.loadProvenance
      |> fun provenance -> FalsePositiveDebug.toText provenance elements
      |> Some
    else
      None

  { ConvertedGroundTruth = convertedGroundTruth
    Json = Metric.toJson metric
    Log = Log.toText logState
    FalsePositiveLog = falsePositiveLog }
