module Checker.Return64Detection.Evaluator.Evaluator

open PointerAnalyzer.Return64Detection
open PointerAnalyzer.Return64Detection.Return64Types

module Return64Formatter = Checker.Return64Detection.Return64Formatter

type EvalOptions =
  { BinaryPath: string
    GroundTruthPath: string
    Range: AnalysisRange
    Heuristic: DetectionHeuristic }

type EvalOutput =
  { Detector: string
    ConvertedGroundTruth: string
    Json: string
    Log: string }

let detectorResultFileName suffix = suffix + "_Return64Result"

let convertedGroundTruthFileName suffix = suffix + "_Return64ConvertedGT.json"

let evalResultJsonFileName suffix = suffix + "_Return64EvalResult.json"

let evalResultLogFileName suffix = suffix + "_Return64EvalResult.log"

/// Execute Return64Detector with given options and evaluate it based on given
/// GT file.
let run options =
  let detectOptions: Return64Detector.DetectOptions =
    { BinaryPath = options.BinaryPath
      Range = options.Range
      Heuristic = options.Heuristic }

  (* Execute Return64Detector *)
  let detection = Return64Detector.run detectOptions

  (* Construct ABI information from the binary used by Return64Detector. *)
  (* This configuration is used to convert RawGT into ABI-specific GT *)
  let config: EvaluateAnalyzer.Evaluator.Types.AnalysisConfig =
    { Platform = detection.Platform
      WordSize = detection.WordSize }

  (* Parse source-level GT and convert it to the target ABI representation. *)
  let rawGroundTruth =
    EvaluateAnalyzer.Evaluator.ParseJSON.loadGroundTruth options.GroundTruthPath

  let convertedGroundTruth =
    EvaluateAnalyzer.Evaluator.GroundTruthConverter.GroundTruthConverter.convert
      config
      rawGroundTruth

  (* Extract Return32/Return64 expectations from ABI-converted GT. *)
  let groundTruth =
    GroundTruthParser.fromConverted config.WordSize convertedGroundTruth

  let gtAll = Map.count groundTruth

  (* Evaluate Return64Detector *)
  let results, logState =
    ElementEvaluator.evaluate groundTruth detection.Functions

  (* Calculate Metrics *)
  let metric = Metric.build gtAll results logState

  (* Transform into string (log file) *)
  { Detector = Return64Formatter.toText detection
    ConvertedGroundTruth =
      EvaluateAnalyzer.Evaluator.GroundTruthConverter.GroundTruthConverter.toJson
        config
        convertedGroundTruth
    Json = Metric.toJson metric
    Log = Log.toText logState }
