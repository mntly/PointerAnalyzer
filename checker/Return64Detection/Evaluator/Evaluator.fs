module Checker.Return64Detection.Evaluator.Evaluator

open Checker.Return64Detection
open Checker.Return64Detection.Return64Types

type EvalOptions =
  { BinaryPath: string
    GroundTruthPath: string
    Range: AnalysisRange
    Heuristic: DetectionHeuristic }

type EvalOutput =
  { Detector: string
    Json: string
    Log: string }

let detectorResultFileName suffix = suffix + "_Return64Result"

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

  (* Load GT Json and extract GT return size *)
  let groundTruth = GroundTruthParser.load options.GroundTruthPath

  let gtAll = Map.count groundTruth

  (* Evaluate Return64Detector *)
  let results, logState =
    ElementEvaluator.evaluate groundTruth detection.Functions

  (* Calculate Metrics *)
  let metric = Metric.build gtAll results logState

  (* Transform into string (log file) *)
  { Detector = Return64Formatter.toText detection
    Json = Metric.toJson metric
    Log = Log.toText logState }
