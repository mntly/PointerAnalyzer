module Checker.Return64Detection.Return64Detector

open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.B2R2Diagnostics
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open Checker.Return64Detection.Return64Types

/// <summary>
/// Stores the config that indicates how to execute Return64Detector.
/// </summary>
type DetectOptions =
  { BinaryPath: string
    Range: AnalysisRange
    Heuristic: DetectionHeuristic }

/// Execute Return64Detector with given options
let run options =
  (* Prepare binary to use B2R2 *)
  let binary =
    try
      BinaryLoader.load options.BinaryPath
    with cause ->
      raise (
        B2R2AnalysisException (
          options.BinaryPath,
          BinaryLoading,
          None,
          None,
          cause
        )
      )

  (* Current, Return64Detector only analyzes ELF x86-32 binaries *)
  if binary.Platform.Kind <> ElfX86_32 then
    invalidArg
      (nameof options.BinaryPath)
      "Return64Detector supports only ELF x86-32 binaries."

  (* B2R2 DFA. Use DS of PointerAnalyzer *)
  let program = ProgramDFA.runDFA binary
  (* Both heurisitc analyzes with basic heuristics first *)
  let basic = BasicDetector.run options.Range program

  (* If Heuristic is BasicWithCallerChecker, use the evidence from caller *)
  let functions =
    match options.Heuristic with
    | Basic -> basic
    | BasicWithCallerChecker -> CallerChecker.apply program basic

  (*
    Store not only the result of Return64Detection, but also binary
    information. Binary information will be used to convert RawGT into
    ABI-specific GT.
  *)
  { BinaryPath = binary.Path
    Platform = binary.Platform.Name
    WordSize = binary.Platform.WordSize
    Range = options.Range
    Heuristic = options.Heuristic
    B2R2Diagnostics = program.B2R2Diagnostics
    Functions = functions }
