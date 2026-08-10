module PointerAnalyzer.Return64Detection.Return64Detector

open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Return64Detection.Return64Types

/// <summary>
/// Stores the config that indicates how to execute Return64Detector.
/// </summary>
type DetectOptions =
  { BinaryPath: string
    Range: AnalysisRange
    Heuristic: DetectionHeuristic }

/// Execute Return64Detector over an existing B2R2 DFA result.
let detect range heuristic (program: ProgramDFAResult) =
  let binary = program.Binary

  (* Both heuristics analyze with the basic heuristic first. *)
  let basic = BasicDetector.run range program

  let functions =
    match heuristic with
    | Basic -> basic
    | BasicWithCallerChecker -> CallerChecker.apply program basic

  { Platform = binary.Platform.Name
    WordSize = binary.Platform.WordSize
    Range = range
    Heuristic = heuristic
    Functions = functions }

/// Execute Return64Detector with given options
let run options =
  (* Prepare binary to use B2R2 *)
  let binary = BinaryLoader.load options.BinaryPath

  (* Current, Return64Detector only analyzes ELF x86-32 binaries *)
  if binary.Platform.Kind <> ElfX86_32 then
    invalidArg
      (nameof options.BinaryPath)
      "Return64Detector supports only ELF x86-32 binaries."

  (* B2R2 DFA. Use the data structures of PointerAnalyzer. *)
  let program = ProgramDFA.runDFA binary
  detect options.Range options.Heuristic program
