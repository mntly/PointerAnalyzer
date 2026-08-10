module PointerAnalyzer.Frontend.AnalysisProgramBuilder

open B2R2
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Return64Detection.Return64Detector
open PointerAnalyzer.Return64Detection.Return64Types
open PointerAnalyzer.Frontend.ProgramDFA

type SSAMode =
  | OriginalSSA
  | Return64AwareSSA

  member this.Name =
    match this with
    | OriginalSSA -> "Original"
    | Return64AwareSSA -> "Return64Aware"

type AnalysisProgram =
  { Program: ProgramDFAResult
    Return64Detection: DetectionResult
    Return64Functions: Set<Addr>
    SSAMode: SSAMode }

let private detectedReturn64Functions (detection: DetectionResult) =
  detection.Functions
  |> Map.toSeq
  |> Seq.choose (fun (address, function_) ->
    if function_.Status = Return64 then Some address else None)
  |> Set.ofSeq

/// Recover once, detect two-slot returns on baseline SSA, and optionally
/// re-lift with additional call-output definitions.
let build mode range heuristic binary =
  let recovered = ProgramDFA.recover binary
  let baseline = ProgramDFA.build recovered
  let detection = detect range heuristic baseline
  let return64Functions = detectedReturn64Functions detection

  let program =
    match mode with
    | OriginalSSA -> baseline
    | Return64AwareSSA ->
      let callback =
        ReturnRegisterSSAModifier.create
          binary.Platform
          binary.Handle
          return64Functions

      let lifter = SSALifterFactory.Create (binary.Handle, callback)
      ProgramDFA.buildWithLifter lifter recovered

  { Program = program
    Return64Detection = detection
    Return64Functions = return64Functions
    SSAMode = mode }
