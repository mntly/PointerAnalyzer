module PointerAnalyzer.Frontend.B2R2Diagnostics

open System
open B2R2

/// <summary>
/// Stage of a B2R2 API call that raised an exception.
/// </summary>
type B2R2AnalysisStage =
  | BinaryLoading
  | CFGRecovery
  | SSALifting
  | DataFlowAnalysis

  member this.ToString =
    match this with
    | BinaryLoading -> "binary loading"
    | CFGRecovery -> "CFG recovery"
    | SSALifting -> "SSA lifting"
    | DataFlowAnalysis -> "data-flow analysis"

/// <summary>
/// Represents the detail of exception cause by B2R2 API.
/// </summary>
type B2R2AnalysisException
  (
    binaryPath: string,
    stage: B2R2AnalysisStage,
    functionAddress: Addr option,
    functionName: string option,
    cause: exn
  ) =
  inherit Exception (cause.Message, cause)

  member _.BinaryPath = binaryPath
  member _.Stage = stage
  member _.FunctionAddress = functionAddress
  member _.FunctionName = functionName
  member _.Cause = cause

  member this.ToString =
    let functionText =
      match this.FunctionAddress, this.FunctionName with
      | Some address, Some name -> sprintf "0x%08x %s" address name
      | Some address, None -> sprintf "0x%08x" address
      | None, Some name -> name
      | None, None -> "<unknown>"

    [ "====== B2R2 Analysis Error ======"
      sprintf "Binary: %s" this.BinaryPath
      sprintf "Stage: %s" this.Stage.ToString
      sprintf "Function: %s" functionText
      sprintf "Exception: %s" (this.Cause.GetType().FullName)
      sprintf "Message: %s" this.Cause.Message
      ""
      "Stack trace:"
      this.Cause.ToString() ]
    |> String.concat "\n"
    |> fun text -> text + "\n"

/// <summary>
/// An instruction which B2R2 represented as UnsupportedInstruction.
/// </summary>
type UnsupportedInstInfo =
  { FunctionAddress: Addr
    FunctionName: string
    ProgramPoint: ProgramPoint
    Instruction: string }

let unsupportedToText binaryPath diagnostics =
  let diagnosticLines diagnostic =
    [ sprintf
        "Function: 0x%08x %s"
        diagnostic.FunctionAddress
        diagnostic.FunctionName
      sprintf
        "ProgramPoint: 0x%08x+%d"
        diagnostic.ProgramPoint.Address
        diagnostic.ProgramPoint.Position
      sprintf "Instruction: %s" diagnostic.Instruction
      "B2R2 SSA: SideEffect UnsupportedInstruction"
      "Action: Analysis continued"
      "" ]

  [ [ "====== B2R2 Unsupported Instructions ======"
      sprintf "Binary: %s" binaryPath
      sprintf "Count: %d" (List.length diagnostics)
      "" ]
    diagnostics |> List.collect diagnosticLines ]
  |> List.concat
  |> String.concat "\n"
  |> fun text -> text + "\n"
