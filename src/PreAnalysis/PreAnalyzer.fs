module PointerAnalyzer.PreAnalysis.PreAnalyzer

open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.PreAnalysis.PreAnalysisTypes
open PointerAnalyzer.PreAnalysis.Sink

/// Extract live SSA variables of one function.
let private analyzeFunction platform functions (function_: FunctionDFAResult) =
  let cfg = function_.CFG

  (* Collect Sink Live SSA Variables. *)
  let sinks = SinkCollector.collect platform functions function_

  (* Backpropagate liveness from sink registers *)
  let propagatedLive = LivenessPropagator.propagate cfg sinks

  (*
    Extract SSA variables handled as default live SSA variables.
    Current, the StackVar and SSA variables with trivial type is handled as
    default SSA variables.
  *)
  let defaultLive = DefaultLiveCollector.collect platform cfg

  (* Merge all live variables to construct live variables of given function *)
  let liveVariables = Set.union propagatedLive defaultLive

  { LiveVariables = liveVariables }

/// Extract live SSA variables of each function.
/// This utilzes the DFA result.
let analyze (program: ProgramDFAResult) =
  let platform = program.Binary.Platform

  let functions =
    program.Functions
    |> Map.map (fun _ function_ ->
      { FunctionDFA = function_
        PreAnalysis = analyzeFunction platform program.Functions function_ })

  { Binary = program.Binary
    Functions = functions
    VisitOrder = program.VisitOrder }
