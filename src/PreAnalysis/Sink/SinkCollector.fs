module PointerAnalyzer.PreAnalysis.Sink.SinkCollector

open B2R2.MiddleEnd.DataFlow
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes

/// Collect sink SSA variables from leaf blocks and callsites.
let collect
  platform
  (functions: Map<B2R2.Addr, FunctionDFAResult>)
  (function_: FunctionDFAResult)
  =
  let cfg = function_.CFG

  (* Live SSA Variables at leaf nodes *)
  let leafLive = LeafSinkCollector.collect cfg

  (* SSA Variables used for function arguments also live *)
  (* Jump target is also marked as live *)
  let cfLive = CallSiteSinkCollector.collect platform functions function_

  (* Other live SSA variabes in Use Edge *)
  let edges = SSAEdges cfg
  let usedSet = edges.Uses.Keys |> Set.ofSeq

  Set.union leafLive cfLive |> Set.union usedSet
