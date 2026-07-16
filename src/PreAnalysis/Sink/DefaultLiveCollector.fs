module PointerAnalyzer.PreAnalysis.Sink.DefaultLiveCollector

open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check whether given variable should be handled as live by default.
let private isDefaultLive platform variable =
  isStackVariable variable
  || platform.IsTrivialAddress variable
  || platform.IsTrivialValue variable

/// Collect SSA variables are live as default.
let collect platform (cfg: SSACFG) =
  cfg.Vertices
  |> Seq.collect (fun block ->
    block.VData.Internals.Statements
    |> Seq.collect (fun (_, stmt) -> variablesInStmt stmt))
  |> Seq.filter (isDefaultLive platform)
  |> Set.ofSeq
