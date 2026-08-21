module PointerAnalyzer.PreAnalysis.RegParamDetector

open B2R2
open B2R2.MiddleEnd.DataFlow
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Collect the register given from caller by collecting the minimum identifier
/// with used before definition.
let private incomingRegisters (edges: SSAEdges) =
  edges.Uses.Keys
  |> Seq.filter (fun variable -> not (edges.Defs.ContainsKey variable))
  |> Seq.choose (fun variable ->
    tryRegisterId variable
    |> Option.map (fun registerId -> registerId, variable))
  |> Seq.groupBy fst
  |> Seq.map (fun (registerId, variables) ->
    let variable =
      variables |> Seq.map snd |> Seq.minBy (fun value -> value.Identifier)

    registerId, variable)
  |> Map.ofSeq

/// Detect used-before-defined variables only for predefined regparam registers.
let detect (platform: Platform) (function_: FunctionDFAResult) =
  let incomingRegs = incomingRegisters function_.DFAResult.Edges

  let detectedRegParams =
    platform.RegParams
    |> List.choose (fun registerId ->
      Map.tryFind registerId incomingRegs
      |> Option.map (fun variable -> registerId, variable))

  detectedRegParams
