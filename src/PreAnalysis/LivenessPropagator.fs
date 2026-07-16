module PointerAnalyzer.PreAnalysis.LivenessPropagator

open B2R2.BinIR.SSA
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check Def-Use chain and backpropagate the liveness starting from sink live
/// registers.
let propagate (cfg: SSACFG) sinks =
  let edges = SSAEdges cfg

  let rec loop live =
    let next =
      live
      |> Seq.fold
        (fun acc variable ->
          match edges.Defs.TryGetValue variable with
          | true, definition ->
            (*
              The operands used for defining live register are also live SSA
              variables
            *)
            Set.union acc (usedVariablesInStmt definition)
          | false, _ -> acc)
        live

    (* Iterate until fixed-point *)
    if next = live then next else loop next

  loop sinks
