module PointerAnalyzer.PreAnalysis.Sink.LeafSinkCollector

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check given basic block is leaf node of given CFG.
let private isLeafBlock (cfg: SSACFG) (block: IVertex<SSABasicBlock>) =
  cfg.GetSuccs block |> Array.isEmpty

/// Update live register by syntactically checking given statement
let private collectLiveRegister (liveRegs: Map<RegisterID, Variable>) stmt =
  (* Extract regVar if it given stmt defines it  *)
  match definedVariable stmt with
  | Some variable ->
    match tryRegisterId variable with
    | Some registerId -> Map.add registerId variable liveRegs
    | None -> liveRegs
  | None -> liveRegs

/// Collect live RegVar from each leaf block.
let collect (cfg: SSACFG) =
  (* Extract live registers of given one block *)
  let extractLiveRegs (block: IVertex<SSABasicBlock>) =
    block.VData.Internals.Statements
    |> Seq.map snd
    |> Seq.fold collectLiveRegister Map.empty
    |> Map.values

  cfg.Vertices
  |> Seq.filter (isLeafBlock cfg)
  |> Seq.collect extractLiveRegs
  |> Set.ofSeq
