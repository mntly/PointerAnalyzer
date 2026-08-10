module PointerAnalyzer.Return64Detection.Analysis.RangeSelector

open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Return64Detection.Return64Types

/// Extract the address of last statement of given basic block
let private terminalAddress (block: IVertex<SSABasicBlock>) =
  block.VData.Internals.Statements
  |> Array.tryLast
  |> Option.map (fun (programPoint, _) -> programPoint.Address)

/// Extract return leaf blocks.
/// The constraint of return leaf block
/// 1. Real Node: Not FakeNode(FunctionAbstraction)
/// 2. Leaf Node: No successed blocks
/// 3. Contain `ret` instruction
let normalReturnBlocks (function_: FunctionDFAResult) =
  function_.CFG.Vertices
  |> Array.filter (fun block ->
    not block.VData.Internals.IsAbstract
    && function_.CFG.GetSuccs block |> Array.isEmpty
    && terminalAddress block
       |> Option.exists (fun address ->
         Set.contains address function_.RetAddresses))

/// Extract every CFG block ID in given CFG
let private allBlockIds (cfg: SSACFG) =
  cfg.Vertices |> Seq.map (fun block -> block.ID) |> Set.ofSeq

/// Select basic blocks to analyze based on given AnalysisRange
let select analysisRange (function_: FunctionDFAResult) =
  let cfg = function_.CFG
  let allBlocks = allBlockIds cfg

  (* Extract target blocks per return leaf nodes *)
  (* EntireFunction: A -> B -> C / A -> B -> D => C: {A, B, C}, D: {A, B, D} *)
  (* LeafAndDirectPredecessors: A -> B -> C / A -> B -> D => C:{B,C},D:{B,D} *)
  normalReturnBlocks function_
  |> Array.map (fun leaf ->
    let blockIds =
      match analysisRange with
      | EntireFunction -> allBlocks
      | LeafAndDirectPredecessors ->
        cfg.GetPreds leaf
        |> Seq.map (fun block -> block.ID)
        |> Set.ofSeq
        |> Set.add leaf.ID

    { LeafId = leaf.ID
      BlockIds = blockIds })
  |> Array.toList
