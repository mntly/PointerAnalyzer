module PointerAnalyzer.Return64Detection.Analysis.StatementIndex

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph

/// <summary>
/// Represents single statement with its ProgramPoint and the basic block
/// contains it.
/// </summary>
type StatementEntry =
  { BlockId: VertexID
    ProgramPoint: ProgramPoint
    Statement: Stmt }

/// <summary>
/// Represents all statements of given CFG with mantaining block id and the
/// offset of each statement as key.
/// </summary>
type StatementIndex = Map<VertexID * int, StatementEntry>

/// Construct StatementIndex from given cfg
let build (cfg: SSACFG) : StatementIndex =
  (* Extract all stmts and construct StatementIndex of given block *)
  let constructStmtIdx (block: IVertex<SSABasicBlock>) =
    block.VData.Internals.Statements
    |> Seq.mapi (fun index (programPoint, stmt) ->
      (block.ID, index),
      { BlockId = block.ID
        ProgramPoint = programPoint
        Statement = stmt })

  cfg.Vertices |> Seq.collect constructStmtIdx |> Map.ofSeq
