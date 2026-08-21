module PointerAnalyzer.PreAnalysis.PreAnalysisTypes

open B2R2.BinIR.SSA
open B2R2
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.ProgramDFA

(*
  ToDo
    Liveness detection is only needed for remove type ambiguity.
    I think only PHI node may affect type ambiguity, so keep the previous logic
    of PointerAnalyzer, and only adopt checking liveness at PHI node.

    To figure out the affect of this, I'll gradually increase the target of
    liveness, such as Branch Condition, Memory Reference, ...
*)

/// Store live registers. PreAnalyzer extracts live registers from
/// 1. Live registers in Leaf Node of CFG
/// 2. SSA varaibles used for arguments of each function
/// 3. SSA varaibles used for jump targets
type PreAnalysisResult =
  { LiveVariables: Set<Variable>
    DetectedRegParams: (RegisterID * Variable) list }

module PreAnalysisResult =
  let empty =
    { LiveVariables = Set.empty
      DetectedRegParams = [] }

  /// Check given SSA variable is Live or Dead
  let isLive variable result =
    Set.contains variable result.LiveVariables

/// Construct function pre-analysis result by combining function DFA result
type FunctionPreResult =
  { FunctionDFA: FunctionDFAResult
    PreAnalysis: PreAnalysisResult }

/// Construct binary pre-analysis result by combining binary DFA result
type ProgramPreResult =
  { Binary: LoadedBinary
    Functions: Map<Addr, FunctionPreResult>
    VisitOrder: Addr list }
