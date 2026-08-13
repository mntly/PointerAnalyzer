module Checker.Return64Detection.Return64Types

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph

/// <summary>
/// Represent the range for 64 bit return value detection.
/// </summary>
/// <remarks>
/// <c>LeafAndDirectPredecessors</c> indicates Return64Detector analyzes only
/// return leaf node and its immadiate predecessors of given function CFG.
/// <c>EntireFunction</c> indicates Return64Detector analyzes entire nodes of
/// given function CFG.
/// </remarks>
type AnalysisRange =
  | LeafAndDirectPredecessors
  | EntireFunction

/// <summary>
/// Represent the type of heuristic ruls to use for analyzing.
/// </summary>
/// <remarks>
/// <c>Basic</c> indicates Return64Detector analyzes using statements
/// evaluation heuristics.
/// <c>BasicWithCallerChecker</c> indicates Return64Detector analyzes both
/// statements evaluation heuristics and caller-callee relationship.
/// </remarks>
type DetectionHeuristic =
  | Basic
  | BasicWithCallerChecker

/// <summary>
/// Represent the analysis result of Return64Detector.
/// </summary>
/// <remarks>
/// <c>Return64</c> indicates corresponding function returns 64 bit value.
/// <c>NotReturn64</c> indicates corresponding function returns 32 bit value.
/// <c>Unknown</c> indicates analyzer cannot determine the size of return value.
/// <c>UnknownCalleEvidence</c> indicates basic heuristics said corresponding
/// function returns 64 bit value, but there does not exist caller-callee
/// relationship evidence to support this.
/// </remarks>
type DetectionStatus =
  | Return64
  | NotReturn64
  | Unknown
  | UnknownCallEvidence

/// <summary>
/// Stores the version(identifier) of SSA variables which live at ret leaf node.
/// </summary>
/// <remarks>
/// <c>Versions</c> stores all version information of all incoming edges. If
/// PHI merges them, then it tracks single version, if not it tracks various
/// versions.
/// <c>DefinedOnAllPaths</c> indicates that corresponding SSA variables are
/// defined at current execution path.
/// </remarks>
type VersionState =
  { Versions: Set<Variable>
    DefinedOnAllPaths: bool }

module VersionState =
  /// Corresponding SSA variables are not set at this execution path.
  let undefined =
    { Versions = Set.empty
      DefinedOnAllPaths = false }

  /// Corresponding SSA variables are set as given variable version.
  let define variable =
    { Versions = Set.singleton variable
      DefinedOnAllPaths = true }

  /// Join of VersionState: Set union.
  let join left right =
    { Versions = Set.union left.Versions right.Versions
      DefinedOnAllPaths = left.DefinedOnAllPaths && right.DefinedOnAllPaths }

/// <summary>
/// Target registers used for detecting return 64 bit value.
/// Since x86-32 uses EAX:EDX as 64 bit return value, Return64Detector tracks
/// EAX and EDX.
/// </summary>
type RegisterState =
  { EAX: VersionState; EDX: VersionState }

module RegisterState =
  /// Initial State
  let empty =
    { EAX = VersionState.undefined
      EDX = VersionState.undefined }

  /// Join of RegisterState
  let join left right =
    { EAX = VersionState.join left.EAX right.EAX
      EDX = VersionState.join left.EDX right.EDX }

/// <summary>
/// Represent target blocks per leaf node to analyze.
/// </summary>
type ReturnRange =
  { LeafId: VertexID
    BlockIds: Set<VertexID> }

/// <summary>
/// Represent the reason that corresponding register is not return register by
/// heurisitc rules.
/// </summary>
type UseFailure =
  { Variable: Variable
    ProgramPoint: ProgramPoint
    Reason: string }

/// <summary>
/// Represent the result of register detector.
/// </summary>
type RegisterDetection =
  { Versions: Set<Variable>
    DefinedOnAllPaths: bool
    Accepted: bool
    Failures: UseFailure list }

/// <summary>
/// Represent the result of leaf node detector.
/// </summary>
/// <remarks>
/// <c>LeafId</c> represents the block id of corresponding return leaf node.
/// <c>BlockIds</c> represents the block ids analyzed.
/// <c>EAX</c> represents analysis result of EAX register.
/// <c>EDX</c> represents analysis result of EDX register.
/// <c>Accepted</c> indicates whether this result can act as the return state.
/// </remarks>
type LeafDetection =
  { LeafId: VertexID
    BlockIds: Set<VertexID>
    EAX: RegisterDetection
    EDX: RegisterDetection
    Accepted: bool }

type CallerEvidence =
  { CallerAddress: Addr
    CallSite: Addr
    EDXVariable: Variable
    Uses: UseFailure list }

/// <summary>
/// Represent the result of leaf function detector.
/// </summary>
type FunctionDetection =
  { Address: Addr
    Name: string
    BasicStatus: DetectionStatus
    Status: DetectionStatus
    Leaves: LeafDetection list
    CallerEvidence: CallerEvidence list }

/// <summary>
/// Represent the result of Return64Detector.
/// </summary>
type DetectionResult =
  { BinaryPath: string
    Platform: string
    WordSize: int
    Range: AnalysisRange
    Heuristic: DetectionHeuristic
    B2R2Diagnostics:
      PointerAnalyzer.Frontend.B2R2Diagnostics.UnsupportedInstructionDiagnostic list
    Functions: Map<Addr, FunctionDetection> }
