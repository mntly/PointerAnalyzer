module EvaluateAnalyzer.Evaluator.Types

/// <summary>
/// Represent the type of the PointerAnalyzer consider.
/// </summary>
type EvalType =
  | Address
  | Value
  | Unknown
  | Conflict

  member this.ToString =
    match this with
    | Address -> "Address"
    | Value -> "Value"
    | Unknown -> "Unknown"
    | Conflict -> "Conflict"

/// <summary>
/// Represent the type of each slot.
/// </summary>
/// <remarks>
/// <c>Argument</c> indicates normal argument whose type is not structure.
/// <c>ArgumentSlot</c> indicates the slot as the field of argument structure.
/// <c>Return</c> indicates normal return whose type is not structure.
/// <c>ReturnSlot</c> indicates the slot as the field of return structure.
/// </remarks>
type EvalTarget =
  | Argument of int
  | ArgumentSlot of argumentIndex: int * slotIndex: int * path: string
  | Return of int
  | ReturnSlot of returnIndex: int * slotIndex: int * path: string

/// Location in PointerAnalyzer's inferred signature used by an evaluation.
type InferredSource =
  | ArgumentSource of int
  | ReturnSource of int

/// <summary>
/// Represent the result of evaluation.
/// This evaluation is only valid when GT type is not Unknown.
/// </summary>
/// <remarks>
/// <c>Correct</c> indicates that GT type and inferred type are same.
/// <c>MisInferred</c> indicates that GT type and inferred type are different
/// when inferred sucess(not Unknown).
/// <c>ConflictResult</c> indicates that PointerAnalyzer inferred as both
/// Address and Value type.
/// <c>Fail</c> indicates that PointerAnalyzer can not infer to one concreate
/// type, i.e PointerAnalyzer inferred as Unknown type.
/// </remarks>
type EvalCategory =
  | Correct
  | MisInferred
  | ConflictResult
  | Fail

/// <summary>
/// Represent key for identify each function.
/// </summary>
type FunctionKey = { Address: string; Name: string }

/// <summary>
/// Source-level(Raw) ground-truth type extracted from DWARF.
/// </summary>
type RawGTType =
  | RawAddress
  | RawValue
  | RawUnknown
  | RawStructure of RawGTField list

/// <summary>
/// Indicates the source-level(raw) ground-truth type information of structure
/// field. Offset is relative to its containing structure.
/// </summary>
and RawGTField =
  { Name: string
    Offset: int
    Size: int
    Type: RawGTType }

/// <summary>
/// Source-level(raw) ground-truth element before ABI conversion.
/// </summary>
type RawGTElement = { Size: int; Type: RawGTType }

/// <summary>
/// Information passed from PointerAnalyzer used for Evaluation.
/// </summary>
type AnalysisConfig = { Platform: string; WordSize: int }

/// <summary>
/// Source-level(raw) ground-truth function signature before ABI conversion.
/// </summary>
type RawGTFunction =
  { Function: FunctionKey
    Args: RawGTElement list
    Return: RawGTElement list }

/// <summary>
/// Indicates whether corresponding element is field of structure or not after
/// ABI conversion.
/// </summary>
type GTElementKind =
  | NormalElement
  | StructureElement

/// <summary>
/// Indicates Word-Size GT type information per word slot. The element such as
/// parameter and return value is divided into word-size slot.
/// </summary>
/// <remarks>
/// <c>Index</c> indicates the word size index of current word slot among its
/// containing element.
/// <c>Size</c> indicates the size of current word slot. It should be same as
/// word size.
/// <c>Type</c> represents GT type of each word slot.
/// <c>Path</c> represents the field name if current slot is field of
/// structure. This is represented as `StructureName.FieldName`.
/// </remarks>
type GTSlot =
  { Index: int
    Size: int
    Type: EvalType
    Path: string }

/// <summary>
/// Indicates ABI-converted GT type per element.
/// </summary>
/// <remarks>
/// <c>Size</c> indicates that the size of current element. It should be the
/// sum of its slot's size.
/// <c>Type</c> represents GT type of each word slot. If corresponding element
/// is structure, this field has no meaning.
/// <c>Kind</c> indicates that corresponding element is structure or not.
/// <c>OccupiedSlotCount</c> represents the total number of slots in
/// corresponding element.
/// <c>Slots</c> stores the ABI-converted GT type of its slots.
/// </remarks>
type GTElement =
  { Size: int
    Type: EvalType
    Kind: GTElementKind
    OccupiedSlotCount: int
    Slots: GTSlot list }

/// <summary>
/// Represent per-function function signature.
/// </summary>
type GTFunction =
  { Function: FunctionKey
    Args: GTElement list
    Return: GTElement list }

/// <summary>
/// Represent per-function inferred function signature.
/// </summary>
type InferredFunction =
  { Function: FunctionKey
    Args: Map<int, EvalType>
    Return: EvalType list }

/// <summary>
/// Represents the number of slots in GT function signature and inferred
/// function signature.
/// </summary>
type StructureCoverage =
  { Function: FunctionKey
    Target: EvalTarget
    ExpectedSlots: int
    ObservedSlots: int }

/// <summary>
/// Represent per-variable evaluation result.
/// </summary>
/// <remarks>
/// <c>Function</c> indicates the function infor containing current element.
/// <c>Target</c> indicates what is the current element in corresponding
/// function.
/// <c>GT</c> is ABI-converted GT type.
/// <c>Inferred</c> is the result of PointerAnalyzer.
/// <c>Category</c> indicates the evaluation result, one of .
/// </remarks>
type ElementResult =
  { Function: FunctionKey
    Target: EvalTarget
    GT: EvalType
    Inferred: EvalType
    Sources: InferredSource list
    Category: EvalCategory }

type ProvenanceFact = { Type: EvalType; TypeId: int }

type ProvenanceOrigin =
  { FunctionName: string
    Location: string
    Statement: string
    Annotation: string }

type ProvenanceDerivation =
  { Constraint: string
    Premises: ProvenanceFact list
    OriginId: int option }

type FunctionProvenance =
  { Name: string
    Arguments: Map<int, int>
    Return: int list }

type ProvenanceData =
  { Functions: Map<string, FunctionProvenance>
    TypeNames: Map<int, string list>
    Origins: Map<int, ProvenanceOrigin>
    Derivations: Map<string, ProvenanceDerivation> }

/// <summary>
/// Used for tracking the detail of evaluation.
/// </summary>
type EvalLogState =
  { GTUnknown: Set<FunctionKey>
    InvalidGTSize: Set<FunctionKey>
    LargeReturn: Set<FunctionKey>
    MissedDetect: Set<FunctionKey>
    CountMismatch: Set<FunctionKey>
    Correct: Set<FunctionKey>
    MisInferred: Set<FunctionKey>
    Conflict: Set<FunctionKey>
    Fail: Set<FunctionKey>
    InferMoreParams: Set<FunctionKey>
    StructureCoverage: Set<StructureCoverage> }

module EvalLogState =
  let empty =
    { GTUnknown = Set.empty
      InvalidGTSize = Set.empty
      LargeReturn = Set.empty
      MissedDetect = Set.empty
      CountMismatch = Set.empty
      Correct = Set.empty
      MisInferred = Set.empty
      Conflict = Set.empty
      Fail = Set.empty
      InferMoreParams = Set.empty
      StructureCoverage = Set.empty }
