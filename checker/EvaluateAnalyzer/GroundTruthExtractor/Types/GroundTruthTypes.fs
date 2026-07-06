module EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// <summary>
/// Indicate how the GroundTruthExtractor extracts ground truth type
/// information.
/// </summary>
/// <remarks>
/// <c>TargetBinary</c> is the method extracting ground truth of the
/// unextracted functions in given binary.
/// <c>AllUClibc</c> is the method extracting ground truth of the
/// all unextracted functions in given library codes.
/// </remarks>
type GroundTruthExtractMode =
  | TargetBinary
  | AllUClibc

/// <summary>
/// Represent the type of return register.
/// </summary>
type TypeTruth = { CType: string; Kind: string }

/// <summary>
/// Represent the type of parameters.
/// </summary>
type ParameterTruth =
  { Index: int
    Name: string
    CType: string
    Kind: string }

/// <summary>
/// Represent the type of parameters and return value of each function.
/// This used for storing the result as JSON format.
/// </summary>
type FunctionTruth =
  { Name: string
    CanonicalName: string
    Source: string
    Prototype: string
    Return: TypeTruth
    Parameters: ParameterTruth list }

/// <summary>
/// Represent one direct alias relation extracted from source.
/// For exmaple, `Alias` is extracted as `calloc` and `CanonicalName` is
/// extracted as `__libc_calloc` in `weak_alias(__libc_calloc, calloc)`
/// </summary>
type AliasTruth =
  { Alias: string; CanonicalName: string }

/// <summary>
/// Represent all relationship of alias names and its representative name.
/// </summary>
type AliasGroupTruth =
  { Representative: string
    Names: string list }

/// <summary>
/// Represent extracted ground truth type information.
/// This used for storing the mismatched result as JSON format.
/// </summary>
type SignatureTruth =
  { Name: string
    Source: string
    Prototype: string
    Return: TypeTruth
    Parameters: ParameterTruth list }

/// <summary>
/// Represent the type signature of mismatched functions among related aliases.
/// </summary>
type TypeMismatchTruth =
  { Representative: string
    Names: string list
    Signatures: SignatureTruth list }

/// <summary>
/// Represent the missed functions in library even in given binary.
/// </summary>
type MissingTruth = { Name: string; Reason: string }

/// Track the function representation result
type GroundTruthDb =
  { LibRoot: string
    SourceProfile: string
    TargetProfile: string
    Mode: string
    Functions: FunctionTruth list
    Aliases: AliasTruth list
    AliasGroups: AliasGroupTruth list
    TypeMismatches: TypeMismatchTruth list
    Missing: MissingTruth list }

module GroundTruthExtractMode =
  let toString =
    function
    | TargetBinary -> "target-binary"
    | AllUClibc -> "all-uclibc"

  let ofInt =
    function
    | 0 -> TargetBinary
    | 1 -> AllUClibc
    | mode -> failwithf "Unsupported ground-truth mode %d" mode
