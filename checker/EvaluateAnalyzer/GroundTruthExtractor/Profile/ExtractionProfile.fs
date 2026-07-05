module EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile

open EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// <summary>
/// Profile of extracting given library codes.
/// Current, this GT extraction construct profile of uClibc at
/// <see cref="module EvaluateAnalyzer.GroundTruthExtractor.UClibc.UClibcProfile" />.
/// </summary>
/// <remarks>
/// <c>Name</c> indicates the type of corresponding library.
/// <c>DefaultLibRoot</c> indicates the path of root directory of library to
/// extract.
/// <c>GetSourceFiles</c> is the function to extract possible .c and .h files
/// used for extracting type information under given path.
/// <c>RelativePath</c> is the function to convert relative path from its
/// library.
/// <c>NormalizeSourceText</c> is the function to remove unrelated keywards for
/// type ground truth extraction.
/// <c>ExtractAliases</c> is the function to remove unrelated keywards for
/// type ground truth extraction.
/// <c>ClassifyCType</c> is the function to classify given ctype to type value.
/// <c>NormalizeCType</c> is the function to remove type-unrelated keywords and
/// continuous blank.
/// </remarks>
type SourceExtractionProfile =
  { Name: string
    DefaultLibRoot: string
    GetSourceFiles: string -> string list
    RelativePath: string -> string -> string
    NormalizeSourceText: string -> string
    ExtractAliases: string -> AliasTruth list
    ClassifyCType: string -> string
    NormalizeCType: string -> string }

type TargetBinaryProfile =
  { Name: string
    FunctionNames: string -> Set<string> }
