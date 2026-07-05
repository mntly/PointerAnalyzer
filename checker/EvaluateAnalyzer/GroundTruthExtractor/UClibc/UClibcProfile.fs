module EvaluateAnalyzer.GroundTruthExtractor.UClibc.UClibcProfile

open System.IO
open System.Text.RegularExpressions
open EvaluateAnalyzer.GroundTruthExtractor.C
open EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile
open EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// Default path of uClibc directory
let defaultLibRoot =
  "/mnt/c/MyProject/SoftSec/vSim/Datas/uClibc-ng-1.0.57/uClibc-ng-1.0.57"

/// Only check within source code
let private skippedDirectoryNames =
  set [ ".git"; "test"; "tests"; "docs"; "doc"; "extra/scripts" ]

/// Check given path belongs to uClibc code
let shouldSkipPath (path: string) =
  let normalized = path.Replace('\\', '/').ToLowerInvariant ()

  skippedDirectoryNames
  |> Seq.exists (fun dir -> normalized.Contains ("/" + dir + "/"))

/// Extract possible .c and .h files to extract type information under given
/// path
let sourceFiles root =
  let filterCH (path: string) =
    let ext = Path.GetExtension(path).ToLowerInvariant ()
    (ext = ".c" || ext = ".h") && not (shouldSkipPath path)

  Directory.EnumerateFiles (root, "*.*", SearchOption.AllDirectories)
  |> Seq.filter filterCH
  |> Seq.toList

/// Covert absolute path to relative path based on root
/// The root becomes the path of library directory
let relativePath root path =
  Path.GetRelativePath(root, path).Replace ('\\', '/')

/// Remove unrelated keywards for type ground truth extraction
let normalizeSourceText (text: string) =
  text
  |> fun s -> Regex.Replace (s, @"\blibc_hidden_proto\s*\([^)]*\)", "")
  |> fun s -> Regex.Replace (s, @"\blibc_hidden_def\s*\([^)]*\)", "")
  |> fun s -> Regex.Replace (s, @"\blibc_hidden_weak\s*\([^)]*\)", "")
  |> fun s -> Regex.Replace (s, @"\b__BEGIN_NAMESPACE_[A-Za-z0-9_]+\b", "")
  |> fun s -> Regex.Replace (s, @"\b__END_NAMESPACE_[A-Za-z0-9_]+\b", "")
  |> fun s -> Regex.Replace (s, @"\b__BEGIN_DECLS\b", "")
  |> fun s -> Regex.Replace (s, @"\b__END_DECLS\b", "")
  |> fun s -> Regex.Replace (s, @"\b__THROW\b", "")
  |> fun s -> Regex.Replace (s, @"\b__THROWNL\b", "")
  |> fun s -> Regex.Replace (s, @"\battribute_hidden\b", "")
  |> fun s -> Regex.Replace (s, @"\binternal_function\b", "")
  |> fun s -> Regex.Replace (s, @"\b__nonnull\s*\([^)]*\)", "")
  |> fun s ->
      Regex.Replace (
        s,
        @"\b__attribute__\s*\(\(.*?\)\)",
        "",
        RegexOptions.Singleline
      )
      |> fun s -> Regex.Replace (s, @"\bweak_function\b", "")
      |> fun s -> Regex.Replace (s, @"\bwarn_unused_result\b", "")

/// Extract aliases of each function and construct alias name list
let extractAliasesFromText (text: string) =
  let aliasRegex =
    Regex (
      @"\b(?<kind>weak_alias|strong_alias)\s*\(\s*(?<real>[A-Za-z_]\w*)\s*,\s*(?<alias>[A-Za-z_]\w*)\s*\)",
      RegexOptions.Compiled
    )

  aliasRegex.Matches text
  |> Seq.cast<Match>
  |> Seq.map (fun m ->
    { Alias = m.Groups["alias"].Value
      CanonicalName = m.Groups["real"].Value })
  |> Seq.toList

/// uClibc profile to extract GT
let profile: SourceExtractionProfile =
  { Name = "uClibc-ng"
    DefaultLibRoot = defaultLibRoot
    GetSourceFiles = sourceFiles
    RelativePath = relativePath
    NormalizeSourceText = normalizeSourceText
    ExtractAliases = extractAliasesFromText
    ClassifyCType = CTypeClassifier.classify >> CTypeClassifier.toString
    NormalizeCType = CTypeClassifier.normalizeCType }
