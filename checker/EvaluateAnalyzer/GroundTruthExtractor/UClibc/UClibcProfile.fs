module EvaluateAnalyzer.GroundTruthExtractor.UClibc.UClibcProfile

open System.IO
open System.Text.RegularExpressions
open EvaluateAnalyzer.GroundTruthExtractor.C
open EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile
open EvaluateAnalyzer.GroundTruthExtractor.TreeSitter

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

/// Extract implementation files to extract type information under given path.
/// Parsing every header as a standalone unit is noisy, so the first Tree-sitter
/// path focuses on implementation files.
let sourceFiles root =
  let filterC (path: string) =
    let ext = Path.GetExtension(path).ToLowerInvariant ()
    ext = ".c" && not (shouldSkipPath path)

  Directory.EnumerateFiles (root, "*.*", SearchOption.AllDirectories)
  |> Seq.filter filterC
  |> Seq.toList

/// Covert absolute path to relative path based on root.
/// The root becomes the path of library directory.
let relativePath root path =
  Path.GetRelativePath(root, path).Replace ('\\', '/')

/// Extract ground truth type using Tree-Sitter with python
let extractFacts libRoot path =
  TreeSitterCommand.extractFacts libRoot path

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

/// uClibc profile to extract GT
let profile: SourceExtractionProfile =
  { Name = "uClibc-ng"
    DefaultLibRoot = defaultLibRoot
    GetSourceFiles = sourceFiles
    RelativePath = relativePath
    NormalizeSourceText = normalizeSourceText
    ExtractFacts = extractFacts
    ClassifyCType = CTypeClassifier.classify >> CTypeClassifier.toString
    NormalizeCType = CTypeClassifier.normalizeCType }
