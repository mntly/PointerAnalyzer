module PointerAnalyzer.Frontend.BinaryLoader

open System.IO
open B2R2.FrontEnd
open PointerAnalyzer.Platform.PlatformTypes

/// <summary>
/// Binary context used by PointerAnalyzer.
/// </summary>
/// <remarks>
/// <c>Handle</c> is B2R2's <see cref="B2R2.FrontEnd.BinHandle" />
/// for pre-analysis.
/// <c>Platform</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.Platform.PlatformTypes.Platform" /> for
/// specifying target-specific information, such as calling conventions.
/// </remarks>
type LoadedBinary =
  { Path: string
    Handle: BinHandle
    Platform: Platform }

module BinaryLoader =
  /// Load binary
  let load path =
    if not (File.Exists path) then
      invalidArg (nameof path) (sprintf "Binary does not exist: %s" path)

    let handle = BinHandle path

    { Path = Path.GetFullPath path
      Handle = handle
      Platform = PointerAnalyzer.Platform.Platform.forBinary handle }
