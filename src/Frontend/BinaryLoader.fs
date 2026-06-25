module PointerAnalyzer.Frontend.BinaryLoader

open System.IO
open B2R2.FrontEnd
open PointerAnalyzer.Platform.PlatformTypes

type LoadedBinary =
  { Path: string
    Handle: BinHandle
    Platform: Platform }

module BinaryLoader =
  let load path =
    if not (File.Exists path) then
      invalidArg (nameof path) (sprintf "Binary does not exist: %s" path)

    let handle = BinHandle path

    { Path = Path.GetFullPath path
      Handle = handle
      Platform = PointerAnalyzer.Platform.Platform.forBinary handle }
