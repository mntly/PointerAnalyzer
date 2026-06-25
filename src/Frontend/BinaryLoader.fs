module PointerAnalyzer.Frontend.BinaryLoader

open System.IO
open B2R2
open B2R2.FrontEnd
open PointerAnalyzer
open PointerAnalyzer.Platform.CallingConvention

type LoadedBinary =
  { Path: string
    Handle: BinHandle
    Architecture: Architecture
    CallingConvention: CallingConvention }

module BinaryLoader =
  let private analyzerArchitecture (isa: ISA) =
    match isa with
    | X86 -> ArchX86_32
    | _ ->
      invalidArg
        (nameof isa)
        (sprintf "Unsupported binary ISA: %A. Current domains support x86-32." isa)

  let load path =
    if not (File.Exists path) then
      invalidArg (nameof path) (sprintf "Binary does not exist: %s" path)

    let handle = BinHandle path
    let architecture = analyzerArchitecture handle.File.ISA

    { Path = Path.GetFullPath path
      Handle = handle
      Architecture = architecture
      CallingConvention = CallingConvention.forBinary architecture handle }
