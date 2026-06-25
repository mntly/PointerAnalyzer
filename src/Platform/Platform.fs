module PointerAnalyzer.Platform.Platform

open B2R2
open B2R2.FrontEnd
open B2R2.FrontEnd.BinFile

let forBinary (handle: BinHandle) =
  match handle.File.ISA, handle.File.Format with
  | ISA.X86, FileFormat.ELFBinary -> ELF.X86_32.Platform.create ()
  | _ ->
    invalidArg
      (nameof handle)
      (sprintf
        "Unsupported platform for ISA %A and file format %A."
        handle.File.ISA
        handle.File.Format)

let ofString platform =
  match platform with
  | "elf-x86"
  | "elf-x86-32"
  | "elf-i386"
  | "elf-i586"
  | "x86"
  | "x86_32"
  | "i386"
  | "i586" -> ELF.X86_32.Platform.create ()
  | _ ->
    invalidArg (nameof platform) (sprintf "Unsupported platform: %s" platform)
