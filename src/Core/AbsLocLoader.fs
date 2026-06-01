namespace PointerAnalyzer

module AbsLocLoader =
  let load architecture =
    match architecture with
    | ArchX86_32 -> Convention.X86_32.AbsLoc.values
