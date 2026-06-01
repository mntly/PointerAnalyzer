module PointerAnalyzer.Convention.X86_32.AbsLoc

open PointerAnalyzer

let private wordSize = int IntTypes.NInt.WORD_SIZE

let values =
  { IsStackArgument = fun offset -> offset >= 0
    IsStackRootArgument = fun offset -> offset >= 0
    IsNthStackArgument = fun n offset -> n * wordSize = offset
    TryGetStackRootArgIdx =
      fun offset -> if offset < wordSize then None else Some (offset / wordSize)
    IsSymbolicArgument = true
    IsSymbolicRootArgument = false }
