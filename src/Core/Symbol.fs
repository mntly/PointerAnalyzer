namespace PointerAnalyzer

/// Symbols used in modular analysis.

type SymbolInt = SymVal of int
type SymbolLoc = SymLoc of int

type Symbol =
  | IntSymbol of SymbolInt
  | LocSymbol of SymbolLoc

type SymbolIdx =
  { NextSymIntIdx: int
    NextSymLocIdx: int }

module Symbol =
  let toString =
    function
    | IntSymbol (SymVal sint) -> sprintf "SymVal(%d)" sint
    | LocSymbol (SymLoc sloc) -> sprintf "SymLoc(%d)" sloc

module SymbolInt =
  let toString sym = Symbol.toString (IntSymbol sym)

module SymbolLoc =
  let toString sym = Symbol.toString (LocSymbol sym)

module SymbolIdx =
  let create startSymIntIdx startSymLocIdx =
    { NextSymIntIdx = startSymIntIdx
      NextSymLocIdx = startSymLocIdx }

  let freshInt idx =
    SymVal idx.NextSymIntIdx,
    { idx with NextSymIntIdx = idx.NextSymIntIdx + 1 }

  let freshLoc idx =
    SymLoc idx.NextSymLocIdx,
    { idx with NextSymLocIdx = idx.NextSymLocIdx + 1 }
