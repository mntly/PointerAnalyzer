namespace PointerAnalyzer.Summary

open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.TypeMap

type AbsValTyp = AbsVal * (TypeId option)

type ArgumentMap = Map<int, AbsValTyp>

type FunctionSummary =
  { Arguments: ArgumentMap
    ReturnInfo: AbsValTyp }

module FunctionSummary =
  let empty =
    { Arguments = Map.empty
      ReturnInfo = Bot, None }
