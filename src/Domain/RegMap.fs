module PointerAnalyzer.AbsDom.RegMap

open B2R2.BinIR.SSA
open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsVal

/// R_M = R -> V, where an SSA Variable is the register indicator R.
type RegMap = Map<Variable, AbsVal>

type RegMapModule (architecture: Architecture) =
  let absVal = AbsValDomain.create architecture

  member _.bot: RegMap = Map.empty

  member _.tryFind variable regMap = Map.tryFind variable regMap

  member _.find variable regMap =
    Map.tryFind variable regMap |> Option.defaultValue absVal.bot

  member _.add variable value regMap = Map.add variable value regMap

  member _.join (left: Map<Variable, AbsVal>) right =
    let joinInner acc variable value =
      match Map.tryFind variable acc with
      | Some oldValue -> Map.add variable (absVal.join oldValue value) acc
      | None -> Map.add variable value acc

    Map.fold joinInner right left

  member _.leq left right =
    let leqInner variable value =
      match Map.tryFind variable right with
      | Some rightValue -> absVal.leq value rightValue
      | None -> false

    Map.forall leqInner left

  member _.toString registers =
    let printElem (variable, value) =
      sprintf "%s |-> %s" (Variable.ToString variable) (absVal.toString value)

    registers |> Map.toList |> List.map printElem |> String.concat "\n"

module RegMapDomain =
  let create architecture = RegMapModule architecture
