module PointerAnalyzer.AbsDom.RegMap

open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal

/// <summary>
/// Mapping from SSA varaible to its abstract integer.
/// </summary>
type RegMap = Map<Variable, AbsVal>

type RegMapModule (platform: Platform) =
  let absVal = AbsValDomain.create platform

  member _.bot: RegMap = Map.empty

  /// Return abstract value of given register
  member _.tryFind variable (regMap: RegMap) = Map.tryFind variable regMap

  /// Return abstract value of given register
  member _.find variable (regMap: RegMap) =
    Map.tryFind variable regMap |> Option.defaultValue absVal.bot

  /// Record the abstract value of given register as given value
  member _.add variable value (regMap: RegMap) = Map.add variable value regMap

  /// Join RegMap
  member _.join (left: RegMap) (right: RegMap) =
    let joinInner acc variable value =
      match Map.tryFind variable acc with
      | Some oldValue -> Map.add variable (absVal.join oldValue value) acc
      | None -> Map.add variable value acc

    Map.fold joinInner right left

  member _.toString (registers: RegMap) =
    let printElem (variable, value) =
      sprintf "%s |-> %s" (Variable.ToString variable) (absVal.toString value)

    registers |> Map.toList |> List.map printElem |> String.concat "\n"

module RegMapDomain =
  let create platform = RegMapModule platform
