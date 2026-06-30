module PointerAnalyzer.AbsDom.TypeMap

open B2R2.BinIR.SSA

/// <summary>
/// Identity to represent type constraint of each SSA varaible.
/// </summary>
type TypeId = int

(*
  ToDo
    Current it does not use global Type Id for Address, Value.
    If the global Type Id is removed, then change to Normal Same
*)
module TypeIds =
  let address = 0
  let value = 1
  let firstFresh = 2

/// <summary>
/// Mapping from SSA varaible to its type Id.
/// </summary>
type TypeIndicatorMap = Map<Variable, TypeId>
type TypeMap = TypeIndicatorMap

type TypeMapModule () =
  member _.bot: TypeIndicatorMap = Map.empty

  member _.tryFind variable typeIndicators = Map.tryFind variable typeIndicators

  member _.find variable typeIndicators = Map.tryFind variable typeIndicators

  member _.add variable typeId typeIndicators =
    Map.add variable typeId typeIndicators

  member _.toString typeIndicators =
    typeIndicators
    |> Map.toList
    |> List.map (fun (variable, typeId) ->
      sprintf "%s |-> t%d" (Variable.ToString variable) typeId)
    |> String.concat "\n"

module TypeMapDomain =
  let create () = TypeMapModule ()
