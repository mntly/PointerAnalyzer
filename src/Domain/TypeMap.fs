module PointerAnalyzer.AbsDom.TypeMap

open B2R2.BinIR.SSA

type TypeId = int

/// T_I = R -> N
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
      sprintf "%s |-> tid_%d" (Variable.ToString variable) typeId)
    |> String.concat "\n"

module TypeMapDomain =
  let create () = TypeMapModule ()
