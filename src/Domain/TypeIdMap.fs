module PointerAnalyzer.AbsDom.TypeIdMap

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
type TypeIdMap = Map<Variable, TypeId>

type TypeIdMapModule () =
  member _.bot: TypeIdMap = Map.empty

  /// Return type Id of given SSA variable
  member _.tryFind variable (typeIndicators: TypeIdMap) =
    Map.tryFind variable typeIndicators

  /// Return the Id of given SSA variable
  member _.find variable (typeIndicators: TypeIdMap) =
    Map.tryFind variable typeIndicators

  /// Set the type Id of given variable as given type Id
  member _.add variable typeId (typeIndicators: TypeIdMap) =
    Map.add variable typeId typeIndicators

module TypeIdMapDomain =
  let create () = TypeIdMapModule ()
