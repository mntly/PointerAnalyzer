namespace PointerAnalyzer.Summary

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeMap

type VarTypeMap = Map<Variable, TypeId>

module VarTypeMap =
  let empty: VarTypeMap = Map.empty

  let tryFind variable variableTypes =
    Map.tryFind variable variableTypes

  let add variable typeId variableTypes =
    Map.add variable typeId variableTypes

type TypePerInst = Map<ProgramPoint, VarTypeMap>

module TypePerInst =
  let empty: TypePerInst = Map.empty

  let findVarMap programPoint (types: TypePerInst) =
    Map.tryFind programPoint types |> Option.defaultValue Map.empty

  let tryFind programPoint variable types =
    types
    |> Map.tryFind programPoint
    |> Option.bind (Map.tryFind variable)

  let add programPoint variable typeId types =
    let variableTypes = findVarMap programPoint types
    Map.add programPoint (Map.add variable typeId variableTypes) types

  let addMany programPoint entries types =
    entries
    |> Seq.fold (fun result (variable, typeId) ->
      add programPoint variable typeId result) types

  let toString types =
    types
    |> Map.toList
    |> List.collect (fun (programPoint, variableTypes) ->
      variableTypes
      |> Map.toList
      |> List.map (fun (variable, typeId) ->
        sprintf
          "%O: %s -> t%d"
          programPoint
          (Variable.ToString variable)
          typeId))
    |> String.concat "\n"
