namespace PointerAnalyzer.Summary

open B2R2
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeIdMap

type FunctionSummary =
  { Address: Addr
    Name: string
    Parameters: Map<int, TypeId>
    RegisterOutputs: Map<RegisterID, TypeId>
    Constraints: ConstraintSet
    ConstraintOrigins: Map<TypeConstraint, ConstraintOrigin> option
    NextTypeId: TypeId }

  member this.ParamToString =
    let header = "  Parameters:\n"

    let content =
      if Map.isEmpty this.Parameters then
        "    <none detected>\n"
      else
        this.Parameters
        |> Map.toSeq
        |> Seq.map (fun (index, typeId) ->
          sprintf "    arg%d -> t%d\n" index typeId)
        |> String.concat ""

    header + content

  member this.RegisterOutputsToString =
    let header = "  Register outputs:\n"

    let content =
      if Map.isEmpty this.RegisterOutputs then
        "    <none detected>\n"
      else
        this.RegisterOutputs
        |> Map.toSeq
        |> Seq.map (fun (regId, typeId) ->
          sprintf "    reg%A -> t%d\n" regId typeId)
        |> String.concat ""

    header + content

  member this.ConstraintsToString = this.Constraints.ToString

module FunctionSummary =
  let empty address name nextTypeId =
    { Address = address
      Name = name
      Parameters = Map.empty
      RegisterOutputs = Map.empty
      Constraints = Set.empty
      ConstraintOrigins = None
      NextTypeId = nextTypeId }
