namespace PointerAnalyzer.Summary

open B2R2
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

type FunctionSummary =
  { Address: Addr
    Name: string
    Parameters: Map<int, TypeId>
    Returns: Map<int, TypeId>
    Constraints: ConstraintSet
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

  member this.ReturnToString =
    let header = "  Returns:\n"

    let content =
      if Map.isEmpty this.Returns then
        "    <none detected>\n"
      else
        this.Returns
        |> Map.toSeq
        |> Seq.map (fun (index, typeId) ->
          sprintf "    ret%d -> t%d\n" index typeId)
        |> String.concat ""

    header + content

  member this.ConstraintsToString = this.Constraints.ToString

module FunctionSummary =
  let empty address name nextTypeId =
    { Address = address
      Name = name
      Parameters = Map.empty
      Returns = Map.empty
      Constraints = Set.empty
      NextTypeId = nextTypeId }
