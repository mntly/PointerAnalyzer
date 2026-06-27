module PointerAnalyzer.Result2Json

open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open B2R2
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.Interproc.ModularAnalyzer
open PointerAnalyzer.Summary

type ArgumentsJson =
  { [<JsonPropertyName("ArgNum")>]
    ArgNum: int

    [<JsonPropertyName("Args")>]
    Args: string list }

type ReturnJson =
  { [<JsonPropertyName("ReturnNum")>]
    ReturnNum: int

    [<JsonPropertyName("Returns")>]
    Returns: string list }

type PP = string
type SSARegName = string
type InferredType = string
type DetailType = Map<PP, Map<SSARegName, InferredType>>

type FunctionJson =
  { [<JsonPropertyName("Name")>]
    Name: string

    [<JsonPropertyName("Arguments")>]
    Arguments: ArgumentsJson

    [<JsonPropertyName("Return")>]
    Return: ReturnJson

    [<JsonPropertyName("DetailType")>]
    DetailType: DetailType }

type AnalysisResultJson = Map<string, FunctionJson>

module FunctionJson =
  let private typeToOutputString
    (constraints: ConstraintSet)
    (conflicts: Set<TypeId>)
    typeId
    =
    if Set.contains typeId conflicts then
      "Conflict"
    else
      let isAddress = Set.contains (TypeConstraint.Address typeId) constraints

      let isValue = Set.contains (TypeConstraint.Value typeId) constraints

      match isAddress, isValue with
      | true, false -> "Address"
      | false, true -> "Value"
      | true, true -> "Conflict"
      | false, false -> "Unknown"

  let private indexedTypesToStringList constraints conflicts indexedTypes =
    indexedTypes
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map (fun (_, typeId) ->
      typeToOutputString constraints conflicts typeId)
    |> Seq.toList

  let fromAnalysisResult
    (resultAnalysisResult: ModularAnalysisResult)
    (funAnalysis: FunctionAnalysisResult)
    =
    let constraints = resultAnalysisResult.TypeConstraints
    let conflicts = resultAnalysisResult.TypeConflicts

    let args =
      indexedTypesToStringList
        constraints
        conflicts
        funAnalysis.Summary.Parameters

    let returns =
      indexedTypesToStringList constraints conflicts funAnalysis.Summary.Returns

    let detailType =
      TypePerInst.build
        constraints
        conflicts
        funAnalysis.Result.FinalState.Types.TypeIndicators
        funAnalysis.Function.DFAResult.Statements

    { Name = funAnalysis.Function.Name
      Arguments =
        { ArgNum = List.length args
          Args = args }
      Return =
        { ReturnNum = List.length returns
          Returns = returns }
      DetailType = detailType }

module AnalysisResultJson =
  let private jsonOptions =
    JsonSerializerOptions (
      WriteIndented = true,
      Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  let private addressToHexString (address: Addr) = sprintf "0x%08x" address

  let fromAnalysisResult
    resultAnalysisResult
    targetFunctions
    : AnalysisResultJson =

    let func2JsonElem (address, funAnalysis) =
      let addrStr = addressToHexString address

      let funJson =
        FunctionJson.fromAnalysisResult resultAnalysisResult funAnalysis

      addrStr, funJson

    targetFunctions |> Map.toSeq |> Seq.map func2JsonElem |> Map.ofSeq

  let toJsonString (analysisResultJson: AnalysisResultJson) =
    JsonSerializer.Serialize (analysisResultJson, jsonOptions) + "\n"

  let fromAnalysisResultToJsonString resultAnalysisResult targetFunctions =
    fromAnalysisResult resultAnalysisResult targetFunctions |> toJsonString
