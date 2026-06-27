module PointerAnalyzer.Result2Json

open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open B2R2
open PointerAnalyzer.Interproc.ModularAnalyzer
open PointerAnalyzer.Summary
open PointerAnalyzer.TypeInference.ResolvedType

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
  let private indexedTypesToStringList constraints conflicts indexedTypes =
    let resolveTypeId2Str (_idx, typeId) =
      let resolvedType = ResolvedTypeInfo.ofTypeId constraints conflicts typeId
      resolvedType.Type.ToOutputString

    let sortedIndexedTypes = indexedTypes |> Map.toSeq |> Seq.sortBy fst
    sortedIndexedTypes |> Seq.map resolveTypeId2Str |> Seq.toList

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
      let resolvedTypes =
        ResolvedTypeMap.build
          constraints
          conflicts
          funAnalysis.Result.FinalState.Types.TypeIndicators

      TypePerInst.build resolvedTypes funAnalysis.Function.DFAResult.Statements

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
