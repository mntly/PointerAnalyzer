module PointerAnalyzer.Result2Json

open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open B2R2
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Interproc.ModularAnalyzer
open PointerAnalyzer.Summary
open PointerAnalyzer.TypeInference.ResolvedType

type ArgumentsJson =
  { [<JsonPropertyName("ArgNum")>]
    ArgNum: int

    [<JsonPropertyName("Args")>]
    Args: Map<int, string> }

type PP = string
type SSARegName = string
type InferredType = string
type DetailType = Map<PP, Map<SSARegName, InferredType>>

type FunctionJson =
  { [<JsonPropertyName("Name")>]
    Name: string

    [<JsonPropertyName("Arguments")>]
    Arguments: ArgumentsJson

    [<JsonPropertyName("ReturnReg")>]
    ReturnReg: Map<string, string>

    [<JsonPropertyName("DetailType")>]
    DetailType: DetailType }

type AnalysisResultJson = Map<string, FunctionJson>

type AnalysisConfigJson =
  { [<JsonPropertyName("Platform")>]
    Platform: string

    [<JsonPropertyName("WordSize")>]
    WordSize: int

    [<JsonPropertyName("ReturnSlotRegisters")>]
    ReturnSlotRegisters: string list

    [<JsonPropertyName("FunctionApply")>]
    FunctionApply: bool }

module AnalysisConfigJson =
  let private jsonOptions = JsonSerializerOptions (WriteIndented = true)

  let fromPlatform
    (platform: Platform)
    functionApply
    : AnalysisConfigJson =
    { Platform = platform.Name
      WordSize = platform.WordSize
      ReturnSlotRegisters =
        platform.ReturnSlotRegisters |> List.map platform.RegisterName
      FunctionApply = functionApply }

  let toJsonString config =
    JsonSerializer.Serialize (config, jsonOptions) + "\n"

module FunctionJson =
  let private indexedTypesToStringMap constraints conflicts indexedTypes =
    let resolveTypeId2Str (_idx, typeId) =
      let resolvedType = ResolvedTypeInfo.ofTypeId constraints conflicts typeId
      resolvedType.Type.ToOutputString

    indexedTypes
    |> Map.toSeq
    |> Seq.map (fun (index, typeId) -> index, resolveTypeId2Str (index, typeId))
    |> Map.ofSeq

  /// Convert type Id into resolved Type String (Address|Value|Conflict|Unknown)
  let private typeIdToTypeString constraints conflicts typeId =
    let resolvedType = ResolvedTypeInfo.ofTypeId constraints conflicts typeId
    resolvedType.Type.ToOutputString

  /// Covert type Id of each return registers into resolved Type String
  let private returnRegTypesToStringMap
    constraints
    conflicts
    platform
    regTypes
    =
    let returnRegTypeStr (regId: RegisterID) =
      let regName = platform.RegisterName regId

      match Map.tryFind regId regTypes with
      | Some tid ->
        let typeStr = typeIdToTypeString constraints conflicts tid
        Some ((regName, typeStr))
      | None -> Some ((regName, "Unknown"))

    platform.ReturnSlotRegisters
    |> List.choose returnRegTypeStr
    |> Map.ofList

  let fromAnalysisResult
    includeDetailType
    (platform: Platform)
    (resultAnalysisResult: ModularAnalysisResult)
    (funAnalysis: FunctionAnalysisResult)
    =
    let constraints = resultAnalysisResult.TypeConstraints
    let conflicts = resultAnalysisResult.TypeConflicts

    (* Resolved type of argumentes *)
    let args =
      indexedTypesToStringMap
        constraints
        conflicts
        funAnalysis.Summary.Parameters

    (* Resolved type of return register *)
    let returnRegs =
      returnRegTypesToStringMap
        constraints
        conflicts
        platform
        funAnalysis.Summary.RegisterOutputs

    (* Resolved type per instruction(SSA) *)
    let detailType =
      if includeDetailType then
        let resolvedTypes =
          ResolvedTypeMap.build constraints conflicts funAnalysis.TypeIndicators

        funAnalysis.Function.DFAResult.Statements
        |> Seq.map (fun entry -> entry.ProgramPoint, entry.Statement)
        |> TypePerInst.build resolvedTypes
      else
        TypePerInst.empty

    { Name = funAnalysis.Function.Name
      Arguments = { ArgNum = Map.count args; Args = args }
      ReturnReg = returnRegs
      DetailType = detailType }

module AnalysisResultJson =
  let private jsonOptions =
    JsonSerializerOptions (
      WriteIndented = true,
      Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  let private addressToHexString (address: Addr) = sprintf "0x%08x" address

  let fromAnalysisResult
    includeDetailType
    platform
    resultAnalysisResult
    targetFunctions
    : AnalysisResultJson =

    let func2JsonElem (address, funAnalysis) =
      let addrStr = addressToHexString address

      let funJson =
        FunctionJson.fromAnalysisResult
          includeDetailType
          platform
          resultAnalysisResult
          funAnalysis

      addrStr, funJson

    targetFunctions |> Map.toSeq |> Seq.map func2JsonElem |> Map.ofSeq

  let toJsonString (analysisResultJson: AnalysisResultJson) =
    JsonSerializer.Serialize (analysisResultJson, jsonOptions) + "\n"

  let fromAnalysisResultToJsonString
    includeDetailType
    platform
    resultAnalysisResult
    targetFunctions
    =
    fromAnalysisResult
      includeDetailType
      platform
      resultAnalysisResult
      targetFunctions
    |> toJsonString
