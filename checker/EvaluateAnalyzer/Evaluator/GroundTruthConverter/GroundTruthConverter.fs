module EvaluateAnalyzer.Evaluator.GroundTruthConverter.GroundTruthConverter

open System
open System.Text.Json
open EvaluateAnalyzer.Evaluator.Types

type SlotJson =
  { Index: int
    Size: int
    Type: string
    Path: string }

/// <summary>
/// Represents ABI-converted GT type per element.
/// </summary>
type ElementJson =
  { Size: int
    SourceType: string
    OccupiedSlotCount: int
    Slots: SlotJson list }

/// <summary>
/// Represents ABI-converted GT function signature.
/// </summary>
type FunctionJson =
  { Name: string
    Args: ElementJson list
    Return: ElementJson list }

/// <summary>
/// Represents entire ABI-converted GT function signatures.
/// </summary>
type ConvertedGroundTruthJson =
  { Platform: string
    WordSize: int
    Functions: Map<string, FunctionJson> }

let private evalTypeToString =
  function
  | Address -> "Address"
  | Value -> "Value"
  | Unknown -> "Unknown"
  | Conflict -> "Conflict"

let private sourceTypeToString =
  function
  | NormalElement -> "Normal"
  | StructureElement -> "Structure"

let private normalizePlatform (platform: string) =
  platform.Trim().ToLowerInvariant ()
  |> fun value -> value.Replace ("_", "-")
  |> fun value -> value.Replace (" ", "-")

/// Convert one RawGT function signautre to ABI-specific low-level GT
let convertFunction (config: AnalysisConfig) (raw: RawGTFunction) =
  match normalizePlatform config.Platform with
  | "elf-x86-32"
  | "elf-x86"
  | "elf-i386"
  | "elf-i586"
  | "x86"
  | "x86-32"
  | "i386"
  | "i586" -> ELF.X86_32.convert config.WordSize raw
  | _ ->
    failwithf "Unsupported ground-truth conversion platform: %s" config.Platform

/// Convert from RawGT function signature map to ABI-specific low-level GT map
let convert
  (config: AnalysisConfig)
  (rawFunctions: Map<string, RawGTFunction>)
  : Map<string, GTFunction> =
  rawFunctions |> Map.map (fun _ raw -> convertFunction config raw)

let private slotToJson (slot: GTSlot) : SlotJson =
  { Index = slot.Index
    Size = slot.Size
    Type = evalTypeToString slot.Type
    Path = slot.Path }

let private elementToJson (element: GTElement) : ElementJson =
  { Size = element.Size
    SourceType = sourceTypeToString element.Kind
    OccupiedSlotCount = element.OccupiedSlotCount
    Slots = element.Slots |> List.map slotToJson }

let private functionToJson (gt: GTFunction) : FunctionJson =
  { Name = gt.Function.Name
    Args = gt.Args |> List.map elementToJson
    Return = gt.Return |> List.map elementToJson }

let toJson (config: AnalysisConfig) (functions: Map<string, GTFunction>) =
  let output: ConvertedGroundTruthJson =
    { Platform = config.Platform
      WordSize = config.WordSize
      Functions = functions |> Map.map (fun _ gt -> functionToJson gt) }

  let options = JsonSerializerOptions (WriteIndented = true)
  JsonSerializer.Serialize (output, options) + Environment.NewLine
