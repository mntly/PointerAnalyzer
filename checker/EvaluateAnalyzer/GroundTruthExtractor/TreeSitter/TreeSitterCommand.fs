module EvaluateAnalyzer.GroundTruthExtractor.TreeSitter.TreeSitterCommand

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open EvaluateAnalyzer.GroundTruthExtractor.C.CSignatureParser
open EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile
open EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// Python code for extracting GT type information using tree-sitter
let private scriptPath =
  Path.Combine (__SOURCE_DIRECTORY__, "extract_c_facts.py")

/// Used for executing python script
let private runProcess fileName args =
  (*
    ToDo
      Modify to immutable style!!!!!!!!!!!!!!!!!!!!!!!!!!!
  *)
  let psi = ProcessStartInfo ()
  psi.FileName <- fileName
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true

  args |> List.iter (fun arg -> psi.ArgumentList.Add arg)

  use proc = new Process ()
  proc.StartInfo <- psi

  try
    proc.Start () |> ignore
  with :? ComponentModel.Win32Exception as ex ->
    failwithf
      "python3 is not installed or is not available in PATH. Detail: %s"
      ex.Message

  let stdout = proc.StandardOutput.ReadToEnd ()
  let stderr = proc.StandardError.ReadToEnd ()
  proc.WaitForExit ()

  proc.ExitCode, stdout, stderr

let private tryProperty (name: string) (element: JsonElement) =
  match element.TryGetProperty name with
  | true, value -> Some value
  | false, _ -> None

let private stringProperty (name: string) (element: JsonElement) =
  match tryProperty name element with
  | Some value when value.ValueKind = JsonValueKind.String -> value.GetString ()
  | _ -> ""

let private arrayProperty (name: string) (element: JsonElement) =
  match tryProperty name element with
  | Some value when value.ValueKind = JsonValueKind.Array ->
    value.EnumerateArray () |> Seq.toList
  | _ -> []

let private parseParameter (element: JsonElement) =
  { Name = stringProperty "name" element
    CType = stringProperty "ctype" element }

let private parseSignature (element: JsonElement) =
  { Name = stringProperty "name" element
    Source = stringProperty "source" element
    Prototype = stringProperty "prototype" element
    ReturnCType = stringProperty "returnCType" element
    Parameters = arrayProperty "parameters" element |> List.map parseParameter }

let private parseAlias (element: JsonElement) =
  { Alias = stringProperty "alias" element
    CanonicalName = stringProperty "canonicalName" element }

let private parseFacts (stdout: string) =
  use doc = JsonDocument.Parse stdout
  let root = doc.RootElement

  { Signatures = arrayProperty "signatures" root |> List.map parseSignature
    Aliases = arrayProperty "aliases" root |> List.map parseAlias }

/// Extract ground type information of given library code.
/// This calls python script to utilize tree-sitter for parsing codes.
let extractFacts libRoot sourcePath =
  if not (File.Exists scriptPath) then
    failwithf "Tree-sitter extractor script does not exist: %s" scriptPath

  (* Execute python script *)
  (* Python script must produce Json format string *)
  let args = [ scriptPath; "--root"; libRoot; "--source"; sourcePath ]

  let exitCode, stdout, stderr = runProcess "python3" args

  (* Check python script is executed well *)
  if exitCode <> 0 then
    failwithf
      "Tree-sitter extraction failed for %s with exit code %d:\n%s"
      sourcePath
      exitCode
      stderr

  (* Post-processing the result of tree-sitter *)
  try
    parseFacts stdout
  with :? JsonException as ex ->
    failwithf
      "Tree-sitter extractor returned invalid JSON for %s: %s"
      sourcePath
      ex.Message
