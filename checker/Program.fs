module Checker.Program

open Argu
open System.IO
open EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

type CheckerMode =
  | FindCall0
  | FindCall0Invalid
  | BuildGroundTruth
  | EvalAnalyzer

type MainOptions =
  { Mode: CheckerMode
    BinaryPath: string option
    OutputDirPath: string
    IsStore: bool
    GTExtractMode: GroundTruthExtractMode
    LibRoot: string }

type CLIArg =
  | [<AltCommandLine("-m")>] Mode of int
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-gm")>] GTExtractMode of int
  | LibRoot of string

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Mode _ ->
        "Mode 0 prints out the SSA jump/call instructions whose target is 0.
          This checks the functions in symbol table.
        Mode 1 prints out the SSA jump/call instructions whose target is 1.
          This checks the invalid functions of B2R2.
        Mode 2 extracts ground-truth signatures from uClibc.
        Mode 3 prints out the evaluation result of PointerAnalyzer"
      | Binary _ -> "Binary file to inspect."
      | Output _ -> "Optional output file path. If omitted, print to stdout."
      | GTExtractMode _ ->
        "Ground-truth extraction mode. 0 extracts functions in target binary.
          1 extracts all functions parsed from uClibc."
      | LibRoot _ -> "Optional uClibc source root path."

let private storeOutput options fileName (content: string) =
  let dirPath = options.OutputDirPath
  Directory.CreateDirectory dirPath |> ignore

  let outFilePath = Path.Combine (dirPath, fileName)
  File.WriteAllText (outFilePath, content)

  printfn "Result stored at %s" outFilePath

let private emitOutput options fileName (content: string) =
  if options.IsStore then
    storeOutput options fileName content
  else
    printf "%s" content

let private parseArg (args: string array) =
  let parser =
    ArgumentParser.Create<CLIArg> (
      programName = "dotnet run --project Checker.fsproj --"
    )

  let r =
    try
      parser.Parse args
    with :? Argu.ArguParseException ->
      printfn "%s" (parser.PrintUsage ())
      exit 1

  let modeInt = r.GetResult <@ Mode @>

  let mode =
    if modeInt = 0 then
      FindCall0
    else if modeInt = 1 then
      FindCall0Invalid
    else if modeInt = 2 then
      BuildGroundTruth
    else if modeInt = 3 then
      EvalAnalyzer
    else
      eprintf "Unsupported mode %d" modeInt
      exit 1

  let bin =
    if r.Contains Binary then
      Some (r.GetResult <@ Binary @>)
    else
      None

  let isStore = r.Contains Output
  let outDir = if isStore then r.GetResult <@ Output @> else "output"

  let gtMode =
    if r.Contains GTExtractMode then
      r.GetResult <@ GTExtractMode @> |> GroundTruthExtractMode.ofInt
    else
      TargetBinary

  let libRoot =
    if r.Contains LibRoot then
      r.GetResult <@ LibRoot @>
    else
      EvaluateAnalyzer.GroundTruthExtractor.UClibc.UClibcProfile.defaultLibRoot

  { Mode = mode
    BinaryPath = bin
    OutputDirPath = outDir
    IsStore = isStore
    GTExtractMode = gtMode
    LibRoot = libRoot }

let private requireBinary options =
  match options.BinaryPath with
  | Some path -> path
  | None ->
    eprintfn "Binary path is required for this mode."
    exit 1

let private runFindCall0 options =
  let binPath = requireBinary options

  let result = FindCall0.run binPath |> FindCall0.toText
  emitOutput options "FindCall0Result" result

let private runFindCall0Invalid options =
  let binPath = requireBinary options

  let result = FindCall0Invalid.run binPath |> FindCall0Invalid.toText
  emitOutput options "FindCall0InvalidResult" result

let private runBuildGroundTruth options =
  let buildOptions: EvaluateAnalyzer.GroundTruthExtractor.Builder.BuildOptions =
    { LibRoot = options.LibRoot
      ExtractMode = options.GTExtractMode
      TargetBinary = options.BinaryPath
      SourceProfile =
        EvaluateAnalyzer.GroundTruthExtractor.UClibc.UClibcProfile.profile
      TargetBinaryProfile =
        EvaluateAnalyzer.GroundTruthExtractor.ELF.ELFSymbolReader.profile }

  try
    let result =
      EvaluateAnalyzer.GroundTruthExtractor.Builder.build buildOptions
      |> EvaluateAnalyzer.GroundTruthExtractor.Builder.toJson

    emitOutput options "groundTruth.json" result
  with ex ->
    eprintfn "%s" ex.Message
    exit 1

let private runEvalAnalyzer options = eprintf "Not implemented"

[<EntryPoint>]
let main argv =
  let options = parseArg argv

  match options.Mode with
  | FindCall0 -> runFindCall0 options
  | FindCall0Invalid -> runFindCall0Invalid options
  | BuildGroundTruth -> runBuildGroundTruth options
  | EvalAnalyzer -> runEvalAnalyzer options

  0
