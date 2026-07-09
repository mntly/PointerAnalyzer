module Checker.Program

open Argu
open System.IO

type CheckerMode =
  | FindCall0
  | FindCall0Invalid
  | BuildGroundTruth
  | EvalAnalyzer

type MainOptions =
  { Mode: CheckerMode
    BinaryPath: string option
    OutFileName: string
    OutputDirPath: string
    IsStore: bool }

type CLIArg =
  | [<AltCommandLine("-m")>] Mode of int
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-on")>] OutputName of string

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Mode _ ->
        "Mode 0 prints out the SSA jump/call instructions whose target is 0.
          This checks the functions in symbol table.
        Mode 1 prints out the SSA jump/call instructions whose target is 1.
          This checks the invalid functions of B2R2.
        Mode 2 extracts ground-truth signatures from DWARF debug info.
        Mode 3 prints out the evaluation result of PointerAnalyzer"
      | Binary _ ->
        "Binary file to inspect. For mode 2, this should bethe ground-truth binary with DWARF debug info."
      | Output _ ->
        "Optional output directory path. If omitted, print to stdout."

      | OutputName _ ->
        "Optional output file name. It is only used for storing. If omitted, file name is determined based on binary name."

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

  let outFileName =
    if r.Contains OutputName then
      r.GetResult <@ OutputName @>
    else
      match bin with
      | Some binName -> Path.GetFileName binName
      | None -> ""

  let isStore = r.Contains Output
  let outDir = if isStore then r.GetResult <@ Output @> else "output"

  { Mode = mode
    BinaryPath = bin
    OutFileName = outFileName
    OutputDirPath = outDir
    IsStore = isStore }

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
  let binPath = requireBinary options

  if options.IsStore then
    Directory.CreateDirectory options.OutputDirPath |> ignore

  let logFilePath =
    if options.IsStore then
      let logOptions: EvaluateAnalyzer.GroundTruthExtractor.Builder.BuildOptions =
        { GroundTruthBinary = binPath
          OutputSuffix = options.OutFileName
          LogFilePath = None }

      let logFileName =
        EvaluateAnalyzer.GroundTruthExtractor.Builder.logFileName logOptions

      Some (Path.Combine (options.OutputDirPath, logFileName))
    else
      None

  let buildOptions: EvaluateAnalyzer.GroundTruthExtractor.Builder.BuildOptions =
    { GroundTruthBinary = binPath
      OutputSuffix = options.OutFileName
      LogFilePath = logFilePath }

  try
    let result =
      EvaluateAnalyzer.GroundTruthExtractor.Builder.build buildOptions
      |> EvaluateAnalyzer.GroundTruthExtractor.Builder.toJson

    let fileName =
      EvaluateAnalyzer.GroundTruthExtractor.Builder.outputFileName buildOptions

    emitOutput options fileName result
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
