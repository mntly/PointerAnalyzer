module Checker.Program

open Argu
open System.IO

/// <summary>
/// Checker mode for evaluating <see cref="PointerAnalyzer" />.
/// </summary>
/// <remarks>
/// <c>FindCall0</c> mode checks call 0 instructions from lifted
/// <see cref="B2R2.BinIR.SSA" /> of the functions in given binary.
/// <c>FindCall0Invalid</c> mode checks call 0 instructions from lifted
/// <see cref="B2R2.BinIR.SSA" /> of the functions detected by
/// <see cref="B2R2" />. In addition, it prints out which function is marked as
/// Valid or Invalid by <see cref="B2R2" />.
/// <c>BuildGroundTruth</c> mode extracts type signature of given binary by
/// parsing DWARF information. If given binary was not compiled with debug
/// option, it will fail.
/// <c>EvalAnalyzer</c> mode ToDo.
/// </remarks>
type CheckerMode =
  | FindCall0
  | FindCall0Invalid
  | BuildGroundTruth
  | EvalAnalyzer

/// <summary>
/// Options used for propagating given input to each mode.
/// </summary>
type MainOptions =
  { Mode: CheckerMode
    BinaryPath: string
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
        "Binary file to inspect. For mode 2, this should be the ground-truth binary with DWARF debug info."
      | Output _ ->
        "Optional output directory path. If omitted, print to stdout."
      | OutputName _ ->
        "Optional output file name. It is only used for storing. If omitted, file name is determined based on binary name."

/// Store given content to file with given file name
let private storeOutput options fileName (content: string) =
  let dirPath = options.OutputDirPath
  Directory.CreateDirectory dirPath |> ignore

  let outFilePath = Path.Combine (dirPath, fileName)
  File.WriteAllText (outFilePath, content)

  printfn "Result stored at %s" outFilePath

/// Based on option, print or store given content
let private emitOutput options fileName (content: string) =
  if options.IsStore then
    storeOutput options fileName content
  else
    printf "%s" content

/// Parse given arguments and construct ManiOptions
let private parseArg (args: string array) =
  let parser =
    ArgumentParser.Create<CLIArg> (
      programName = "dotnet run --project Checker.fsproj --"
    )

  (* Check whether valid options come *)
  let r =
    try
      parser.Parse args
    with :? Argu.ArguParseException ->
      printfn "%s" (parser.PrintUsage ())
      exit 1

  (* Extract mode *)
  let modeInt = r.GetResult <@ Mode @>

  (* Normalize mode *)
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

  (* Extract binary path *)
  let bin = r.GetResult <@ Binary @>

  (* Extract name of output file *)
  let outFileName =
    if r.Contains OutputName then
      r.GetResult <@ OutputName @>
    else
      Path.GetFileName bin

  (* Extract how to emit output; print or store *)
  let isStore = r.Contains Output

  (* Extract the path of output directory. Default as `output` *)
  let outDir = if isStore then r.GetResult <@ Output @> else "output"

  { Mode = mode
    BinaryPath = bin
    OutFileName = outFileName
    OutputDirPath = outDir
    IsStore = isStore }

/// Execute FindCall0 mode
let private runFindCall0 options =
  let result = FindCall0.run options.BinaryPath |> FindCall0.toText
  emitOutput options "FindCall0Result" result

/// Execute FindCall0Invalid mode
let private runFindCall0Invalid options =
  let result =
    FindCall0Invalid.run options.BinaryPath |> FindCall0Invalid.toText

  emitOutput options "FindCall0InvalidResult" result

/// Execute GroundTruth Extractor
let private runBuildGroundTruth options =
  (* Generate output directory for storing log file *)
  if options.IsStore then
    Directory.CreateDirectory options.OutputDirPath |> ignore

  (* Combine path of log file *)
  let logFilePath =
    if options.IsStore then
      let logFileName =
        EvaluateAnalyzer.GroundTruthExtractor.Builder.logFileName
          options.OutFileName

      Some (Path.Combine (options.OutputDirPath, logFileName))
    else
      None

  let buildOptions: EvaluateAnalyzer.GroundTruthExtractor.Builder.BuildOptions =
    { GroundTruthBinary = options.BinaryPath
      OutputSuffix = options.OutFileName
      LogFilePath = logFilePath }

  (* Execute GroundTruthExtractor*)
  try
    let result =
      EvaluateAnalyzer.GroundTruthExtractor.Builder.build buildOptions
      |> EvaluateAnalyzer.GroundTruthExtractor.Builder.toJson

    let fileName =
      EvaluateAnalyzer.GroundTruthExtractor.Builder.outputFileName
        options.OutFileName

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
