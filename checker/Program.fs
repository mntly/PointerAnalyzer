module Checker.Program

open Argu
open System.IO

/// <summary>
/// Checker mode for evaluating <see cref="PointerAnalyzer" /> or testing
/// <see cref="B2R2" />.
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
/// <c>EvalAnalyzer</c> mode evaluates <see cref="PointerAnalyzer" /> using the
/// result of <see cref="PointerAnalyzer" /> and GT from BuildGroundTruth.
/// <c>DetectReturn64</c> mode detects the functions return 64 bit value.
/// <c>EvalReturn64</c> mode evaluates Return64Detector using GT.
/// </remarks>
type CheckerMode =
  | FindCall0
  | FindCall0Invalid
  | BuildGroundTruth
  | EvalAnalyzer
  | DetectReturn64
  | EvalReturn64

/// <summary>
/// Options used for propagating given input to each mode.
/// </summary>
type MainOptions =
  { Mode: CheckerMode
    BinaryPath: string option
    GroundTruthPath: string option
    InferredPath: string option
    WhiteListPath: string option
    OutFileName: string
    OutputDirPath: string
    IsStore: bool
    Return64Range: Checker.Return64Detection.Return64Types.AnalysisRange
    Return64Heuristic:
      Checker.Return64Detection.Return64Types.DetectionHeuristic }

type CLIArg =
  | [<AltCommandLine("-m")>] Mode of int
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-gt")>] GroundTruth of string
  | [<AltCommandLine("-i")>] Inferred of string
  | [<AltCommandLine("-wl")>] WhiteList of string
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-on")>] OutputName of string
  | [<AltCommandLine("-rr")>] ReturnRange of int
  | [<AltCommandLine("-rh")>] ReturnHeuristic of int

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Mode _ ->
        "Mode 0 prints out the SSA jump/call instructions whose target is 0.
          This checks the functions in symbol table.
        Mode 1 prints out the SSA jump/call instructions whose target is 1.
          This checks the invalid functions of B2R2.
        Mode 2 extracts ground-truth signatures from DWARF debug info.
        Mode 3 prints out the evaluation result of PointerAnalyzer.
        Mode 4 detects functions that return 64 bit values in EDX:EAX.
        Mode 5 evaluates Return64Detector using ground-truth return sizes."
      | Binary _ ->
        "Binary file to inspect. For mode 2, this should be the ground-truth binary with DWARF debug info."
      | GroundTruth _ ->
        "Ground-truth JSON file. This is required for modes 3 and 5."
      | Inferred _ ->
        "PointerAnalyzer inferredTypes.json file. This is required for mode 3."
      | WhiteList _ ->
        "Optional function-name whitelist for mode 2. The file contains one function name per line."
      | Output _ ->
        "Optional output directory path. If omitted, print to stdout."
      | OutputName _ ->
        "Optional output file name. It is only used for storing. If omitted, file name is determined based on binary name."
      | ReturnRange _ ->
        "Return64 range: 0 = leaf and direct predecessors; 1 = entire function."
      | ReturnHeuristic _ ->
        "Return64 heuristic: 0 = basic; 1 = basic with caller checker."

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
    else if modeInt = 4 then
      DetectReturn64
    else if modeInt = 5 then
      EvalReturn64
    else
      eprintf "Unsupported mode %d" modeInt
      exit 1

  (* Extract input paths *)
  let bin =
    if r.Contains Binary then
      Some (r.GetResult <@ Binary @>)
    else
      None

  let gt =
    if r.Contains GroundTruth then
      Some (r.GetResult <@ GroundTruth @>)
    else
      None

  let inferred =
    if r.Contains Inferred then
      Some (r.GetResult <@ Inferred @>)
    else
      None

  let whiteList =
    if r.Contains WhiteList then
      Some (r.GetResult <@ WhiteList @>)
    else
      None

  (* Extract name of output file *)
  let outFileName =
    if r.Contains OutputName then
      r.GetResult <@ OutputName @>
    else
      match bin, gt, inferred with
      | Some path, _, _ -> Path.GetFileName path
      | _, Some path, _ -> Path.GetFileNameWithoutExtension path
      | _, _, Some path -> Path.GetFileNameWithoutExtension path
      | _ -> "result"

  (* Extract how to emit output; print or store *)
  let isStore = r.Contains Output

  (* Extract the path of output directory. Default as `output` *)
  let outDir = if isStore then r.GetResult <@ Output @> else "output"

  (* Extract the type of AnalysisRange for Return64Detector *)
  let return64Range =
    match r.GetResult (<@ ReturnRange @>, defaultValue = 0) with
    | 0 -> Checker.Return64Detection.Return64Types.LeafAndDirectPredecessors
    | 1 -> Checker.Return64Detection.Return64Types.EntireFunction
    | value ->
      eprintfn "Unsupported Return64Detector range %d. Use 0 or 1." value
      exit 1

  (* Extract the type of Heuristics for Return64Detector *)
  let return64Heuristic =
    match r.GetResult (<@ ReturnHeuristic @>, defaultValue = 0) with
    | 0 -> Checker.Return64Detection.Return64Types.Basic
    | 1 -> Checker.Return64Detection.Return64Types.BasicWithCallerChecker
    | value ->
      eprintfn "Unsupported Return64Detector heuristic %d. Use 0 or 1." value
      exit 1

  { Mode = mode
    BinaryPath = bin
    GroundTruthPath = gt
    InferredPath = inferred
    WhiteListPath = whiteList
    OutFileName = outFileName
    OutputDirPath = outDir
    IsStore = isStore
    Return64Range = return64Range
    Return64Heuristic = return64Heuristic }

/// If binary path are not given, halt
let private requireBinary options =
  match options.BinaryPath with
  | Some path -> path
  | None ->
    eprintfn "Binary path is required. Use -b <binary>."
    exit 1

/// If ground truth path are not given, halt
let private requireGroundTruth options =
  match options.GroundTruthPath with
  | Some path -> path
  | None ->
    eprintfn "Ground-truth JSON path is required. Use -gt <ground-truth-json>."
    exit 1

/// If inferred result from PointerAnalyzer path are not given, halt
let private requireInferred options =
  match options.InferredPath with
  | Some path -> path
  | None ->
    eprintfn
      "Inferred result JSON path is required. Use -i <inferredTypes.json>."

    exit 1

/// Execute FindCall0 mode
let private runFindCall0 options =
  let result = FindCall0.run (requireBinary options) |> FindCall0.toText
  emitOutput options "FindCall0Result" result

/// Execute FindCall0Invalid mode
let private runFindCall0Invalid options =
  let result =
    FindCall0Invalid.run (requireBinary options) |> FindCall0Invalid.toText

  emitOutput options "FindCall0InvalidResult" result

/// Execute GroundTruth Extractor
let private runBuildGroundTruth options =
  let binaryPath = requireBinary options

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
    { GroundTruthBinary = binaryPath
      OutputSuffix = options.OutFileName
      LogFilePath = logFilePath
      WhiteListPath = options.WhiteListPath }

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

/// Execute PointerAnalyzer evaluator
let private runEvalAnalyzer options =
  let evalOptions: EvaluateAnalyzer.Evaluator.Evaluator.EvalOptions =
    { GroundTruthJsonPath = requireGroundTruth options
      InferredJsonPath = requireInferred options }

  try
    (* Execute PointerAnalyzer evaluator *)
    let result = EvaluateAnalyzer.Evaluator.Evaluator.run evalOptions

    (* Construct low-level GT Function Signatures based on the target ABI *)
    let convertedGTFileName =
      EvaluateAnalyzer.Evaluator.Evaluator.convertedGroundTruthFileName
        options.OutFileName

    (* Construct evaluate result path *)
    let jsonFileName =
      EvaluateAnalyzer.Evaluator.Evaluator.evalResultJsonFileName
        options.OutFileName

    (* Construct path of log during evaluator *)
    let logFileName =
      EvaluateAnalyzer.Evaluator.Evaluator.evalResultLogFileName
        options.OutFileName

    (* Store result *)
    emitOutput options convertedGTFileName result.ConvertedGroundTruth
    printfn ""
    emitOutput options jsonFileName result.Json
    printfn ""
    emitOutput options logFileName result.Log

  with ex ->
    eprintfn "%s" ex.Message
    exit 1

/// Execute x86-32 EDX:EAX 64 bit return detector.
let private runReturn64Detector options =
  let detectOptions: Return64Detection.Return64Detector.DetectOptions =
    { BinaryPath = requireBinary options
      Range = options.Return64Range
      Heuristic = options.Return64Heuristic }

  try
    let result =
      Return64Detection.Return64Detector.run detectOptions
      |> Return64Detection.Return64Formatter.toText

    emitOutput options "Return64Result" result
  with ex ->
    eprintfn "%s" ex.Message
    exit 1

/// Execute and evaluate x86-32 EDX:EAX 64 bit return detector.
let private runReturn64Evaluator options =
  let evalOptions: Return64Detection.Evaluator.Evaluator.EvalOptions =
    { BinaryPath = requireBinary options
      GroundTruthPath = requireGroundTruth options
      Range = options.Return64Range
      Heuristic = options.Return64Heuristic }

  try
    let result = Return64Detection.Evaluator.Evaluator.run evalOptions

    let detectorFileName =
      Return64Detection.Evaluator.Evaluator.detectorResultFileName
        options.OutFileName

    let convertedGTFileName =
      Return64Detection.Evaluator.Evaluator.convertedGroundTruthFileName
        options.OutFileName

    let jsonFileName =
      Return64Detection.Evaluator.Evaluator.evalResultJsonFileName
        options.OutFileName

    let logFileName =
      Return64Detection.Evaluator.Evaluator.evalResultLogFileName
        options.OutFileName

    emitOutput options detectorFileName result.Detector
    printfn ""
    emitOutput options convertedGTFileName result.ConvertedGroundTruth
    printfn ""
    emitOutput options jsonFileName result.Json
    printfn ""
    emitOutput options logFileName result.Log
  with ex ->
    eprintfn "%s" ex.Message
    exit 1

[<EntryPoint>]
let main argv =
  let options = parseArg argv

  match options.Mode with
  | FindCall0 -> runFindCall0 options
  | FindCall0Invalid -> runFindCall0Invalid options
  | BuildGroundTruth -> runBuildGroundTruth options
  | EvalAnalyzer -> runEvalAnalyzer options
  | DetectReturn64 -> runReturn64Detector options
  | EvalReturn64 -> runReturn64Evaluator options

  0
