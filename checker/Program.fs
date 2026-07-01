module Checker.Program

open Argu
open System.IO

type CheckerMode =
  | FindCall0
  | FindCall0Invalid
  | EvalAnalyzer

type MainOptions =
  { Mode: CheckerMode
    BinaryPath: string
    OutputDirPath: string
    IsStore: bool }

type CLIArg =
  | [<AltCommandLine("-m")>] Mode of int
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-o")>] Output of string

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Mode _ ->
        "Mode 0 prints out the SSA jump/call instructions whose target is 0.
          This checks the functions in symbol table.
        Mode 1 prints out the SSA jump/call instructions whose target is 1.
          This checks the invalid functions of B2R2.
        Mode 2 prints out the evaluation result of PointerAnalyzer"
      | Binary _ -> "Binary file to inspect."
      | Output _ -> "Optional output file path. If omitted, print to stdout."

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
      EvalAnalyzer
    else
      eprintf "Unsupported mode %d" modeInt
      exit 1

  let bin = r.GetResult <@ Binary @>
  let isStore = r.Contains Output
  let outDir = if isStore then r.GetResult <@ Output @> else "output"

  { Mode = mode
    BinaryPath = bin
    OutputDirPath = outDir
    IsStore = isStore }

let private runFindCall0 options =
  let binPath = options.BinaryPath

  let result = FindCall0.run binPath |> FindCall0.toText
  emitOutput options "FindCall0Result" result

let private runFindCall0Invalid options =
  let binPath = options.BinaryPath

  let result = FindCall0Invalid.run binPath |> FindCall0Invalid.toText
  emitOutput options "FindCall0Result" result

let private runEvalAnalyzer options = eprintf "Not implemented"

[<EntryPoint>]
let main argv =
  let options = parseArg argv

  match options.Mode with
  | FindCall0 -> runFindCall0 options
  | FindCall0Invalid -> runFindCall0Invalid options
  | EvalAnalyzer -> runEvalAnalyzer options

  0
