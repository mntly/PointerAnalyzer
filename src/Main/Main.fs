module PointerAnalyzer.Main

open System
open System.Diagnostics
open System.Globalization
open System.IO
open Argu
open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Interproc.ModularAnalyzer

type FunctionSelector =
  | ByAddress of Addr
  | ByName of string

  member this.ToString =
    match this with
    | ByAddress address -> sprintf "0x%x" address
    | ByName name -> name

type MainOptions =
  { BinaryPath: string
    OutputDirPath: string
    IsStore: bool
    DumpSSA: bool
    ListFunctions: bool
    FunctionSelector: FunctionSelector option
    TrackTime: bool }

type CLIArg =
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-d")>] DumpSSA
  | [<AltCommandLine("-lf")>] ListFunctions
  | Function of string
  | Store of int
  | TrackTime of int

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Binary _ -> "Binary file to analyze."
      | Output _ ->
        "Output directory to store analysis results. Basically, the inferred type will be stored here."
      | Store _ ->
        "If 1 then store printed result (Dumped SSA, Listed Function) at output directory. If 0 then print out."
      | DumpSSA -> "Print recovered B2R2 SSA"
      | ListFunctions -> "Print recovered functions and exit before analysis."
      | Function _ ->
        "Print only the selected function. Accepts an address or exact function name."
      | TrackTime _ -> "Track and print out the processing time of each step."

let private tryParseAddress (text: string) =
  let parseHex (value: string) =
    match
      UInt64.TryParse (
        value,
        NumberStyles.HexNumber,
        CultureInfo.InvariantCulture
      )
    with
    | true, address -> Some address
    | false, _ -> None

  let parseDecimal (value: string) =
    match UInt64.TryParse value with
    | true, address -> Some address
    | false, _ -> None

  if text.StartsWith ("0x", StringComparison.OrdinalIgnoreCase) then
    parseHex text[2..]
  else
    parseDecimal text |> Option.orElseWith (fun () -> parseHex text)

let private parseArg (args: string array) =
  let parser =
    ArgumentParser.Create<CLIArg> (
      programName = "dotnet run --project src/PointerAnalyzer.fsproj --"
    )

  let r =
    try
      parser.Parse args
    with :? Argu.ArguParseException ->
      printfn "%s" (parser.PrintUsage ())
      exit 1

  let bin = r.GetResult <@ Binary @>
  let outDir = r.GetResult (<@ Output @>, defaultValue = "../../output")
  let isStore = r.GetResult (<@ Store @>, defaultValue = 0) = 1
  let dumpSSA = r.Contains DumpSSA
  let listFunctions = r.Contains ListFunctions
  let trackTime = r.GetResult (<@ TrackTime @>, defaultValue = 0) = 1

  let targetFunc =
    match r.TryGetResult Function with
    | Some tarFun ->
      match tryParseAddress tarFun with
      | Some address -> Some (ByAddress address)
      | None -> Some (ByName tarFun)
    | None -> None

  { BinaryPath = bin
    OutputDirPath = outDir
    IsStore = isStore
    DumpSSA = dumpSSA
    ListFunctions = listFunctions
    FunctionSelector = targetFunc
    TrackTime = trackTime }

let private formatElapsed (elapsed: TimeSpan) =
  sprintf "%.3fs" elapsed.TotalSeconds

let private timed trackTime label work =
  if trackTime then
    let stopwatch = Stopwatch.StartNew ()
    let result = work ()
    stopwatch.Stop ()
    printfn "[Time] %s: %s" label (formatElapsed stopwatch.Elapsed)
    result
  else
    work ()

let private resolveFunctionSelector
  (program: ProgramDFAResult)
  (selector: FunctionSelector)
  =
  match selector with
  | ByAddress address ->
    if Map.containsKey address program.Functions then
      Ok address
    else
      Error (sprintf "Function address not found: 0x%x" address)
  | ByName name ->
    let matches =
      program.Functions
      |> Map.toList
      |> List.filter (fun (_, function_) -> function_.Name = name)

    match matches with
    | [ address, _ ] -> Ok address
    | [] -> Error (sprintf "Function name not found: %s" name)
    | many ->
      let candidates =
        many
        |> List.map (fun (address, function_) ->
          sprintf "0x%x (%s)" address function_.Name)
        |> String.concat ", "

      Error (
        sprintf "Ambiguous function name: %s. Candidates: %s" name candidates
      )

let private formatProgramPoint (programPoint: ProgramPoint) =
  sprintf "0x%08x+%d" programPoint.Address programPoint.Position

let private dumpSSA (binaryPath: string) (suffix: string) targetFunctions =
  let outputDirectory =
    Path.Combine (Directory.GetCurrentDirectory (), "output")

  Directory.CreateDirectory outputDirectory |> ignore

  let binaryName = Path.GetFileName binaryPath

  let outputPath = Path.Combine (outputDirectory, binaryName + suffix)

  let lines =
    targetFunctions
    |> Map.toList
    |> List.collect (fun (address, function_) ->
      let header = [ sprintf "Function 0x%x (%s)" address function_.Name ]

      let statements =
        function_.DFAResult.Statements
        |> List.map (fun (programPoint, statement) ->
          sprintf
            "  %-20s %s"
            (formatProgramPoint programPoint)
            ((PrettyPrinter.ToString [| statement |]).Trim ()))

      header @ statements @ [ "" ])

  File.WriteAllLines (outputPath, lines)
  printfn "SSA output: %s" outputPath
  outputPath

[<EntryPoint>]
let main argv =
  let totalStopwatch = Stopwatch.StartNew ()
  let options = parseArg argv

  (* Load Given Binary *)
  let binary =
    timed options.TrackTime "Load binary" (fun () ->
      BinaryLoader.load options.BinaryPath)

  (* Print Binary Info *)
  printfn "Binary: %s" binary.Path
  printfn "ISA: %A" binary.Handle.File.ISA
  printfn "Platform: %s" binary.Platform.Name

  (* PreAnalysis Given Binary (B2R2 DFA)  *)
  let program =
    timed
      options.TrackTime
      "PreAnalyze binary (B2R2 DFA) and lift SSA"
      (fun () -> ProgramDFA.runDFA binary)

  (* Print PreAnalysis Result *)
  printfn "Recovered functions: %d" program.Functions.Count

  if options.ListFunctions then
    (* ListFunctions: Only print out the functions in given binary *)
    timed options.TrackTime "Print function list" (fun () ->
      program.PrintFuncList)

    totalStopwatch.Stop ()
    printfn "[Time] Total: %s" (formatElapsed totalStopwatch.Elapsed)
    0
  else
    let targetParsed =
      match options.FunctionSelector with
      | Some funSelector ->
        (* Analyze All, Print out only given function *)
        match resolveFunctionSelector program funSelector with
        | Ok addr ->
          printfn "Selected function: %s -> 0x%x" funSelector.ToString addr

          let suffix = sprintf "_0x%x_ssa" addr

          let extractFuncDFA allFunctions =
            match Map.tryFind addr allFunctions with
            | Some function_ -> Map.ofList [ addr, function_ ]
            | None -> Map.empty

          let extractFuncRes allFunctions =
            match Map.tryFind addr allFunctions with
            | Some function_ -> Map.ofList [ addr, function_ ]
            | None -> Map.empty

          Ok (suffix, extractFuncDFA, extractFuncRes)
        | Error reason -> Error reason
      | None ->
        (* Analyze All, Print out all functions *)
        printfn "Analyze all functions in given binary"

        let suffix = "_ssa"
        let extractFuncDFA allFunctions = allFunctions
        let extractFuncRes allFunctions = allFunctions

        Ok (suffix, extractFuncDFA, extractFuncRes)

    match targetParsed with
    | Ok (suffix, extractFuncDFA, extractFuncRes) ->
      (* Dump SSA *)
      if options.DumpSSA then
        let outputPath =
          timed options.TrackTime "Dump SSA" (fun () ->
            extractFuncDFA program.Functions |> dumpSSA binary.Path suffix)

        ()

      (* Run PointerAnalyzer *)
      let result =
        timed options.TrackTime "Analyze functions" (fun () ->
          ModularAnalyzer.analyze program)

      (* Extract only target function *)
      timed options.TrackTime "Print analysis result" (fun () ->
        extractFuncRes result.Functions
        |> Map.iter (ModularAnalyzer.printFunctionAnalysis result))

      (* Print out overall analysis result *)
      printfn ""
      printfn "Whole-program constraints: %d" result.TypeConstraints.Count
      printfn "Whole-program conflicts: %d" result.TypeConflicts.Count
      printfn "Next global TypeId: t%d" result.NextTypeId
      totalStopwatch.Stop ()
      printfn "[Time] Total: %s" (formatElapsed totalStopwatch.Elapsed)
      0

    | Error reason ->
      eprintfn "%s" reason
      1
