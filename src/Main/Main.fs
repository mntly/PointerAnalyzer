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
    DumpConstraints: bool
    ListFunctions: bool
    FunctionSelector: FunctionSelector option
    TrackTime: bool }

type CLIArg =
  | [<AltCommandLine("-b")>] Binary of string
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-d")>] DumpSSA
  | [<AltCommandLine("-dc")>] DumpConstraints
  | [<AltCommandLine("-lf")>] ListFunctions
  | [<AltCommandLine("-s")>] Store of int
  | [<AltCommandLine("-t")>] TrackTime
  | Function of string

  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Binary _ -> "Binary file to analyze."
      | Output _ ->
        "Output directory to store analysis results. Basically, the inferred type will be stored here."
      | Store _ ->
        "If 1 then store printed result (Dumped SSA, Listed Function) at output directory. If 0 then print out."
      | DumpSSA -> "Print recovered B2R2 SSA"
      | DumpConstraints ->
        "Print/Store the human-readable type constraints and type IDs."
      | ListFunctions -> "Print recovered functions and exit before analysis."
      | Function _ ->
        "Print only the selected function. Accepts an address or exact function name."
      | TrackTime -> "Track and print out the processing time of each step."

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
  let outDir = r.GetResult <@ Output @>
  let isStore = r.GetResult (<@ Store @>, defaultValue = 0) = 1
  let dumpSSA = r.Contains DumpSSA
  let dumpConstraints = r.Contains DumpConstraints
  let listFunctions = r.Contains ListFunctions
  let trackTime = r.Contains TrackTime

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
    DumpConstraints = dumpConstraints
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

let private outputDirectory options =
  Path.Combine (options.OutputDirPath, Path.GetFileName options.BinaryPath)

let private emitOutput
  (options: MainOptions)
  (fileName: string)
  (content: string)
  =
  if options.IsStore then
    let directory = outputDirectory options
    Directory.CreateDirectory directory |> ignore
    let outputPath = Path.Combine (directory, fileName)
    File.WriteAllText (outputPath, content)
    printfn "%s output: %s" fileName outputPath
  else
    printf "%s" content

let private storeOutput
  (options: MainOptions)
  (fileName: string)
  (content: string)
  =
  let directory = outputDirectory options
  Directory.CreateDirectory directory |> ignore
  let outputPath = Path.Combine (directory, fileName)
  File.WriteAllText (outputPath, content)
  printfn "%s output: %s" fileName outputPath

let private formatFunctionList (program: ProgramDFAResult) =
  let header = "Recovered functions:"

  let content =
    if Map.isEmpty program.Functions then
      [ "  <empty>" ]
    else
      program.Functions
      |> Map.toList
      |> List.map (fun (address, function_) ->
        sprintf "  0x%08x  %s" address function_.Name)

  header :: content |> String.concat "\n" |> (fun text -> text + "\n")

let private formatSSA targetFunctions =
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
  |> String.concat "\n"
  |> fun text -> text + "\n"

let private formatConstraintSet constraints =
  let content =
    if Set.isEmpty constraints then
      [ "  <empty>" ]
    else
      constraints
      |> Set.toList
      |> List.map (TypeConstraint.toString >> sprintf "  %s")

  "Constraints" :: content |> String.concat "\n"

let private formatConflictSet conflicts =
  let content =
    if Set.isEmpty conflicts then
      [ "  <empty>" ]
    else
      conflicts |> Set.toList |> List.map (sprintf "  t%d")

  "Conflicts" :: content |> String.concat "\n"

let private formatAnalysisResult result targetFunctions =
  let functionResults =
    targetFunctions
    |> Map.toList
    |> List.map (fun (address, analysis) ->
      ModularAnalyzer.functionAnalysisToString result address analysis)
    |> String.concat "\n\n"

  let wholeProgram =
    [ "---"
      formatConstraintSet result.TypeConstraints
      ""
      formatConflictSet result.TypeConflicts
      sprintf "Next global TypeId: t%d" result.NextTypeId ]
    |> String.concat "\n"

  functionResults + "\n" + wholeProgram + "\n"

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
      formatFunctionList program |> emitOutput options "funcList")

    totalStopwatch.Stop ()

    if options.TrackTime then
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

          let extractFuncDFA allFunctions =
            match Map.tryFind addr allFunctions with
            | Some function_ -> Map.ofList [ addr, function_ ]
            | None -> Map.empty

          let extractFuncRes allFunctions =
            match Map.tryFind addr allFunctions with
            | Some function_ -> Map.ofList [ addr, function_ ]
            | None -> Map.empty

          Ok (extractFuncDFA, extractFuncRes)
        | Error reason -> Error reason
      | None ->
        (* Analyze All, Print out all functions *)
        printfn "Analyze all functions in given binary"

        let extractFuncDFA allFunctions = allFunctions
        let extractFuncRes allFunctions = allFunctions

        Ok (extractFuncDFA, extractFuncRes)

    match targetParsed with
    | Ok (extractFuncDFA, extractFuncRes) ->
      (* Dump SSA *)
      if options.DumpSSA then
        timed options.TrackTime "Dump SSA" (fun () ->
          extractFuncDFA program.Functions
          |> formatSSA
          |> emitOutput options "dumpedSSA")

      (* Run PointerAnalyzer *)
      let result =
        timed options.TrackTime "Analyze functions" (fun () ->
          ModularAnalyzer.analyzeWithTimer options.TrackTime program)

      (* Extract only target function *)
      let selectedResults = extractFuncRes result.Functions

      timed options.TrackTime "Print analysis result" (fun () ->
        selectedResults
        |> Result2Json.AnalysisResultJson.fromAnalysisResultToJsonString result
        |> storeOutput options "inferredTypes.json")

      if options.DumpConstraints then
        timed options.TrackTime "Print type constraints" (fun () ->
          selectedResults
          |> formatAnalysisResult result
          |> emitOutput options "typeConstraints")

      totalStopwatch.Stop ()

      if options.TrackTime then
        printfn "[Time] Total: %s" (formatElapsed totalStopwatch.Elapsed)

      0

    | Error reason ->
      eprintfn "%s" reason
      1
