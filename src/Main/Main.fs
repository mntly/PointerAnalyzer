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
open PointerAnalyzer.Utils

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

(*
  Filter only given target function.
  Only single target function is valid.
*)
let private resolveFunctionSelector (program: ProgramDFAResult) selector =
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
    | [] -> Error (sprintf "Function name not found: %s" name)
    | [ address, _dfaRet ] -> Ok address
    | targetFuncs ->
      let candidates =
        targetFuncs
        |> List.map (fun (address, function_) ->
          sprintf "0x%x (%s)" address function_.Name)
        |> String.concat ", "

      Error (
        sprintf "Ambiguous function name: %s. Candidates: %s" name candidates
      )

let private storeOutput options fileName (content: string) =
  let dirPath = options.OutputDirPath
  let binName = Path.GetFileName options.BinaryPath
  let outDirPath = Path.Combine (dirPath, binName)
  Directory.CreateDirectory outDirPath |> ignore

  let outFilePath = Path.Combine (outDirPath, fileName)
  File.WriteAllText (outFilePath, content)

  printfn "%s output: %s" fileName outFilePath

let private emitOutput options fileName (content: string) =
  if options.IsStore then
    storeOutput options fileName content
  else
    printf "%s" content

let private functionListStr (program: ProgramDFAResult) =
  let addrFun2Str (addr, func) = sprintf "  0x%08x  %s" addr func.Name

  let header = "Recovered functions:"

  let content =
    if Map.isEmpty program.Functions then
      [ "  <empty>" ]
    else
      program.Functions |> Map.toList |> List.map addrFun2Str

  let funLstStr = header :: content |> String.concat "\n"
  funLstStr + "\n"

let private funcSSAStr targetFunctions =
  let ppStr (programPoint: ProgramPoint) =
    sprintf "0x%08x+%d" programPoint.Address programPoint.Position

  let stmtStr (pp, stmt: Stmt) =
    sprintf
      "  %-20s %s"
      (ppStr pp)
      ((PrettyPrinter.ToString [| stmt |]).Trim ())

  let funcSSA2Str (addr, func) =
    let header = sprintf "Function 0x%x (%s)" addr func.Name
    let statements = func.DFAResult.Statements |> List.map stmtStr
    header :: statements @ [ "" ]

  let ssaStr =
    targetFunctions
    |> Map.toList
    |> List.collect funcSSA2Str
    |> String.concat "\n"

  ssaStr + "\n"

let private constraintSetStr constraints =
  let header = "Constraints"

  let content =
    if Set.isEmpty constraints then
      [ "  <empty>" ]
    else
      constraints
      |> Set.toList
      |> List.map (TypeConstraint.toString >> sprintf "  %s")

  header :: content |> String.concat "\n"

let private conflictSetStr conflicts =
  let header = "Conflicts"

  let content =
    if Set.isEmpty conflicts then
      [ "  <empty>" ]
    else
      conflicts |> Set.toList |> List.map (sprintf "  t%d")

  header :: content |> String.concat "\n"

let private analysisResultStr result targetFunctions =
  let functionResults =
    targetFunctions
    |> Map.toList
    |> List.map (fun (address, analysis) ->
      ModularAnalyzer.functionAnalysisToString result address analysis)
    |> String.concat "\n\n"

  let typeState =
    [ "------------------------------"
      constraintSetStr result.TypeConstraints
      ""
      conflictSetStr result.TypeConflicts ]
    |> String.concat "\n"

  functionResults + "\n" + typeState + "\n"

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
      functionListStr program |> emitOutput options "funcList")

    totalStopwatch.Stop ()

    if options.TrackTime then
      printfn "[Time] Total: %s" (formatElapsed totalStopwatch.Elapsed)

    0
  else
    (* Filtering to print/store only given function *)
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
        let selectedSSA = extractFuncDFA program.Functions

        timed options.TrackTime "Dump SSA" (fun () ->
          selectedSSA |> funcSSAStr |> emitOutput options "dumpedSSA")

      (* Run PointerAnalyzer *)
      let result = ModularAnalyzer.analyzeWithTimer options.TrackTime program

      (* Extract only targeted function *)
      let selectedResults = extractFuncRes result.Functions

      timed options.TrackTime "Print analysis result" (fun () ->
        selectedResults
        |> Result2Json.AnalysisResultJson.fromAnalysisResultToJsonString result
        |> storeOutput options "inferredTypes.json")

      (* Dump type constraints collected during entire analysis process *)
      if options.DumpConstraints then
        timed options.TrackTime "Print type constraints" (fun () ->
          selectedResults
          |> analysisResultStr result
          |> emitOutput options "typeConstraints")

      totalStopwatch.Stop ()

      if options.TrackTime then
        printfn "[Time] Total: %s" (formatElapsed totalStopwatch.Elapsed)

      0

    | Error reason ->
      eprintfn "%s" reason
      1
