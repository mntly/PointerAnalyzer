module PointerAnalyzer.Main

open System.IO
open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Interproc.ModularAnalyzer

let private constraintToString =
  function
  | Address typeId -> sprintf "Address(t%d)" typeId
  | Value typeId -> sprintf "Value(t%d)" typeId
  | Same typeIds ->
    typeIds
    |> Set.toList
    |> List.map (sprintf "t%d")
    |> String.concat ", "
    |> sprintf "Same({%s})"
  | AddResult (result, left, right) ->
    sprintf "AddResult(t%d, t%d, t%d)" result left right
  | SubResult (result, left, right) ->
    sprintf "SubResult(t%d, t%d, t%d)" result left right

let private typeToString constraints conflicts typeId =
  if Set.contains typeId conflicts then
    sprintf "Conflict(t%d)" typeId
  else
    let isAddress = Set.contains (Address typeId) constraints
    let isValue = Set.contains (Value typeId) constraints

    match isAddress, isValue with
    | true, false -> sprintf "Address(t%d)" typeId
    | false, true -> sprintf "Value(t%d)" typeId
    | false, false -> sprintf "Unknown(t%d)" typeId
    | true, true -> sprintf "Conflict(t%d)" typeId

let private formatProgramPoint (programPoint: ProgramPoint) =
  sprintf "0x%08x+%d" programPoint.Address programPoint.Position

let private dumpSSA (binaryPath: string) (program: ProgramDFAResult) =
  let outputDirectory =
    Path.Combine (Directory.GetCurrentDirectory (), "output")

  Directory.CreateDirectory outputDirectory |> ignore

  let binaryName = Path.GetFileName binaryPath
  let outputPath = Path.Combine (outputDirectory, binaryName + "_ssa")

  let lines =
    program.Functions
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
  outputPath

let private printFunctionAnalysis resultAnalysisResult address analysis =
  printfn ""
  printfn "Function 0x%x (%s)" address analysis.Function.Name
  printfn "  NextTypeId: t%d" analysis.Summary.NextTypeId

  printfn "  Parameters:"

  if Map.isEmpty analysis.Summary.Parameters then
    printfn "    <none detected>"
  else
    analysis.Summary.Parameters
    |> Map.iter (fun index typeId -> printfn "    arg%d -> t%d" index typeId)

  printfn "  Returns:"

  if Map.isEmpty analysis.Summary.Returns then
    printfn "    <none detected>"
  else
    analysis.Summary.Returns
    |> Map.iter (fun index typeId -> printfn "    ret%d -> t%d" index typeId)

  printfn "  SSA register types:"

  analysis.Result.FinalState.Types.TypeIndicators
  |> Map.iter (fun variable typeId ->
    let inferredType =
      typeToString
        resultAnalysisResult.TypeConstraints
        resultAnalysisResult.TypeConflicts
        typeId

    printfn "    %s -> %s" (Variable.ToString variable) inferredType)

  printfn "  Constraints:"

  if Set.isEmpty analysis.Result.TypeConstraints then
    printfn "    <empty>"
  else
    analysis.Result.TypeConstraints
    |> Set.iter (constraintToString >> printfn "    %s")

  printfn "  Conflicts:"

  if Set.isEmpty analysis.Result.TypeConflicts then
    printfn "    <empty>"
  else
    analysis.Result.TypeConflicts |> Set.iter (printfn "    t%d")

[<EntryPoint>]
let main argv =
  let validArguments =
    argv.Length = 1 || (argv.Length = 2 && argv[1] = "--dump-ssa")

  if not validArguments then
    eprintfn
      "Usage: dotnet run --project src/PointerAnalyzer.fsproj -- \
       <binary> [--dump-ssa]"

    1
  else
    try
      let binary = BinaryLoader.load argv[0]
      let shouldDumpSSA = argv.Length = 2

      printfn "Binary: %s" binary.Path
      printfn "ISA: %A" binary.Handle.File.ISA
      printfn "Platform: %s" binary.Platform.Name

      let program = ProgramRecovery.recover binary
      printfn "Recovered functions: %d" program.Functions.Count

      if shouldDumpSSA then
        let outputPath = dumpSSA binary.Path program
        printfn "SSA output: %s" outputPath

      let result = ModularAnalyzer.analyze program

      Map.iter (printFunctionAnalysis result) result.Functions

      printfn ""
      printfn "Whole-program constraints: %d" result.TypeConstraints.Count
      printfn "Whole-program conflicts: %d" result.TypeConflicts.Count
      printfn "Next global TypeId: t%d" result.NextTypeId
      0
    with ex ->
      eprintfn "Analysis failed: %s" ex.Message
      1
