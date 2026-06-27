module PointerAnalyzer.Interproc.ModularAnalyzer

open System
open System.Diagnostics
open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.AbsDom.TypeState
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Analysis.StmtEval
open PointerAnalyzer.Frontend.ConstantClassifier
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Summary
open PointerAnalyzer.Summary.FunctionSummaryBuilder
open PointerAnalyzer.Summary.SummaryApplicator
open PointerAnalyzer.TypeInference.ResolvedType

type FunctionAnalysisResult =
  { Function: FunctionDFAResult
    Result: AnalysisResult
    Summary: FunctionSummary }

type ModularAnalysisResult =
  { Functions: Map<Addr, FunctionAnalysisResult>
    Summaries: Map<Addr, FunctionSummary>
    TypeConstraints: ConstraintSet
    TypeConflicts: Set<TypeId>
    NextTypeId: TypeId }

module ModularAnalyzer =
  let functionAnalysisToString
    resultAnalysisResult
    (address: Addr)
    funAnalysis
    =
    let resolvedTypes =
      ResolvedTypeMap.build
        resultAnalysisResult.TypeConstraints
        resultAnalysisResult.TypeConflicts
        funAnalysis.Result.FinalState.Types.TypeIndicators

    let registerTypes =
      resolvedTypes
      |> Map.toSeq
      |> Seq.map (fun (variable, typeInfo) ->
        sprintf
          "    %s -> %s"
          (Variable.ToString variable)
          (ResolvedTypeInfo.toDebugString typeInfo))
      |> String.concat "\n"

    [ sprintf "Function 0x%x (%s)" address funAnalysis.Function.Name
      sprintf "  NextTypeId: t%d" funAnalysis.Summary.NextTypeId
      funAnalysis.Summary.ParamToString.TrimEnd ()
      funAnalysis.Summary.ReturnToString.TrimEnd ()
      "  SSA register types:"
      if registerTypes = "" then "    <empty>" else registerTypes ]
    |> String.concat "\n"

  let printFunctionAnalysis resultAnalysisResult address analysis =
    printfn
      "%s"
      (functionAnalysisToString resultAnalysisResult address analysis)

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

  let private internalCallees
    (functions: Map<Addr, FunctionDFAResult>)
    function_
    =
    let calleeSeq =
      function_.Callees |> Map.toSeq |> Seq.collect (snd >> Set.toSeq)

    let inFunctionsCallees =
      calleeSeq
      |> Seq.filter (fun address -> Map.containsKey address functions)
      |> Set.ofSeq

    inFunctionsCallees

  let private revDFS functions =
    let rec dfs address (visited, visitOrder) =
      if Set.contains address visited then
        visited, visitOrder
      else
        let newVisited = Set.add address visited
        let function_ = Map.find address functions

        let calleeSet = internalCallees functions function_

        let visited, visitOrder =
          Set.fold
            (fun acc callee -> dfs callee acc)
            (newVisited, visitOrder)
            calleeSet

        visited, address :: visitOrder

    let funcAddrSeq = functions |> Map.toSeq |> Seq.map fst

    funcAddrSeq
    |> Seq.fold (fun acc address -> dfs address acc) (Set.empty, [])
    |> snd
    |> List.rev

  let private trySingleCallee function_ callSite =
    let callee = Map.tryFind callSite function_.Callees

    match callee with
    | Some calleeSet when Set.count calleeSet = 1 ->
      Some (Set.minElement calleeSet)
    | _ -> None

  let analyzeWithTimer trackTime (program: ProgramDFAResult) =
    let platform = program.Binary.Platform
    let applicator = SummaryApplicator.create platform
    let classifyConstant = ConstantClassifier.forBinary program.Binary.Handle
    let visitOrder = revDFS program.Functions

    let analyzeFunction (calleeAnalyResults, summaries, nextTypeId) targetAddr =
      let function_ = Map.find targetAddr program.Functions

      let applyCallSummary
        (programPoint: ProgramPoint)
        (inputs: Variable list)
        (outputs: Variable list)
        state
        =
        let calleeOpt = trySingleCallee function_ programPoint.Address

        match calleeOpt with
        | Some callee ->
          match Map.tryFind callee summaries with
          | Some calleeSum ->
            Some (applicator.apply calleeSum inputs outputs state)
          | None -> None
        | None -> None

      let config =
        { StmtEvalConfig.empty with
            PointerUse = function_.DFAResult.PointerUse
            ConstValue = function_.DFAResult.ConstValue
            ClassifyConstant = classifyConstant
            StackPointer = Some platform.StackPointer
            ApplyCallSummary = applyCallSummary }

      let result =
        AnalyzerDomain.analyzeRawWithStart
          platform
          nextTypeId
          config
          function_.CFG

      let summary =
        FunctionSummaryBuilder.build
          function_.Address
          function_.Name
          platform
          result

      let analysis =
        { Function = function_
          Result = result
          Summary = summary }

      Map.add targetAddr analysis calleeAnalyResults,
      Map.add targetAddr summary summaries,
      summary.NextTypeId

    let analyses, summaries, nextTypeId =
      timed trackTime "Analyze transfer and summaries" (fun () ->
        List.fold analyzeFunction (Map.empty, Map.empty, 0) visitOrder)

    let typeStateDomain = TypeStateDomain.createDefault ()

    let rawTypeState =
      analyses
      |> Map.toSeq
      |> Seq.map (fun (_, analysis) -> analysis.Result.FinalState.Types)
      |> Seq.fold typeStateDomain.join typeStateDomain.bot

    let solvedTypeState =
      timed trackTime "Solve type constraints" (fun () ->
        typeStateDomain.solve rawTypeState)

    { Functions = analyses
      Summaries = summaries
      TypeConstraints = solvedTypeState.Constraints
      TypeConflicts = solvedTypeState.Conflicts
      NextTypeId = nextTypeId }

  let analyze program = analyzeWithTimer true program
