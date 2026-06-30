module PointerAnalyzer.Interproc.ModularAnalyzer

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
open PointerAnalyzer.Utils

/// <summary>
/// Main analysis result of one specific function.
/// </summary>
/// <remarks>
/// <c>Function</c> is PointerAnalyzer's
/// <see cref="PPointerAnalyzer.Frontend.ProgramDFA.FunctionDFAResult" />.
/// <c>Result</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.Analysis.Analyzer.AnalysisResult" />.
/// <c>Summary</c> is PointerAnalyzer's
/// <see cref="PointerAnalyzer.Summary.FunctionSummary" />.
/// </remarks>
type FunctionAnalysisResult =
  { Function: FunctionDFAResult
    Result: AnalysisResult
    Summary: FunctionSummary }

/// <summary>
/// Main analysis result of given binary.
/// </summary>
/// <remarks>
/// <c>Functions</c> is per-function main analysis result.
/// <c>Summaries</c> is per-function summary used for function applying.
/// <c>TypeConstraints</c> is final type constraints from constraint sovler.
/// <c>TypeConflicts</c> contains some SSA variables inferred as both address
/// and constant value.
/// <c>NextTypeId</c> is next fresh type id.
/// </remarks>
type ModularAnalysisResult =
  { Functions: Map<Addr, FunctionAnalysisResult>
    Summaries: Map<Addr, FunctionSummary>
    TypeConstraints: ConstraintSet
    TypeConflicts: Set<TypeId>
    NextTypeId: TypeId }

module ModularAnalyzer =
  (* Used for print out the result type of each variable *)
  let functionAnalysisToString
    resultAnalysisResult
    (address: Addr)
    funAnalysis
    =
    (* Get final type of each SSA varaible *)
    let resolvedTypes =
      ResolvedTypeMap.build
        resultAnalysisResult.TypeConstraints
        resultAnalysisResult.TypeConflicts
        funAnalysis.Result.FinalState.Types.TypeIndicators

    (* Transform the SSA variable type mapping into string *)
    let registerTypeStr =
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
      if registerTypeStr = "" then
        "    <empty>"
      else
        registerTypeStr ]
    |> String.concat "\n"

  (* From all callees, filtering only internal functions *)
  let private internalCallees (funcs: Map<Addr, FunctionDFAResult>) func =
    let calleeSeq = func.Callees |> Map.toSeq |> Seq.collect (snd >> Set.toSeq)

    let internalFuncs =
      calleeSeq
      |> Seq.filter (fun address -> Map.containsKey address funcs)
      |> Set.ofSeq

    internalFuncs

  (*
    Sort functions from Callee to Caller.
    The modular analysis is processed with this order
  *)
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

  (* Extract callee at given callsite *)
  (*
    ToDo
      Handle when there exist multiple callees at same callsite 
  *)
  let private trySingleCallee function_ callSite =
    let callee = Map.tryFind callSite function_.Callees

    match callee with
    | Some calleeSet when Set.count calleeSet = 1 ->
      Some (Set.minElement calleeSet)
    | _ -> None

  /// Process main-analysis as modular analysis
  let analyzeWithTimer trackTime (program: ProgramDFAResult) =
    let platform = program.Binary.Platform
    let applicator = SummaryApplicator.create platform
    let classifyConstant = ConstantClassifier.forBinary program.Binary.Handle
    let visitOrder = revDFS program.Functions

    (* Analyze each function *)
    let analyzeFunction (calleeAnalyResults, summaries, nextTypeId) targetAddr =
      (* Recover function to analyze *)
      let func = Map.find targetAddr program.Functions

      (* If callee is valid, then apply callee summary *)
      let applyCallSummary
        (programPoint: ProgramPoint)
        (inputs: Variable list)
        (outputs: Variable list)
        state
        =
        let calleeOpt = trySingleCallee func programPoint.Address

        match calleeOpt with
        | Some callee ->
          match Map.tryFind callee summaries with
          | Some calleeSum ->
            Some (applicator.apply calleeSum inputs outputs state)
          | None -> None
        | None -> None

      let config =
        StmtEvalConfig.construct
          func.DFAResult
          classifyConstant
          platform.StackPointer
          applyCallSummary
          false

      (* Transfer stmt to collect type constraints *)
      let result =
        AnalyzerDomain.analyzeRawWithStart platform nextTypeId config func.CFG

      (* Store analysis result *)
      let summary =
        FunctionSummaryBuilder.build func.Address func.Name platform result

      let analysis =
        { Function = func
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
