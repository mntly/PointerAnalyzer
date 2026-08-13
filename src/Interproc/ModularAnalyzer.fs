module PointerAnalyzer.Interproc.ModularAnalyzer

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeIdMap
open PointerAnalyzer.AbsDom.TypeState
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Analysis.StmtEval
open PointerAnalyzer.Frontend.ConstantClassifier
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.PreAnalysis.PreAnalysisTypes
open PointerAnalyzer.Summary
open PointerAnalyzer.Summary.FunctionSummaryBuilder
open PointerAnalyzer.Summary.SummaryApplicator
open PointerAnalyzer.TypeInference.ResolvedType
open PointerAnalyzer.Utils

/// <summary>
/// Controls whether callee summaries are applied at function calls.
/// </summary>
/// <remarks>
/// <c>ApplyFunctionSummary</c> lets PointerAnalyzer applies function summary.
/// <c>IgnoreFunctionSummary</c> lets PointerAnalyzer does not apply function
/// summary.
/// </remarks>
type FunctionApplyMode =
  | ApplyFunctionSummary
  | IgnoreFunctionSummary

  member this.isApply =
    match this with
    | ApplyFunctionSummary -> true
    | IgnoreFunctionSummary -> false

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
    ConstraintOrigins: Map<TypeConstraint, ConstraintOrigin> option
    TypeDerivations: Map<TypeFact, TypeDerivation> option
    NextTypeId: TypeId }

module ModularAnalyzer =
  /// Used for print out the result type of each variable
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
          (variable.ToString ())
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

  (*
    ToDo
      Handle when there exist multiple callees at same callsite 
  *)
  /// Extract callee at given callsite
  let private trySingleCallee function_ callSite =
    let callee = Map.tryFind callSite function_.Callees

    match callee with
    | Some calleeSet when Set.count calleeSet = 1 ->
      Some (Set.minElement calleeSet)
    | _ -> None

  /// Find entry point of target callee at given callsite, and extract
  /// ReturningStatus of corresponding callee.
  let private tryReturningStatus
    (cfg: SSACFG)
    (programPoint: ProgramPoint)
    calleeAddress
    =
    (* Used for extracting callsite of caller *)
    let containsProgramPoint (block: IVertex<SSABasicBlock>) =
      block.VData.Internals.Statements
      |> Array.exists (fun (pp, _) -> pp = programPoint)

    (*
      Check given successor block is target callee functio
      (FunctionAbstraction)
    *)
    let calleeEntry (successor: IVertex<SSABasicBlock>) =
      successor.VData.Internals.IsAbstract
      && successor.VData.Internals.AbstractContent.EntryPoint = calleeAddress

    (* CallSites must in caller -> Not in Fake Node: Function Abstraction *)
    let callSites =
      cfg.Vertices
      |> Array.tryFind (fun block ->
        not block.VData.Internals.IsAbstract && containsProgramPoint block)

    callSites
    (* Filter target callee *)
    |> Option.bind (fun callerBlock ->
      cfg.GetSuccs callerBlock |> Array.tryFind calleeEntry)
    (* Extract ReturningStatus of target callee *)
    |> Option.map (fun abstraction ->
      abstraction.VData.Internals.AbstractContent.ReturningStatus)

  /// Process main-analysis as modular analysis
  let analyzeWithTimer
    trackTime
    (functionApplyMode: FunctionApplyMode)
    trackTypeProvenance
    (program: ProgramPreResult)
    =
    let platform = program.Binary.Platform
    let applicator = SummaryApplicator.create platform
    let classifyConstant = ConstantClassifier.forBinary program.Binary.Handle
    let visitOrder = program.VisitOrder

    /// Analyze each function
    let analyzeFunction (calleeAnalyResults, summaries, nextTypeId) targetAddr =
      (* Recover function to analyze *)
      let functionPreResult = Map.find targetAddr program.Functions
      let func = functionPreResult.FunctionDFA

      /// If callee is valid, then apply callee summary.
      /// `targetAddr` is used for checking jump target is inlined function by
      /// B2R2.
      let applyCallSummary
        (programPoint: ProgramPoint)
        (targetAddr: Addr option)
        (inputs: Variable list)
        (outputs: Variable list)
        state
        =
        let calleeOpt =
          match trySingleCallee func programPoint.Address with
          | Some callee -> Some callee
          | None ->
            (* Check target address is function inlined by B2R2 *)
            targetAddr
            |> Option.filter (fun address -> Map.containsKey address summaries)

        match calleeOpt, functionApplyMode.isApply with
        | Some callee, true ->
          match Map.tryFind callee summaries with
          | Some calleeSum ->
            let returningStatus =
              tryReturningStatus func.CFG programPoint callee
              |> Option.defaultValue UnknownNoRet

            let state =
              applicator.apply calleeSum returningStatus inputs outputs state

            Some (state, callee)
          | None -> None
        | Some callee, false -> Some (state, callee)
        | None, _ -> None

      // let debug = func.Address = 0x804B17BUL
      let debug = false

      let config =
        StmtEvalConfig.construct
          program.Binary.Handle
          functionPreResult
          classifyConstant
          platform.StackPointer
          applyCallSummary
          trackTypeProvenance
          debug (* Used for tracking new type constraint per stmt *)

      (* Transfer stmt to collect type constraints *)
      let result =
        AnalyzerDomain.analyzeRawWithStart
          platform
          nextTypeId
          config
          func.CFG
          func.RetAddresses

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

    let typeStateDomain =
      TypeStateDomain.createWithProvenance 0 trackTypeProvenance

    (* Merge all type constrains from all analysis results *)
    (* Since I can not know the last evaluated, so just union all constraints *)
    let rawTypeState =
      analyses
      |> Map.toSeq
      |> Seq.map (fun (_, analysis) ->
        { analysis.Result.FinalState.Types with
            Constraints = analysis.Summary.Constraints
            ConstraintOrigins = analysis.Summary.ConstraintOrigins })
      |> Seq.fold typeStateDomain.join typeStateDomain.bot

    let solvedTypeState =
      timed trackTime "Solve type constraints" (fun () ->
        typeStateDomain.solve rawTypeState)

    { Functions = analyses
      Summaries = summaries
      TypeConstraints = solvedTypeState.Constraints
      TypeConflicts = solvedTypeState.Conflicts
      ConstraintOrigins = solvedTypeState.ConstraintOrigins
      TypeDerivations = solvedTypeState.Derivations
      NextTypeId = nextTypeId }
