module PointerAnalyzer.PreAnalysis.RegParamDetector

open System.Collections.Generic
open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.Frontend.FunctionDFA
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check whether the variable reaches a non-PHI use. PHI nodes only propagate
/// the value, so continue from their destination variables.
let private hasNonPhiUse
  (dfa: FunctionDFA)
  (cache: Dictionary<Variable, bool>)
  variable
  =
  let active = HashSet<Variable> ()

  let rec loop variable =
    match cache.TryGetValue variable with
    | true, result -> result
    | false, _ when not (active.Add variable) -> false
    | false, _ ->
      let result =
        match dfa.Edges.Uses.TryGetValue variable with
        | true, uses ->
          uses
          |> Seq.exists (fun location ->
            match Map.tryFind location dfa.StatementIndex with
            | Some { Statement = Phi (destination, _) } -> loop destination
            | Some _ -> true
            | None -> false)
        | false, _ -> false

      active.Remove variable |> ignore
      cache[variable] <- result
      result

  loop variable

/// Detect used-before-defined variables only for predefined regparam registers.
let detect (platform: Platform) (function_: FunctionDFAResult) =
  let dfa = function_.DFAResult
  let cache = Dictionary<Variable, bool> ()

  let regParamIndices =
    platform.RegParams
    |> List.mapi (fun index registerId -> registerId, index)
    |> Map.ofList

  dfa.Edges.Uses.Keys
  |> Seq.choose (fun variable ->
    if variable.Identifier <> 0 || dfa.Edges.Defs.ContainsKey variable then
      None
    else
      tryRegisterId variable
      |> Option.bind (fun registerId ->
        Map.tryFind registerId regParamIndices
        |> Option.map (fun index -> index, registerId, variable)))
  |> Seq.filter (fun (_, _, variable) -> hasNonPhiUse dfa cache variable)
  |> Seq.sortBy (fun (index, _, _) -> index)
  |> Seq.map (fun (_, registerId, variable) -> registerId, variable)
  |> Seq.toList
