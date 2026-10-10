module PointerAnalyzer.PreAnalysis.RegParamDetector

open System.Collections.Generic
open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.Frontend.FunctionDFA
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check whether the variable reaches a non-PHI use.
let private hasNonPhiUse
  (dfa: FunctionDFA)
  (cache: Dictionary<Variable, bool>)
  variable
  =
  let active = HashSet<Variable> ()

  let rec loop variable =
    match cache.TryGetValue variable with
    | true, result ->
      (* Return the cached result(usecase T/F) *)
      result
    | false, _ when not (active.Add variable) ->
      (* Given variable is already exist in active *)
      (* This loop is part of checking the usecase of given variable *)
      false
    | false, _ ->
      (* Given variable is not existed in active *)
      let result =
        match dfa.Edges.Uses.TryGetValue variable with
        | true, uses ->
          (* Given variable is used -> Live variable *)
          uses
          |> Seq.exists (fun location ->
            match tryFindStatement location dfa.StatementIndex with
            | Some { Statement = Phi (destination, _) } ->
              (* If PHI usecase, proppagate to find next usecases *)
              loop destination
            | Some _ ->
              (* Accept other usecases *)
              true
            | None -> false)
        | false, _ ->
          (* Given variable is not used -> Dead variable *)
          false

      (* Checking usecase of given variable is done *)
      active.Remove variable |> ignore
      (* Caching the result(usecase T/F) of given variable *)
      cache[variable] <- result
      result

  loop variable

/// Extract the SSA variables used as regparm:
///   SSA variables used Non-Phi statements but not defined.
let detect (platform: Platform) (function_: FunctionDFAResult) =
  let dfa = function_.DFAResult
  let cache = Dictionary<Variable, bool> ()

  (* Get possible registers used as regparm *)
  let regParamIndices =
    platform.RegParams
    |> List.mapi (fun index registerId -> registerId, index)
    |> Map.ofList

  let candidates =
    dfa.Edges.Uses.Keys
    (* Filter registers only the first(Id == 0) used but not defined  *)
    |> Seq.choose (fun variable ->
      if variable.Identifier <> 0 || dfa.Edges.Defs.ContainsKey variable then
        None
      else
        tryRegisterId variable
        |> Option.bind (fun registerId ->
          Map.tryFind registerId regParamIndices
          |> Option.map (fun _ -> registerId, variable)))
    (* Filter the live undefined registers: use at Non-Phi statement *)
    |> Seq.filter (fun (_, variable) -> hasNonPhiUse dfa cache variable)
    |> Map.ofSeq

  platform.RegParams
  (* regparms used in order of predefined regparms *)
  |> List.takeWhile (fun registerId -> Map.containsKey registerId candidates)
  (* Select SSA variables corresponding to each regparm *)
  |> List.choose (fun registerId ->
    Map.tryFind registerId candidates
    |> Option.map (fun variable -> registerId, variable))
