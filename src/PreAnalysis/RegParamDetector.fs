module PointerAnalyzer.PreAnalysis.RegParamDetector

open B2R2
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.DataFlow
open PointerAnalyzer.Frontend.FunctionDFA
open PointerAnalyzer.Frontend.ProgramDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.PreAnalysis.VariableCollector

/// Check whether the variable reaches a non-PHI use. PHI nodes only propagate
/// the value, so continue from their destination variables.
let private hasNonPhiUse (dfa: FunctionDFA) variable =
  let rec loop visited variable =
    if Set.contains variable visited then
      false
    else
      let visited = Set.add variable visited

      match dfa.Edges.Uses.TryGetValue variable with
      | true, uses ->
        uses
        |> Seq.exists (fun location ->
          match Map.tryFind location dfa.StatementIndex with
          | Some { Statement = Phi (destination, _) } ->
            (*
              If target reg is used as PHI source, check the usage of
              destination of PHI node
            *)
            loop visited destination
          | Some _ -> true
          | None -> false)
      | false, _ -> false

  loop Set.empty variable

/// Collect the register given from caller by collecting the minimum identifier
/// with used before definition.
let private incomingRegisters (dfa: FunctionDFA) =
  dfa.Edges.Uses.Keys
  (*
    Incoming registers should be
    1. Not defined in current function
    2. Used as non-Phi instruction
  *)
  |> Seq.filter (fun variable ->
    not (dfa.Edges.Defs.ContainsKey variable) && hasNonPhiUse dfa variable)
  |> Seq.choose (fun variable ->
    tryRegisterId variable
    |> Option.map (fun registerId -> registerId, variable))
  |> Seq.groupBy fst
  |> Seq.map (fun (registerId, variables) ->
    let variable =
      variables |> Seq.map snd |> Seq.minBy (fun value -> value.Identifier)

    registerId, variable)
  |> Map.ofSeq

/// Detect used-before-defined variables only for predefined regparam registers.
let detect (platform: Platform) (function_: FunctionDFAResult) =
  let incomingRegs = incomingRegisters function_.DFAResult

  let detectedRegParams =
    platform.RegParams
    |> List.choose (fun registerId ->
      Map.tryFind registerId incomingRegs
      |> Option.map (fun variable -> registerId, variable))

  detectedRegParams
