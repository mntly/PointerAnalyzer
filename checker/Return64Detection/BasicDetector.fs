module Checker.Return64Detection.BasicDetector

open B2R2
open PointerAnalyzer.Frontend.ProgramDFA
open Checker.Return64Detection.Return64Types
open Checker.Return64Detection.Analysis.UseChainClassifier

module RangeSelector = Checker.Return64Detection.Analysis.RangeSelector

module ExitVersionAnalyzer = Checker.Return64Detection.Analysis.ExitVersionAnalyzer

let private classifyRegister
  (classifier: Classifier)
  blockIds
  (versionState: VersionState)
  =
  (* Check all live registers whether they used or not *)
  (* versionState stores the live registers from all path *)
  let failures =
    versionState.Versions
    |> Seq.collect (fun variable -> classifier.Classify variable blockIds)
    |> Seq.toList

  { Versions = versionState.Versions
    DefinedOnAllPaths = versionState.DefinedOnAllPaths
    Accepted =
      versionState.DefinedOnAllPaths
      && not (Set.isEmpty versionState.Versions)
      && List.isEmpty failures
    (*
        The registers from at least one path are used to calcuate others, it
        should not be used return value.
      *)
    Failures = failures }

/// Analyze given function with given analysisRange, and
/// detect given function returns 64 bit value or not.
let detectFunction analysisRange (function_: FunctionDFAResult) =
  (* Set Classifier to check given variable is valid return value *)
  let classifier = Classifier function_.CFG

  let leaves =
    (* Filtering target blocks to analyze *)
    RangeSelector.select analysisRange function_
    (* Executes Return64Detector to target blocks per leafs *)
    |> List.map (fun range ->
      (* Select live EAX and EDX register *)
      let exitState = ExitVersionAnalyzer.analyze function_.CFG range
      (* Check EAX and EDX are return value of current return leaf node *)
      let eax = classifyRegister classifier range.BlockIds exitState.EAX
      let edx = classifyRegister classifier range.BlockIds exitState.EDX

      (* Construct Return64Detector per leaf *)
      { LeafId = range.LeafId
        BlockIds = range.BlockIds
        EAX = eax
        EDX = edx
        Accepted = eax.Accepted && edx.Accepted })

  let status =
    match leaves with
    | [] -> Unknown
    | _ when List.forall (fun leaf -> leaf.Accepted) leaves -> Return64
    | _ -> NotReturn64

  { Address = function_.Address
    Name = function_.Name
    BasicStatus = status
    Status = status
    Leaves = leaves
    CallerEvidence = [] }

/// Analyze to find out function returns 64 bit values using basic heuristics
let run analysisRange (program: ProgramDFAResult) =
  program.VisitOrder
  (* Modular Analysis from callee to caller *)
  |> List.map (fun address ->
    let function_ = Map.find address program.Functions
    address, detectFunction analysisRange function_)
  |> Map.ofList
