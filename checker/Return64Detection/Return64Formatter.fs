module Checker.Return64Detection.Return64Formatter

open Checker.Return64Detection.Return64Types

let private rangeName =
  function
  | LeafAndDirectPredecessors -> "LeafAndDirectPredecessors"
  | EntireFunction -> "EntireFunction"

let private heuristicName =
  function
  | Basic -> "Basic"
  | BasicWithCallerChecker -> "BasicWithCallerChecker"

let private statusName =
  function
  | Return64 -> "Return64"
  | NotReturn64 -> "NotReturn64"
  | Unknown -> "Unknown"
  | UnknownCallEvidence -> "UnknownCallEvidence"

let toText result =
  let statusLines status =
    result.Functions
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter (fun function_ -> function_.Status = status)
    |> Seq.map (fun function_ ->
      sprintf "  0x%08x %s" function_.Address function_.Name)
    |> Seq.toList

  let section title status =
    let lines = statusLines status
    title :: (if List.isEmpty lines then [ "  <empty>" ] else lines)

  let return64Count = statusLines Return64 |> List.length
  let notReturn64Count = statusLines NotReturn64 |> List.length
  let unknownCount = statusLines Unknown |> List.length
  let unknownCall = statusLines UnknownCallEvidence |> List.length

  [ sprintf "Range: %s" (rangeName result.Range)
    sprintf "Heuristic: %s" (heuristicName result.Heuristic)
    sprintf
      "Counts: Total=%d Return64=%d NotReturn64=%d Unknown=%d UnknownCall=%d"
      (return64Count + notReturn64Count + unknownCount + unknownCall)
      return64Count
      notReturn64Count
      unknownCount
      unknownCall
    ""
    yield! section "Return64 Functions" Return64
    ""
    yield! section "Unknown Functions" Unknown
    ""
    yield!
      section "Unknown Functions with No Callee Evidence" UnknownCallEvidence ]
  |> String.concat "\n"
  |> fun text -> text + "\n"
