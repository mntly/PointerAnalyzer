module Checker.Return64Detection.Evaluator.Log

open System.Text
open Checker.Return64Detection.Evaluator.Types

let private functionText function_ =
  if System.String.IsNullOrWhiteSpace function_.Name then
    sprintf "0x%08x" function_.Address
  else
    sprintf "0x%08x %s" function_.Address function_.Name

let private appendFunctions
  (builder: StringBuilder)
  (title: string)
  (functions: Set<FunctionKey>)
  =
  builder.AppendLine title |> ignore

  if Set.isEmpty functions then
    builder.AppendLine "<none>" |> ignore
  else
    functions
    |> Seq.map functionText
    |> Seq.sort
    |> Seq.iter (fun line -> builder.AppendLine line |> ignore)

  builder.AppendLine () |> ignore

let private appendInvalidGT
  (builder: StringBuilder)
  (entries: Set<FunctionKey * string>)
  =
  builder.AppendLine "===== Invalid GT =====" |> ignore

  if Set.isEmpty entries then
    builder.AppendLine "<none>" |> ignore
  else
    entries
    |> Seq.map (fun (function_, reason) ->
      sprintf "%s (%s)" (functionText function_) reason)
    |> Seq.sort
    |> Seq.iter (fun line -> builder.AppendLine line |> ignore)

  builder.AppendLine () |> ignore

let toText logState =
  let builder = StringBuilder ()

  appendFunctions builder "===== True Positive =====" logState.TruePositive
  appendFunctions builder "===== True Negative =====" logState.TrueNegative
  appendFunctions builder "===== False Positive =====" logState.FalsePositive
  appendFunctions builder "===== False Negative =====" logState.FalseNegative
  appendInvalidGT builder logState.InvalidGT
  appendFunctions builder "===== Missing GT =====" logState.MissingGT

  builder.ToString ()
