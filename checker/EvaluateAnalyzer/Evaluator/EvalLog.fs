module EvaluateAnalyzer.Evaluator.Log

open System.Text
open EvaluateAnalyzer.Evaluator.Types

let private functionToText (fn: FunctionKey) =
  if System.String.IsNullOrWhiteSpace fn.Name then
    fn.Address
  else
    sprintf "%s %s" fn.Address fn.Name

let private appendSection
  (builder: StringBuilder)
  (title: string)
  (functions: Set<FunctionKey>)
  =
  builder.AppendLine(title) |> ignore

  if Set.isEmpty functions then
    builder.AppendLine("<none>") |> ignore
  else
    functions
    |> Seq.map functionToText
    |> Seq.sort
    |> Seq.iter (fun line -> builder.AppendLine(line) |> ignore)

  builder.AppendLine() |> ignore

let toText logState =
  let builder = StringBuilder()

  appendSection builder "===== GTUnknown Functions =====" logState.GTUnknown
  appendSection builder "===== MissedDetect Functions =====" logState.MissedDetect
  appendSection builder "===== CountMismatch Functions =====" logState.CountMismatch
  appendSection builder "===== Correct Functions =====" logState.Correct
  appendSection builder "===== MisInferred Functions =====" logState.MisInferred
  appendSection builder "===== Conflict Functions =====" logState.Conflict
  appendSection builder "===== Failed Functions =====" logState.Fail

  builder.ToString()
