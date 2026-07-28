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
  builder.AppendLine title |> ignore

  if Set.isEmpty functions then
    builder.AppendLine "<none>" |> ignore
  else
    functions
    |> Seq.map functionToText
    |> Seq.sort
    |> Seq.iter (fun line -> builder.AppendLine (line) |> ignore)

  builder.AppendLine () |> ignore

let private targetToText =
  function
  | Argument index -> sprintf "Argument %d" index
  | ArgumentSlot (argumentIndex, slotIndex, path) ->
    if System.String.IsNullOrWhiteSpace path then
      sprintf "Argument %d Slot %d" argumentIndex slotIndex
    else
      sprintf "Argument %d Slot %d (%s)" argumentIndex slotIndex path
  | Return index -> sprintf "Return %d" index
  | ReturnSlot (returnIndex, slotIndex, path) ->
    if System.String.IsNullOrWhiteSpace path then
      sprintf "Return %d Slot %d" returnIndex slotIndex
    else
      sprintf "Return %d Slot %d (%s)" returnIndex slotIndex path

let private appendStructureCoverage
  (builder: StringBuilder)
  (entries: Set<StructureCoverage>)
  =
  builder.AppendLine "===== Structure Slot Coverage =====" |> ignore

  if Set.isEmpty entries then
    builder.AppendLine "<none>" |> ignore
  else
    entries
    |> Seq.sortBy (fun entry -> entry.Function.Address, entry.Target)
    |> Seq.iter (fun entry ->
      let state =
        if entry.ObservedSlots = 0 then
          "unobserved"
        else
          "partially observed"

      builder.AppendLine (
        sprintf
          "%s %s: %s (%d/%d)"
          (functionToText entry.Function)
          (targetToText entry.Target)
          state
          entry.ObservedSlots
          entry.ExpectedSlots
      )
      |> ignore)

  builder.AppendLine () |> ignore

let toText logState =
  let builder = StringBuilder ()

  appendSection builder "===== GTUnknown Functions =====" logState.GTUnknown

  appendSection
    builder
    "===== Invalid GT Size Functions ====="
    logState.InvalidGTSize

  appendSection
    builder
    "===== Large Return Functions ====="
    logState.LargeReturn

  appendSection
    builder
    "===== MissedDetect Functions ====="
    logState.MissedDetect

  appendSection
    builder
    "===== CountMismatch Functions ====="
    logState.CountMismatch

  appendSection builder "===== Correct Functions =====" logState.Correct
  appendSection builder "===== MisInferred Functions =====" logState.MisInferred
  appendSection builder "===== Conflict Functions =====" logState.Conflict
  appendSection builder "===== Failed Functions =====" logState.Fail
  appendSection builder "===== Infer More Params =====" logState.InferMoreParams
  appendStructureCoverage builder logState.StructureCoverage

  builder.ToString ()
