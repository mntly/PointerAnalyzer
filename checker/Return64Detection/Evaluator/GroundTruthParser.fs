module Checker.Return64Detection.Evaluator.GroundTruthParser

open System
open System.Globalization
open B2R2
open Checker.Return64Detection.Evaluator.Types

module ConvertedTypes = EvaluateAnalyzer.Evaluator.Types

/// Transform given string address to uint64 value.
/// Converted GT stores GT with the address of function as key.
let private parseAddress (gtKey: string) : Addr =
  let normalized = gtKey.Trim ()

  let digits =
    if normalized.StartsWith ("0x", StringComparison.OrdinalIgnoreCase) then
      normalized.Substring 2
    else
      normalized

  match
    UInt64.TryParse (
      digits,
      NumberStyles.HexNumber,
      CultureInfo.InvariantCulture
    )
  with
  | true, address -> address
  | _ -> failwithf "invalid ground-truth function address: %s" gtKey

/// Classify one ABI-converted return element for Return64Detector.
/// Structure returns use a hidden return-buffer argument on x86-32, so their
/// source size must not make them Return64.
let private classifyReturn
  wordSize
  (returns: ConvertedTypes.GTElement list)
  =
  match returns with
  | [] ->
    (* No return value is a negative case for Return64Detector. *)
    Return32
  | [ entry ] ->
    match entry.Kind with
    | ConvertedTypes.StructureElement when entry.Size <= 0 ->
      InvalidReturn (
        sprintf "invalid converted structure return size: %d" entry.Size
      )
    | ConvertedTypes.StructureElement ->
      (* In x86-32, always the address of struture pointer is returned *)
      Return32
    | ConvertedTypes.NormalElement ->
      if entry.Size = wordSize * 2 && entry.OccupiedSlotCount = 2 then
        Return64
      elif entry.Size > 0 && entry.Size <= wordSize then
        Return32
      else
        InvalidReturn (
          sprintf
            "invalid converted return size/slots: size=%d, slots=%d"
            entry.Size
            entry.OccupiedSlotCount
        )
  | _ ->
    InvalidReturn (
      sprintf "multiple return entries: %d" (List.length returns)
    )

/// Adapt ABI-converted GT to the expectation used by Return64Detector.
let fromConverted
  wordSize
  (functions: Map<string, ConvertedTypes.GTFunction>)
  : Map<Addr, GTFunction> =
  if wordSize <= 0 then
    invalidArg (nameof wordSize) "word size must be positive"

  functions
  |> Map.toSeq
  |> Seq.map (fun (addressText, converted) ->
    let address = parseAddress addressText

    address,
    { Function =
        { Address = address
          Name = converted.Function.Name }
      Expectation = classifyReturn wordSize converted.Return })
  |> Map.ofSeq
