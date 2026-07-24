module Checker.Return64Detection.Evaluator.GroundTruthParser

open System
open System.Globalization
open System.IO
open System.Text.Json
open Checker.Return64Detection.Evaluator.Types

/// Transform given string address to uint64 value.
/// GT Json stores GT with the address of function as key.
let private parseAddress (gtKey: string) =
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

/// Extract function name from GT json.
/// If function name not exist, use address as name.
let private functionName address (body: JsonElement) =
  let name = body.GetProperty "Name"

  if name.ValueKind = JsonValueKind.String then
    name.GetString ()
  else
    sprintf "0x%08x" address

/// Check return size of given function GT element, and
/// extract GT of Return64Detector.
let private classifyReturn (body: JsonElement) =
  let returns = body.GetProperty "Return"

  if returns.ValueKind <> JsonValueKind.Array then
    (* GT should store return value as array *)
    InvalidReturn "Return is not an array"
  else
    let entries = returns.EnumerateArray () |> Seq.toList

    match entries with
    | [] ->
      (* Not return: Handle as Return32 *)
      Return32
    | [ entry ] ->
      let size = entry.GetProperty("Size").GetInt32 ()

      if size = 8 then Return64
      elif size > 0 && size <= 4 then Return32
      else InvalidReturn (sprintf "invalid return size: %d" size)
    | _ ->
      InvalidReturn (
        sprintf "multiple return entries: %d" (List.length entries)
      )

let load path : Map<uint64, GTFunction> =
  (* Check given GT file exsits *)
  if not (File.Exists path) then
    failwithf "ground-truth JSON does not exist: %s" path

  (* Parse given GT json file *)
  use document = JsonDocument.Parse (File.ReadAllText path)

  if document.RootElement.ValueKind <> JsonValueKind.Object then
    failwith "ground-truth JSON root is not an object"

  (* Extract GT return size *)
  document.RootElement.EnumerateObject ()
  |> Seq.map (fun property ->
    let address = parseAddress property.Name
    let body = property.Value

    address,
    { Function =
        { Address = address
          Name = functionName address body }
      Expectation = classifyReturn body })
  |> Map.ofSeq
