module EvaluateAnalyzer.Evaluator.ParseJSON

open System
open System.IO
open System.Text.Json
open EvaluateAnalyzer.Evaluator.Types

/// Trasform hex-string to lowercase string
let private normalizeAddress (address: string) =
  if address.StartsWith ("0x", StringComparison.OrdinalIgnoreCase) then
    "0x" + address.Substring(2).ToLowerInvariant ()
  else
    address.ToLowerInvariant ()

/// Classify given type string into type constant
let private parseEvalType (value: string) =
  let normalized = value.Trim().ToLowerInvariant ()

  if normalized.StartsWith "address" then Address
  elif normalized.StartsWith "value" then Value
  elif normalized.StartsWith "conflict" then Conflict
  else Unknown

/// Extract function name from JSON element
let private getFunName defaultValue (name: string) (element: JsonElement) =
  let prop = element.GetProperty name

  if prop.ValueKind = JsonValueKind.String then
    prop.GetString ()
  else
    defaultValue

/// Given JSON element(value of "Args"), extract type of
/// parameter/return value list
let private readTypeArray (element: JsonElement) =
  if element.ValueKind <> JsonValueKind.Array then
    (* This case may not be held *)
    []
  else
    element.EnumerateArray ()
    |> Seq.choose (fun item ->
      if item.ValueKind = JsonValueKind.String then
        Some (parseEvalType (item.GetString ()))
      else
        (* This case may not be held *)
        None)
    |> Seq.toList

/// Given JSON element(value of inferred "Args"), extract type of
/// parameter map. Object form preserves parameter indices. Array form is
/// accepted for old result files and uses array indices as parameter indices.
let private readTypeMap (element: JsonElement) =
  match element.ValueKind with
  | JsonValueKind.Object ->
    element.EnumerateObject ()
    |> Seq.choose (fun prop ->
      match System.Int32.TryParse prop.Name, prop.Value.ValueKind with
      | (true, index), JsonValueKind.String ->
        Some (index, parseEvalType (prop.Value.GetString ()))
      | _ -> None)
    |> Map.ofSeq
  | _ -> Map.empty

(*
  ToDo
    Handle if multiple return register is used (XMM, ...?)
*)
/// Given JSON element(value of "ReturnReg"), extract type of
/// return value list
let private readReturnReg (element: JsonElement) =
  if element.ValueKind <> JsonValueKind.Object then
    (* This case may not be held *)
    []
  else
    element.EnumerateObject ()
    |> Seq.sortBy (fun prop -> prop.Name)
    |> Seq.choose (fun prop ->
      if prop.Value.ValueKind = JsonValueKind.String then
        Some (parseEvalType (prop.Value.GetString ()))
      else
        (* This case may not be held *)
        None)
    |> Seq.toList

/// Parsing GT Json and construct GT Map.
/// The address is used as Key of GT Map.
let loadGroundTruth path : Map<string, GTFunction> =
  (* Check gt json file exists *)
  if not (File.Exists path) then
    failwithf "ground-truth JSON does not exist: %s" path

  (* Parse given JSON file *)
  use doc = JsonDocument.Parse (File.ReadAllText path)

  doc.RootElement.EnumerateObject ()
  |> Seq.map (fun prop ->
    (* Key of GT Json: Address *)
    let address = normalizeAddress prop.Name
    let body = prop.Value

    (* Extract function name *)
    let name = getFunName address "Name" body

    (* Extract GT type of parameters *)
    let args = body.GetProperty "Args" |> readTypeArray

    (* Extract GT type of return value *)
    let returns = body.GetProperty "Return" |> readTypeArray

    (* Construct function signature *)
    let gtFunction: GTFunction =
      { Function = { Address = address; Name = name }
        Args = args
        Return = returns }

    address, gtFunction)
  |> Map.ofSeq

/// Parsing inferred result and construct InferredFunction Map.
/// The address is used as Key of InferredFunction Map.
let loadInferred path : Map<string, InferredFunction> =
  (* Check inferred json file exists *)
  if not (File.Exists path) then
    failwithf "inferred result JSON does not exist: %s" path

  (* Parse given JSON file *)
  use doc = JsonDocument.Parse (File.ReadAllText path)

  doc.RootElement.EnumerateObject ()
  |> Seq.map (fun prop ->
    (* Key of GT Json: Address *)
    let address = normalizeAddress prop.Name
    let body = prop.Value

    (* Extract function name *)
    let name = getFunName address "Name" body

    (* Extract inferred type of parameters *)
    let argsElem = body.GetProperty "Arguments"
    let args = argsElem.GetProperty "Args" |> readTypeMap

    (* Extract inferred type of return value *)
    let retRegElem = body.GetProperty "ReturnReg"
    let returns = readReturnReg retRegElem

    (* Construct inferred function signature *)
    let inferredFunction: InferredFunction =
      { Function = { Address = address; Name = name }
        Args = args
        Return = returns }

    address, inferredFunction)
  |> Map.ofSeq
