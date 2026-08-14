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

/// Given a RawGT JSON element, extract its type.
/// This recursively extracts structure type as a field of structure.
let rec private readRawGTType (element: JsonElement) =
  let typeName =
    element.GetProperty("Type").GetString().Trim().ToLowerInvariant ()

  match typeName with
  | "address" -> RawAddress
  | "value" -> RawValue
  | "structure" ->
    let fields =
      element.GetProperty("Fields").EnumerateArray ()
      |> Seq.map (fun field ->
        { Name = getFunName "" "Name" field
          Offset = field.GetProperty("Offset").GetInt32 ()
          Size = field.GetProperty("Size").GetInt32 ()
          Type = readRawGTType field })
      |> Seq.toList

    RawStructure fields
  | _ -> RawUnknown

/// Given a RawGT JSON element, extract its source-level byte size and type.
let private readGTElement (element: JsonElement) =
  let size = element.GetProperty("Size").GetInt32 ()
  let typ = readRawGTType element
  { Size = size; Type = typ }

/// Given JSON array element, extract GT parameter/return elements.
let private readGTArray (element: JsonElement) =
  if element.ValueKind <> JsonValueKind.Array then
    (* This case may not be held *)
    []
  else
    element.EnumerateArray () |> Seq.map readGTElement |> Seq.toList

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
let private readReturnReg returnSlotRegisters (element: JsonElement) =
  if element.ValueKind <> JsonValueKind.Object then
    (* This case may not be held *)
    []
  else
    let returnTypes =
      element.EnumerateObject ()
      |> Seq.choose (fun prop ->
        if prop.Value.ValueKind = JsonValueKind.String then
          Some (prop.Name, parseEvalType (prop.Value.GetString ()))
        else
          None)
      |> Map.ofSeq

    returnSlotRegisters
    |> List.map (fun registerName ->
      Map.tryFind registerName returnTypes |> Option.defaultValue Unknown)

/// Parsing RawGT Json and construct RawGT Map. RawGT is the direct result of
/// GTExtractor which is not matched with ABI specific low-level conventions.
/// The address is used as Key of RawGT Map.
let loadGroundTruth path : Map<string, RawGTFunction> =
  (* Check raw gt json file exists *)
  if not (File.Exists path) then
    failwithf "raw ground-truth JSON does not exist: %s" path

  (* Parse given JSON file *)
  use doc = JsonDocument.Parse (File.ReadAllText path)

  doc.RootElement.EnumerateObject ()
  |> Seq.map (fun prop ->
    (* Key of RawGT Json: Address *)
    let address = normalizeAddress prop.Name
    let body = prop.Value

    (* Extract function name *)
    let name = getFunName address "Name" body

    (* Extract RawGT type of parameters *)
    let args = body.GetProperty "Args" |> readGTArray

    (* Extract RawGT type of return value *)
    let returns = body.GetProperty "Return" |> readGTArray

    (* Construct raw gt function signature *)
    let gtFunction: RawGTFunction =
      { Function = { Address = address; Name = name }
        Args = args
        Return = returns }

    address, gtFunction)
  |> Map.ofSeq

/// Parse the PointerAnalyzer configuration associated with inferred results.
let loadAnalysisConfig path : AnalysisConfig =
  if not (File.Exists path) then
    failwithf "analysis configuration JSON does not exist: %s" path

  use doc = JsonDocument.Parse (File.ReadAllText path)
  let platform = doc.RootElement.GetProperty("Platform").GetString ()
  let wordSize = doc.RootElement.GetProperty("WordSize").GetInt32 ()
  let returnSlotRegisters =
    doc.RootElement.GetProperty("ReturnSlotRegisters").EnumerateArray ()
    |> Seq.map (fun element -> element.GetString ())
    |> Seq.toList

  if wordSize <= 0 then
    failwithf "analysis configuration has invalid WordSize: %d" wordSize

  if List.isEmpty returnSlotRegisters then
    failwith "analysis configuration has no return-slot registers"

  { Platform = platform
    WordSize = wordSize
    ReturnSlotRegisters = returnSlotRegisters }

/// Parsing inferred result and construct InferredFunction Map.
/// The address is used as Key of InferredFunction Map.
let loadInferred returnSlotRegisters path : Map<string, InferredFunction> =
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
    let returns = readReturnReg returnSlotRegisters retRegElem

    (* Construct inferred function signature *)
    let inferredFunction: InferredFunction =
      { Function = { Address = address; Name = name }
        Args = args
        Return = returns }

    address, inferredFunction)
  |> Map.ofSeq

let private readProvenanceFact (element: JsonElement) =
  { Type = parseEvalType (element.GetProperty("Type").GetString ())
    TypeId = element.GetProperty("TypeId").GetInt32 () }

let private readProvenanceOrigin (element: JsonElement) =
  { FunctionName = element.GetProperty("FunctionName").GetString ()
    Location = element.GetProperty("Location").GetString ()
    Statement = element.GetProperty("Statement").GetString ()
    Annotation = element.GetProperty("Annotation").GetString () }

/// Load the compact type-provenance artifact produced by PointerAnalyzer.
let loadProvenance returnSlotRegisters path : ProvenanceData =
  if not (File.Exists path) then
    failwithf "type provenance JSON does not exist: %s" path

  use doc = JsonDocument.Parse (File.ReadAllText path)

  // Arguments contain integer TypeIds, unlike inferred type maps.
  let functions =
    doc.RootElement.GetProperty("Functions").EnumerateObject ()
    |> Seq.map (fun property ->
      let body = property.Value

      let arguments =
        body.GetProperty("Arguments").EnumerateObject ()
        |> Seq.choose (fun entry ->
          match Int32.TryParse entry.Name with
          | true, index -> Some (index, entry.Value.GetInt32 ())
          | false, _ -> None)
        |> Map.ofSeq

      let returnTypeIds =
        body.GetProperty("ReturnReg").EnumerateObject ()
        |> Seq.map (fun entry -> entry.Name, entry.Value.GetInt32 ())
        |> Map.ofSeq

      let returns =
        returnSlotRegisters
        |> List.indexed
        |> List.choose (fun (index, registerName) ->
          Map.tryFind registerName returnTypeIds
          |> Option.map (fun typeId -> index, typeId))
        |> Map.ofList

      normalizeAddress property.Name,
      { Name = body.GetProperty("Name").GetString ()
        Arguments = arguments
        Return = returns })
    |> Map.ofSeq

  let derivations =
    doc.RootElement.GetProperty("Derivations").EnumerateObject ()
    |> Seq.map (fun property ->
      let body = property.Value

      let premises =
        body.GetProperty("Premises").EnumerateArray ()
        |> Seq.map readProvenanceFact
        |> Seq.toList

      let originIdElement = body.GetProperty "OriginId"

      let originId =
        if originIdElement.ValueKind = JsonValueKind.Null then
          None
        else
          Some (originIdElement.GetInt32 ())

      property.Name,
      { Constraint = body.GetProperty("Constraint").GetString ()
        Premises = premises
        OriginId = originId })
    |> Map.ofSeq

  let typeNames =
    doc.RootElement.GetProperty("TypeNames").EnumerateObject ()
    |> Seq.choose (fun property ->
      match Int32.TryParse property.Name with
      | true, typeId ->
        Some (
          typeId,
          property.Value.EnumerateArray ()
          |> Seq.map (fun element -> element.GetString ())
          |> Seq.toList
        )
      | false, _ -> None)
    |> Map.ofSeq

  let origins =
    doc.RootElement.GetProperty("Origins").EnumerateObject ()
    |> Seq.choose (fun property ->
      match Int32.TryParse property.Name with
      | true, originId -> Some (originId, readProvenanceOrigin property.Value)
      | false, _ -> None)
    |> Map.ofSeq

  { Functions = functions
    TypeNames = typeNames
    Origins = origins
    Derivations = derivations }
