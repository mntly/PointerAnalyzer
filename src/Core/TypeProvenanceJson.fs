module PointerAnalyzer.TypeProvenanceJson

open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.Interproc.ModularAnalyzer
open PointerAnalyzer.Platform.PlatformTypes

type FactJson =
  { [<JsonPropertyName("Type")>]
    Type: string
    [<JsonPropertyName("TypeId")>]
    TypeId: int }

type OriginJson =
  { [<JsonPropertyName("FunctionName")>]
    FunctionName: string
    [<JsonPropertyName("Location")>]
    Location: string
    [<JsonPropertyName("Statement")>]
    Statement: string
    [<JsonPropertyName("Annotation")>]
    Annotation: string }

type DerivationJson =
  { [<JsonPropertyName("Constraint")>]
    Constraint: string
    [<JsonPropertyName("Premises")>]
    Premises: FactJson list
    [<JsonPropertyName("OriginId")>]
    OriginId: int option }

type FunctionProvenanceJson =
  { [<JsonPropertyName("Name")>]
    Name: string
    [<JsonPropertyName("Arguments")>]
    Arguments: Map<int, int>
    [<JsonPropertyName("ReturnReg")>]
    ReturnReg: Map<string, int> }

type ProvenanceJson =
  { [<JsonPropertyName("Functions")>]
    Functions: Map<string, FunctionProvenanceJson>
    [<JsonPropertyName("TypeNames")>]
    TypeNames: Map<int, string list>
    [<JsonPropertyName("Origins")>]
    Origins: Map<int, OriginJson>
    [<JsonPropertyName("Derivations")>]
    Derivations: Map<string, DerivationJson> }

let private factKey (fact: TypeFact) =
  sprintf "%s:t%d" fact.FactType.ToString fact.TypeId

let private factJson (fact: TypeFact) =
  { Type = fact.FactType.ToString
    TypeId = fact.TypeId }

let private originJson (origin: ConstraintOrigin) =
  { FunctionName = origin.FunctionName
    Location = origin.Location
    Statement = origin.Statement
    Annotation = origin.Annotation }

let fromAnalysisResult (platform: Platform) (result: ModularAnalysisResult) =
  let functions =
    result.Functions
    |> Map.toSeq
    |> Seq.map (fun (address, analysis) ->
      let returns =
        analysis.Summary.RegisterOutputs
        |> Map.toSeq
        |> Seq.filter (fun (registerId, _) ->
          List.contains registerId platform.ReturnRegisters)
        |> Seq.map (fun (registerId, typeId) ->
          platform.RegisterName registerId, typeId)
        |> Map.ofSeq

      sprintf "0x%08x" address,
      { Name = analysis.Function.Name
        Arguments = analysis.Summary.Parameters
        ReturnReg = returns })
    |> Map.ofSeq

  let origins = result.ConstraintOrigins |> Option.defaultValue Map.empty

  (* Extract origins appeared at derivation from entire origins *)
  let usedOrigins =
    result.TypeDerivations
    |> Option.defaultValue Map.empty
    |> Map.toSeq
    |> Seq.choose (fun (_, derivation) ->
      Map.tryFind derivation.Constraint origins)
    |> Set.ofSeq
    |> Set.toList

  (* Origin |-> ID *)
  let originIds =
    usedOrigins
    |> List.indexed
    |> List.map (fun (id, origin) -> origin, id)
    |> Map.ofList

  (* ID |-> Origin Json *)
  let originTable =
    usedOrigins
    |> List.indexed
    |> List.map (fun (id, origin) -> id, originJson origin)
    |> Map.ofList

  (* Add mapping from typeId to corresponding SSA variable name *)
  let addTypeName typeId name names =
    let previous = Map.tryFind typeId names |> Option.defaultValue Set.empty
    Map.add typeId (Set.add name previous) names

  let derivationMap = result.TypeDerivations |> Option.defaultValue Map.empty

  (* Construct constraint propagation history *)
  let rec collectDependencies visited fact =
    if Set.contains fact visited then
      visited
    else
      let visited = Set.add fact visited

      match Map.tryFind fact derivationMap with
      | Some derivation ->
        List.fold collectDependencies visited derivation.Premises
      | None -> visited

  (*
    Currently, only track histroy on FP cases
      => Store only inferred Address type
  *)
  let relevantTypeIds =
    derivationMap
    |> Map.keys
    |> Seq.filter (fun fact -> fact.FactType = AddressFact)
    |> Seq.fold collectDependencies Set.empty
    |> Set.map (fun fact -> fact.TypeId)

  (* Construct mapping from TypeId to SSA variables *)
  (* Tid 0 and 1 reserved by Address and Value, respectivly *)
  let typeNames =
    result.Functions
    |> Map.fold
      (fun names _ analysis ->
        let functionName = analysis.Function.Name

        let names =
          analysis.TypeIndicators
          |> Map.fold
            (fun names variable typeId ->
              if Set.contains typeId relevantTypeIds then
                addTypeName
                  typeId
                  (sprintf "%s:%O" functionName variable)
                  names
              else
                names)
            names

        let names =
          analysis.Summary.Parameters
          |> Map.fold
            (fun names index typeId ->
              if Set.contains typeId relevantTypeIds then
                addTypeName
                  typeId
                  (sprintf "%s:argument[%d]" functionName index)
                  names
              else
                names)
            names

        analysis.Summary.RegisterOutputs
        |> Map.fold
          (fun names registerId typeId ->
            if Set.contains typeId relevantTypeIds then
              let outputKind =
                if List.contains registerId platform.ReturnRegisters then
                  "return"
                else
                  "output"

              addTypeName
                typeId
                (sprintf
                  "%s:%s[%s]"
                  functionName
                  outputKind
                  (platform.RegisterName registerId))
                names
            else
              names)
          names)
      Map.empty
    |> Map.add 0 (Set.singleton "<builtin Address>")
    |> Map.add 1 (Set.singleton "<builtin Value>")
    |> Map.map (fun _ names -> Set.toList names)

  let derivations =
    derivationMap
    |> Map.toSeq
    |> Seq.map (fun (fact, derivation) ->
      let originId =
        Map.tryFind derivation.Constraint origins
        |> Option.bind (fun origin -> Map.tryFind origin originIds)

      factKey fact,
      { Constraint = TypeConstraint.toString derivation.Constraint
        Premises = List.map factJson derivation.Premises
        OriginId = originId })
    |> Map.ofSeq

  { Functions = functions
    TypeNames = typeNames
    Origins = originTable
    Derivations = derivations }

let toJsonString provenance =
  let options =
    JsonSerializerOptions (
      WriteIndented = false,
      Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  JsonSerializer.Serialize (provenance, options) + "\n"
