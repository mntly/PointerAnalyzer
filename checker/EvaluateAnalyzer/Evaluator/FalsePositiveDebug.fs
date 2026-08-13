module EvaluateAnalyzer.Evaluator.FalsePositiveDebug

open EvaluateAnalyzer.Evaluator.Types

type private ExplanationStep =
  { Number: int
    Fact: ProvenanceFact
    Derivation: ProvenanceDerivation option
    PremiseNumbers: int list }

let private factKey fact =
  sprintf "%s:t%d" fact.Type.ToString fact.TypeId

let private factString fact =
  sprintf "%s(t%d)" fact.Type.ToString fact.TypeId

let private targetString =
  function
  | Argument index -> sprintf "parameter %d" index
  | ArgumentSlot (index, slot, path) ->
    sprintf "parameter %d, structure slot %d (%s)" index slot path
  | Return index -> sprintf "return value %d" index
  | ReturnSlot (index, slot, path) ->
    sprintf "return value %d, structure slot %d (%s)" index slot path

let private sourceString =
  function
  | ArgumentSource index -> sprintf "argument slot %d" index
  | ReturnSource index -> sprintf "return slot %d" index

let private trySourceTypeId function_ =
  function
  | ArgumentSource index -> Map.tryFind index function_.Arguments
  | ReturnSource index -> List.tryItem index function_.Return

/// Build a flat, numbered dependency graph. Each fact is stored once even
/// when several later steps refer to it.
let private buildSteps provenance root =
  let rec visit (numbers, nextNumber, steps) fact =
    let key = factKey fact

    match Map.tryFind key numbers with
    | Some number -> number, (numbers, nextNumber, steps)
    | None ->
      let number = nextNumber
      let numbers = Map.add key number numbers
      let derivation = Map.tryFind key provenance.Derivations

      let premiseNumbers, state =
        derivation
        |> Option.map (fun derivation -> derivation.Premises)
        |> Option.defaultValue []
        |> List.fold
          (fun (premiseNumbers, state) premise ->
            let premiseNumber, state = visit state premise
            premiseNumber :: premiseNumbers, state)
          ([], (numbers, nextNumber + 1, steps))

      let numbers, nextNumber, steps = state

      let step =
        { Number = number
          Fact = fact
          Derivation = derivation
          PremiseNumbers = List.rev premiseNumbers }

      number, (numbers, nextNumber, step :: steps)

  let _, (_, _, steps) = visit (Map.empty, 1, []) root
  List.sortBy (fun step -> step.Number) steps

let private identities provenance typeId =
  Map.tryFind typeId provenance.TypeNames |> Option.defaultValue []

let private formatPremises step =
  match step.PremiseNumbers with
  | [] -> []
  | numbers ->
    [ sprintf
        "Premises: %s"
        (numbers |> List.map (sprintf "Step %d") |> String.concat ", ") ]

let private formatSSA provenance originId =
  match
    originId |> Option.bind (fun id -> Map.tryFind id provenance.Origins)
  with
  | None -> []
  | Some origin ->
    let annotation =
      if System.String.IsNullOrWhiteSpace origin.Annotation then
        ""
      else
        sprintf " (%s)" origin.Annotation

    [ sprintf
        "SSA: %s (%s) %s%s"
        origin.FunctionName
        origin.Location
        origin.Statement
        annotation ]

let private formatStep provenance step =
  let header = sprintf "Step %d" step.Number
  let targetType = sprintf "TargetType: %s" (factString step.Fact)

  let targetIdentity =
    match identities provenance step.Fact.TypeId with
    | [] -> "TargetIdentity: <not mapped to an SSA variable>"
    | names -> sprintf "TargetIdentity: %s" (String.concat ", " names)

  match step.Derivation with
  | None -> [ header; targetIdentity; targetType; ""; "SSA: <not recorded>" ]
  | Some derivation when List.isEmpty derivation.Premises ->
    [ header; targetIdentity; targetType; "" ]
    @ formatSSA provenance derivation.OriginId
  | Some derivation ->
    [ header
      targetIdentity
      targetType
      ""
      sprintf "TypeConstraint: %s" derivation.Constraint ]
    @ formatSSA provenance derivation.OriginId
    @ formatPremises step

let private explainSource provenance function_ source typeId =
  let root = { Type = Address; TypeId = typeId }
  let steps = buildSteps provenance root

  [ sprintf "Inferred source: %s -> %s" (sourceString source) (factString root)
    "Propagation" ]
  @ (steps |> List.collect (fun step -> formatStep provenance step @ [ "" ]))

let private explainResult provenance result =
  let header =
    sprintf
      "Target: %s - %s (%s)"
      (targetString result.Target)
      result.Function.Name
      result.Function.Address

  match Map.tryFind result.Function.Address provenance.Functions with
  | None -> [ header; "Function provenance is missing." ]
  | Some function_ ->
    let sources =
      result.Sources
      |> List.choose (fun source ->
        trySourceTypeId function_ source
        |> Option.map (fun typeId -> source, typeId))

    let addressSources =
      sources
      |> List.filter (fun (_, typeId) ->
        Map.containsKey
          (factKey { Type = Address; TypeId = typeId })
          provenance.Derivations)

    let selected =
      if List.isEmpty addressSources then
        sources
      else
        addressSources

    if List.isEmpty selected then
      [ header; "No inferred TypeId is mapped to this target." ]
    else
      header
      :: (selected
          |> List.collect (fun (source, typeId) ->
            explainSource provenance function_ source typeId))

/// Explain every FP using its compact proof.
let toText provenance results =
  let falsePositives =
    results
    |> List.filter (fun result ->
      result.GT = Value
      && (result.Inferred = Address || result.Inferred = Conflict))

  let body =
    if List.isEmpty falsePositives then
      [ "<none>" ]
    else
      falsePositives
      |> List.collect (fun result -> explainResult provenance result @ [ "" ])

  String.concat "\n" ("====== False Positive Reasons ======" :: body) + "\n"
