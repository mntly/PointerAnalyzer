module EvaluateAnalyzer.GroundTruthExtractor.Builder

open System.IO
open System.Text.Json
open EvaluateAnalyzer.GroundTruthExtractor.C.CSignatureParser
open EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile
open EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes

/// <summary>
/// Options determine how to extract ground truth.
/// </summary>
/// <remarks>
/// <c>LibRoot</c> is the root path of library directory to extract ground
/// truth.
/// <c>ExtractMode</c> is
/// <see cref="EvaluateAnalyzer.GroundTruthExtractor.Types.GroundTruthTypes.GroundTruthExtractMode" />
/// to determine the target source code of extraction.
/// <c>TargetBinary</c> is path of target binary used for
/// extract-during-evaluate.
/// <c>SourceProfile</c> contains library specific functions for extracting
/// ground truth type.
/// <c>TargetBinaryProfile</c> contains binary specifc functions for extracting
/// function information when it uses extract-during-evaluate.
/// </remarks>
type BuildOptions =
  { LibRoot: string
    ExtractMode: GroundTruthExtractMode
    TargetBinary: string option
    SourceProfile: SourceExtractionProfile
    TargetBinaryProfile: TargetBinaryProfile }

/// Among aliases, set representative function name
/// The representative name should
/// 1. have type signature information
/// 2. not be internal function
/// 3. have shortest name
/// 4. be alphabetic low name
let private representativeOf
  (signatureMap: Map<string, ParsedSignature list>)
  names
  =
  names
  |> Set.toList
  |> List.sortBy (fun name ->
    let hasSignature = Map.containsKey name signatureMap
    let isInternal = name.StartsWith "_"
    not hasSignature, isInternal, name.Length, name)
  |> List.head

/// Given aliases and function signature, combine related aliases names
let private buildAliasGroups
  aliases
  (signatureMap: Map<string, ParsedSignature list>)
  : AliasGroupTruth list =
  (* Construct graph to connect alias to related alias set *)
  (*
    Ex: A - B, B - C, ...
    Graph: {A:{B}, B:{A, C}, ...}
  *)
  let addEdge left right graph =
    let addNeighbor src dst graph =
      let neighbors =
        match graph |> Map.tryFind src with
        | Some graphSet -> graphSet
        | None -> Set.empty

      graph |> Map.add src (Set.add dst neighbors)

    graph |> addNeighbor left right |> addNeighbor right left

  let graph =
    aliases
    |> List.distinct
    |> List.fold
      (fun acc alias -> addEdge alias.Alias alias.CanonicalName acc)
      Map.empty

  (* Collect all names including 1) function name and 2) there aliases *)
  let allNames =
    Seq.concat
      [ signatureMap.Keys :> seq<string>
        aliases
        |> Seq.collect (fun alias -> seq [ alias.Alias; alias.CanonicalName ]) ]
    |> Set.ofSeq

  (*
    For given function name, collects all aliases by iterating connected graph
  *)
  let rec visit members work visited =
    match work with
    | [] -> members, visited
    | name :: rest when Set.contains name visited -> visit members rest visited
    | name :: rest ->
      let neighbors =
        match Map.tryFind name graph with
        | Some vals -> vals
        | None -> Set.empty

      let work = Set.toList neighbors @ rest
      visit (Set.add name members) work (Set.add name visited)

  (* Iterate all nodes in graph to get aliases *)
  let rec loop names visited groups =
    match names with
    | [] -> groups
    | name :: rest when Set.contains name visited -> loop rest visited groups
    | name :: rest ->
      let group, visited = visit Set.empty [ name ] visited
      loop rest visited (group :: groups)

  let groups = loop (Set.toList allNames) Set.empty []

  (* Among each alias set, decide representative name *)
  groups
  |> List.map (fun names ->
    let group: AliasGroupTruth =
      { Representative = representativeOf signatureMap names
        Names = Set.toList names |> List.sort }

    group)
  |> List.sortBy (fun group -> group.Representative)

/// Set mapping from each name to corresponding representative name
let private buildRepresentativeMap (groups: AliasGroupTruth list) =
  groups
  |> List.collect (fun group ->
    group.Names |> List.map (fun name -> name, group.Representative))
  |> Map.ofList

/// Given type signature, construct FunctionTruth
let private toFunctionTruth
  (profile: SourceExtractionProfile)
  canonicalName
  signature
  =
  let returnCType = profile.NormalizeCType signature.ReturnCType
  let returnKind = profile.ClassifyCType returnCType

  let parameters =
    signature.Parameters
    |> List.mapi (fun index param ->
      let ctype = profile.NormalizeCType param.CType

      { Index = index
        Name = param.Name
        CType = ctype
        Kind = profile.ClassifyCType ctype })

  { Name = signature.Name
    CanonicalName = canonicalName
    Source = signature.Source
    Prototype = signature.Prototype
    Return =
      { CType = returnCType
        Kind = returnKind }
    Parameters = parameters }

let private toSignatureTruth profile canonicalName signature =
  let fn = toFunctionTruth profile canonicalName signature

  { Name = fn.Name
    Source = fn.Source
    Prototype = fn.Prototype
    Return = fn.Return
    Parameters = fn.Parameters }

/// Extract normalized return/parameter type shape from a parsed signature.
let private signatureShape profile signature =
  let returnCType = profile.NormalizeCType signature.ReturnCType

  let paramsShape =
    signature.Parameters
    |> List.map (fun param ->
      let ctype = profile.NormalizeCType param.CType
      ctype, profile.ClassifyCType ctype)

  returnCType, profile.ClassifyCType returnCType, paramsShape

let private hasTypeMismatch profile signatures =
  signatures
  |> List.map (signatureShape profile)
  |> List.distinct
  |> List.length
  |> fun len -> len > 1

/// Among aliases, find out mismatched type signature even they are related
/// alias
let private buildTypeMismatches
  profile
  (groups: AliasGroupTruth list)
  signatureMap
  =
  groups
  |> List.filter (fun group -> group.Names.Length > 1)
  |> List.choose (fun group ->
    let signatures =
      group.Names
      |> List.choose (fun name -> Map.tryFind name signatureMap)
      |> List.concat

    if not (hasTypeMismatch profile signatures) then
      None
    else
      let signatureTruths =
        signatures
        |> List.map (toSignatureTruth profile group.Representative)
        |> List.sortBy (fun signature -> signature.Name, signature.Source)

      Some
        { Representative = group.Representative
          Names = group.Names
          Signatures = signatureTruths })

let build options =
  (* Check ground truth library binary exists *)
  if not (Directory.Exists options.LibRoot) then
    failwithf "source root does not exist: %s" options.LibRoot

  (*
    Check extraction mode, and get the functions in given binary if it uses
    extract-during-evaluate mode
  *)
  let funcNames =
    match options.ExtractMode, options.TargetBinary with
    | TargetBinary, Some binaryPath ->
      Some (options.TargetBinaryProfile.FunctionNames binaryPath)
    | TargetBinary, None -> failwith "target-binary mode requires a binary path"
    | AllUClibc, _ -> None

  (* Extract possible .c and .h file from library *)
  let files = options.SourceProfile.GetSourceFiles options.LibRoot

  (* Read given source code and extract GT *)
  let parseFile path =
    (*
      To track the path information, covert path as relative path from library
      root
    *)
    let relative = options.SourceProfile.RelativePath options.LibRoot path
    (* Read file (get source codes) and normalize it *)
    let text = File.ReadAllText path
    let normalized = options.SourceProfile.NormalizeSourceText text
    (* Check there exist alias of function name and store it *)
    let aliases = options.SourceProfile.ExtractAliases normalized
    (*
      Extract Ground Truth type. Target filtering will be applied after alias
      group construction
    *)
    let signatures = parseSignatures relative None normalized

    aliases, signatures

  (* Extract GT from all target files *)
  let aliases, signatures =
    files
    |> List.fold
      (fun (aliasesAcc, sigAcc) path ->
        let aliases, signatures = parseFile path
        aliases @ aliasesAcc, signatures @ sigAcc)
      ([], [])

  (* Mapping from function name and its GT type information *)
  let signatureMap =
    signatures |> List.groupBy (fun signature -> signature.Name) |> Map.ofList

  (* Grouping function names with alias names *)
  let aliasGroups = buildAliasGroups aliases signatureMap
  let representativeMap = buildRepresentativeMap aliasGroups

  let representativeOfName name =
    match Map.tryFind name representativeMap with
    | Some repreName -> repreName
    | None -> name

  (* Filtering only targeted function with name *)
  let namesToEmit =
    match funcNames with
    | Some funcNameSet ->
      funcNameSet |> Set.map representativeOfName |> Set.toList
    | None ->
      aliasGroups
      |> List.map (fun group -> group.Representative)
      |> List.distinct

  (* Among extracted result, filtering *)
  let funFilter
    (functions: FunctionTruth list, missing: MissingTruth list)
    representative
    =
    (* Get function aliases of given function *)
    let group =
      aliasGroups
      |> List.tryFind (fun group -> group.Representative = representative)

    (* Get type signature of given representative function *)
    let candidates =
      match group with
      | Some group ->
        group.Names
        |> List.choose (fun name -> Map.tryFind name signatureMap)
        |> List.concat
      | None ->
        Map.tryFind representative signatureMap |> Option.defaultValue []

    (* Functions only store unique type signature among related alias *)
    if hasTypeMismatch options.SourceProfile candidates then
      functions, missing
    else
      match List.tryHead candidates with
      | Some signature ->
        toFunctionTruth options.SourceProfile representative signature
        :: functions,
        missing
      | None ->
        (* No given representative function exists *)
        functions,
        { Name = representative
          Reason = "signature not found in parsed source profile" }
        :: missing

  let functions, missing = namesToEmit |> List.fold funFilter ([], [])

  let typeMismatches =
    buildTypeMismatches options.SourceProfile aliasGroups signatureMap

  let emittedAliasGroups =
    aliasGroups |> List.filter (fun group -> group.Names.Length > 1)

  { LibRoot = options.LibRoot
    SourceProfile = options.SourceProfile.Name
    TargetProfile = options.TargetBinaryProfile.Name
    Mode = GroundTruthExtractMode.toString options.ExtractMode
    Functions = List.rev functions |> List.sortBy (fun fn -> fn.Name)
    Aliases = aliases |> List.distinct |> List.sortBy (fun a -> a.Alias)
    AliasGroups = emittedAliasGroups
    TypeMismatches = typeMismatches
    Missing = List.rev missing |> List.sortBy (fun item -> item.Name) }

let toJson db =
  let options = JsonSerializerOptions (WriteIndented = true)
  JsonSerializer.Serialize (db, options)
