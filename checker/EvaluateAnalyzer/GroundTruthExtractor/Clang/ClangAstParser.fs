module EvaluateAnalyzer.GroundTruthExtractor.Clang.ClangAstParser

open System
open System.IO
open System.Text.Json
open EvaluateAnalyzer.GroundTruthExtractor.C.CSignatureParser

let private tryProperty (name: string) (element: JsonElement) =
  match element.TryGetProperty name with
  | true, value -> Some value
  | false, _ -> None

let private tryStringProperty (name: string) (element: JsonElement) =
  tryProperty name element
  |> Option.bind (fun value ->
    if value.ValueKind = JsonValueKind.String then
      Some (value.GetString ())
    else
      None)

let private tryBoolProperty (name: string) (element: JsonElement) =
  tryProperty name element
  |> Option.bind (fun value ->
    match value.ValueKind with
    | JsonValueKind.True -> Some true
    | JsonValueKind.False -> Some false
    | _ -> None)

let private qualType (element: JsonElement) =
  tryProperty "type" element
  |> Option.bind (tryStringProperty "qualType")

let private locFile (element: JsonElement) =
  let rec loop loc =
    match tryStringProperty "file" loc with
    | Some file -> Some file
    | None ->
      tryProperty "includedFrom" loc
      |> Option.bind loop

  tryProperty "loc" element |> Option.bind loop

let private relativeSource (libRoot: string) (fallbackSource: string) element =
  let source = locFile element |> Option.defaultValue fallbackSource

  if Path.IsPathRooted source && source.StartsWith(libRoot) then
    Path.GetRelativePath(libRoot, source).Replace('\\', '/')
  else
    fallbackSource

let private directInner (element: JsonElement) =
  match tryProperty "inner" element with
  | Some inner when inner.ValueKind = JsonValueKind.Array ->
    inner.EnumerateArray () |> Seq.toList
  | _ -> []

let private parseReturnType (functionType: string) =
  match functionType.IndexOf " (" with
  | idx when idx > 0 -> functionType.Substring(0, idx).Trim ()
  | _ ->
    match functionType.IndexOf "(" with
    | idx when idx > 0 -> functionType.Substring(0, idx).Trim ()
    | _ -> functionType.Trim ()

let private isFunctionDecl (element: JsonElement) =
  tryStringProperty "kind" element = Some "FunctionDecl"

let private isParamDecl (element: JsonElement) =
  tryStringProperty "kind" element = Some "ParmVarDecl"

let private parseFunction libRoot fallbackSource (element: JsonElement) =
  if not (isFunctionDecl element) then
    None
  else
    let isImplicit = tryBoolProperty "isImplicit" element |> Option.defaultValue false
    let isInvalid = tryBoolProperty "isInvalid" element |> Option.defaultValue false

    match tryStringProperty "name" element, qualType element with
    | Some name, Some functionType
        when not isImplicit && not isInvalid && not (String.IsNullOrWhiteSpace name) ->
      let parameters =
        directInner element
        |> List.filter isParamDecl
        |> List.mapi (fun index param ->
          let name =
            tryStringProperty "name" param
            |> Option.filter (String.IsNullOrWhiteSpace >> not)
            |> Option.defaultValue (sprintf "arg%d" index)

          let ctype = qualType param |> Option.defaultValue "unknown"

          { Name = name
            CType = ctype })

      let returnType = parseReturnType functionType

      let prototype =
        sprintf
          "%s %s(%s)"
          returnType
          name
          (parameters
           |> List.map (fun param -> sprintf "%s %s" param.CType param.Name)
           |> String.concat ", ")

      Some
        { Name = name
          Source = relativeSource libRoot fallbackSource element
          Prototype = prototype
          ReturnCType = returnType
          Parameters = parameters }
    | _ -> None

let parseSignatures libRoot fallbackSource (json: string) =
  if String.IsNullOrWhiteSpace json then
    []
  else
    try
      use doc = JsonDocument.Parse json

      let rec collect element acc =
        let acc =
          match parseFunction libRoot fallbackSource element with
          | Some signature -> signature :: acc
          | None -> acc

        directInner element |> List.fold (fun acc child -> collect child acc) acc

      collect doc.RootElement [] |> List.rev
    with
    | :? JsonException -> []
