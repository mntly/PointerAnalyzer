module EvaluateAnalyzer.GroundTruthExtractor.C.CSignatureParser

open System
open System.Text.RegularExpressions

type ParsedParameter = { Name: string; CType: string }

/// <summary>
/// Represents the parsed type of function/function signature.
/// </summary>
/// <remarks>
/// <c>Name</c> is the function name.
/// <c>Source</c> is the relative path from corresponding library root.
/// <c>Prototype</c> is the definition-form of parsed function signature.
/// <c>ReturnCType</c> is type of return value.
/// <c>Parameters</c> is type of parameters.
/// </remarks>
type ParsedSignature =
  { Name: string
    Source: string
    Prototype: string
    ReturnCType: string
    Parameters: ParsedParameter list }

let removeComments (text: string) =
  let withoutBlock =
    Regex.Replace (text, @"/\*.*?\*/", "", RegexOptions.Singleline)

  Regex.Replace (withoutBlock, @"//.*?$", "", RegexOptions.Multiline)

/// Remove PreprocessorDirectives such as `  #*`
let removePreprocessorDirectives (text: string) =
  Regex.Replace (text, @"^\s*#.*$", "", RegexOptions.Multiline)

/// Change continuous blanks into single blank
let normalizeWhitespace (text: string) =
  Regex.Replace(text, @"\s+", " ").Trim ()

/// Predefined keywords for resolving mismatching cases
let private ignoredNames =
  set [ "if"; "for"; "while"; "switch"; "return"; "sizeof" ]

/// Split params by preserving its type such as int a
let private splitParameters (text: string) =
  (* Extract parameter and its types *)
  let rec loop idx depth start acc =
    if idx >= text.Length then
      text.Substring start :: acc |> List.rev
    else
      match text[idx] with
      | '(' -> loop (idx + 1) (depth + 1) start acc
      | ')' -> loop (idx + 1) (max 0 (depth - 1)) start acc
      | ',' when depth = 0 ->
        let part = text.Substring (start, idx - start)
        loop (idx + 1) depth (idx + 1) (part :: acc)
      | _ -> loop (idx + 1) depth start acc

  loop 0 0 0 []
  |> List.map normalizeWhitespace
  |> List.filter (String.IsNullOrWhiteSpace >> not)

/// Parse parameter string and construct ParsedParameter
let private parseParameter index (param: string) =
  if param = "void" || param = "..." then
    (* Handling 0 or variable parameters *)
    None
  else
    (* Remove __attribute__ and normalize *)
    let param = Regex.Replace (param, @"\b__attribute__\s*\(\(.*?\)\)", "")
    let param = normalizeWhitespace param

    (* Extract type and name of parameters *)
    (* In addition, parse array information([...]) *)
    let m =
      Regex.Match (
        param,
        @"^(?<type>.+?)(?:\s+|\s*\*+\s*)(?<name>[A-Za-z_]\w*)(?<array>\s*\[[^\]]*\])?$"
      )

    if m.Success then
      let name = m.Groups["name"].Value
      let rawType = m.Groups["type"].Value
      let arraySuffix = m.Groups["array"].Value

      let ctype =
        if param.Contains ("*" + name) && not (rawType.Contains "*") then
          (* Handle `char *but`, since it is parsed to `char` and `*but` *)
          rawType + " *" + arraySuffix
        else
          rawType + arraySuffix |> normalizeWhitespace

      Some { Name = name; CType = ctype }
    else
      Some
        { Name = sprintf "arg%d" index
          CType = param }

/// Regex for detecting first line of function definition or declaration
/// and parse it to ret name(params)
let private signatureRegex =
  Regex (
    @"(?<ret>[A-Za-z_][A-Za-z0-9_\s\*\(\)]*?)\s+(?<name>[A-Za-z_]\w*)\s*\((?<params>[^;{}]*)\)\s*(?:;|\{)",
    RegexOptions.Compiled
  )

/// Given codes, parse and extract the function siganture if corresponding
/// function is in target functions.
let parseSignatures sourcePath (funcNames: Set<string> option) codes =
  let text = removeComments codes
  let pureCodes = removePreprocessorDirectives text

  signatureRegex.Matches pureCodes
  |> Seq.cast<Match>
  |> Seq.choose (fun m ->
    (* Get parsed result *)
    let name = m.Groups["name"].Value
    let ret = normalizeWhitespace m.Groups["ret"].Value
    let paramText = m.Groups["params"].Value

    (* Only consider about target functions *)
    let isTarget =
      match funcNames with
      | Some targets -> Set.contains name targets
      | None -> true

    if not isTarget || Set.contains name ignoredNames then
      None
    else
      (* Parse and construct the type of parameters *)
      let parameters =
        splitParameters paramText |> List.mapi parseParameter |> List.choose id

      (* Form extract function signature to its definition *)
      let prototype =
        sprintf
          "%s %s(%s)"
          ret
          name
          (parameters
           |> List.map (fun p ->
             if String.IsNullOrWhiteSpace p.Name then
               p.CType
             else
               sprintf "%s %s" p.CType p.Name)
           |> String.concat ", ")

      Some
        { Name = name
          Source = sourcePath
          Prototype = prototype
          ReturnCType = ret
          Parameters = parameters })
  |> Seq.toList
