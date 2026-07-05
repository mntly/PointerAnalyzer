module EvaluateAnalyzer.GroundTruthExtractor.C.CTypeClassifier

open System.Text.RegularExpressions

/// <summary>
/// The ground truth type read from library source codes.
/// </summary>

type CTypeKind =
  | Address
  | Value
  | Unknown
  | Void

let toString =
  function
  | Address -> "Address"
  | Value -> "Value"
  | Unknown -> "Unknown"
  | Void -> "Void"

/// Replace continuous blank into single blank
let private normalize (ctype: string) =
  Regex.Replace(ctype, @"\s+", " ").Trim ()

/// Remove type-unrelated keywords and continuous blank
let normalizeCType (ctype: string) =
  Regex.Replace (
    ctype,
    @"\b(const|volatile|restrict|__restrict|__const|register|extern|static|inline)\b",
    ""
  )
  |> normalize

let private knownValueTypes =
  set
    [ "char"
      "signed char"
      "unsigned char"
      "short"
      "short int"
      "signed short"
      "signed short int"
      "unsigned short"
      "unsigned short int"
      "int"
      "signed"
      "signed int"
      "unsigned"
      "unsigned int"
      "long"
      "long int"
      "signed long"
      "signed long int"
      "unsigned long"
      "unsigned long int"
      "long long"
      "long long int"
      "signed long long"
      "signed long long int"
      "unsigned long long"
      "unsigned long long int"
      "size_t"
      "ssize_t"
      "off_t"
      "loff_t"
      "pid_t"
      "uid_t"
      "gid_t"
      "mode_t"
      "time_t"
      "clock_t"
      "clockid_t"
      "uintptr_t"
      "intptr_t"
      "uint8_t"
      "uint16_t"
      "uint32_t"
      "uint64_t"
      "int8_t"
      "int16_t"
      "int32_t"
      "int64_t"
      "bool"
      "_Bool" ]

/// Classify the type of given ctype(type string)
let classify ctype =
  (* Remove type-unrelated keywords *)
  let ctype = normalizeCType ctype

  if ctype = "void" then
    (* void type *)
    Void
  elif ctype.Contains "*" then
    (* Some pointer => Address *)
    Address
  elif Regex.IsMatch (ctype, @"\[[^\]]*\]") then
    (* Some array => Address *)
    Address
  elif ctype.StartsWith "enum " then
    (* enum type => Value*)
    Value
  elif Set.contains ctype knownValueTypes then
    (* Known value types *)
    Value
  // elif ctype.StartsWith "struct " || ctype.StartsWith "union " then
  //   Unknown
  // elif ctype = "" then
  //   Unknown
  else
    (*
      ToDo
        Need to specify other types
    *)
    Unknown
