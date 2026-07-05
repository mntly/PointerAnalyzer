module EvaluateAnalyzer.GroundTruthExtractor.ELF.ELFSymbolReader

open System
open System.Diagnostics
open EvaluateAnalyzer.GroundTruthExtractor.Profile.ExtractionProfile

let private runReadElf binaryPath =
  let startInfo =
    ProcessStartInfo (
      FileName = "readelf",
      Arguments = sprintf "-Ws %s" binaryPath,
      RedirectStandardOutput = true,
      RedirectStandardError = true,
      UseShellExecute = false
    )

  use proc = new Process (StartInfo = startInfo)
  proc.Start () |> ignore
  let stdout = proc.StandardOutput.ReadToEnd ()
  let stderr = proc.StandardError.ReadToEnd ()
  proc.WaitForExit ()

  if proc.ExitCode = 0 then
    stdout
  else
    failwithf "readelf failed with exit code %d: %s" proc.ExitCode stderr

/// Extract functions in given binary using readelf
let functionNames binaryPath =
  let lines =
    (runReadElf binaryPath)
      .Split ([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)

  lines
  |> Seq.choose (fun line ->
    let parts =
      line.Split ([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

    if parts.Length >= 8 && parts[3] = "FUNC" && parts[6] <> "UND" then
      Some parts[7]
    else
      None)
  |> Set.ofSeq

let profile =
  { Name = "ELF"
    FunctionNames = functionNames }
