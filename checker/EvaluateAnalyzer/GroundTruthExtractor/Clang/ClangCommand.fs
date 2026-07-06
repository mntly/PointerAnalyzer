module EvaluateAnalyzer.GroundTruthExtractor.Clang.ClangCommand

open System
open System.Diagnostics
open System.IO

type ClangResult =
  { ExitCode: int
    Stdout: string
    Stderr: string }

let private runProcess fileName args =
  let psi = ProcessStartInfo ()
  psi.FileName <- fileName
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true

  args |> List.iter (fun arg -> psi.ArgumentList.Add arg)

  use proc = new Process ()
  proc.StartInfo <- psi

  try
    proc.Start () |> ignore
  with
  | :? System.ComponentModel.Win32Exception as ex ->
    failwithf
      "clang is not installed or is not available in PATH. Please install clang and retry. Detail: %s"
      ex.Message

  let stdout = proc.StandardOutput.ReadToEnd ()
  let stderr = proc.StandardError.ReadToEnd ()
  proc.WaitForExit ()

  { ExitCode = proc.ExitCode
    Stdout = stdout
    Stderr = stderr }

let ensureInstalled () =
  let result = runProcess "clang" [ "--version" ]

  if result.ExitCode <> 0 then
    failwith
      "clang is not installed or is not available in PATH. Please install clang and retry."

let astDumpJson clangArgs (sourcePath: string) =
  let ext = Path.GetExtension(sourcePath).ToLowerInvariant ()

  let languageArgs =
    if ext = ".h" then
      [ "-x"; "c-header" ]
    else
      []

  let args =
    [ "-fsyntax-only"; "-Xclang"; "-ast-dump=json" ]
    @ languageArgs
    @ clangArgs
    @ [ sourcePath ]

  runProcess "clang" args
