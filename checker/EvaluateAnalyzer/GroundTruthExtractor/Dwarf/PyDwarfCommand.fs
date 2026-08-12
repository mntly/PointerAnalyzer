module EvaluateAnalyzer.GroundTruthExtractor.Dwarf.PyDwarfCommand

open System
open System.ComponentModel
open System.Diagnostics
open System.IO

/// Path of GroundTruthExtractor based on checker
let private gtExtractorPath =
  Path.Combine ("EvaluateAnalyzer", "GroundTruthExtractor")

/// Path of python script to parsing DWARF based on checker
let private pythonScriptPath =
  Path.Combine (gtExtractorPath, "Dwarf", "extract_dwarf_gt.py")

/// Path of python venv used for execute pythonscript based on checker

let private venvPythonPath =
  Path.Combine (gtExtractorPath, ".venv", "bin", "python")

/// Get path of python script
let private findScript () =
  let candidates =
    [ Path.Combine (Environment.CurrentDirectory, pythonScriptPath)
      Path.Combine (Environment.CurrentDirectory, "checker", pythonScriptPath)
      Path.Combine (AppContext.BaseDirectory, pythonScriptPath) ]

  candidates |> List.tryFind File.Exists

/// Get path of python in venv
/// If fail to find, used system python3
let private findPython () =
  let candidates =
    [ Path.Combine (Environment.CurrentDirectory, venvPythonPath)
      Path.Combine (Environment.CurrentDirectory, "checker", venvPythonPath)
      Path.Combine (AppContext.BaseDirectory, venvPythonPath) ]

  candidates |> List.tryFind File.Exists |> Option.defaultValue "python3"

let run binaryPath logPath whiteListPath =
  (* Check ground truth binary exists *)
  if not (File.Exists binaryPath) then
    failwithf "ground-truth binary does not exist: %s" binaryPath

  match whiteListPath with
  | Some path when not (File.Exists path) ->
    failwithf "function whitelist does not exist: %s" path
  | _ -> ()

  (* Get python script for parsing DWARF *)
  let scriptPath =
    match findScript () with
    | Some path -> path
    | None ->
      failwith
        "extract_dwarf_gt.py was not found under checker/EvaluateAnalyzer/GroundTruthExtractor/Dwarf."

  (* Execute python script to extract ground truth *)
  let startInfo = ProcessStartInfo ()
  startInfo.FileName <- findPython ()
  startInfo.ArgumentList.Add scriptPath
  startInfo.ArgumentList.Add binaryPath

  match logPath with
  | Some path ->
    startInfo.ArgumentList.Add "--log"
    startInfo.ArgumentList.Add path
  | None -> ()

  match whiteListPath with
  | Some path ->
    startInfo.ArgumentList.Add "--whitelist"
    startInfo.ArgumentList.Add path
  | None -> ()

  startInfo.RedirectStandardOutput <- true
  startInfo.RedirectStandardError <- true
  startInfo.UseShellExecute <- false

  try
    use proc = Process.Start startInfo
    let stdout = proc.StandardOutput.ReadToEnd ()
    let stderr = proc.StandardError.ReadToEnd ()
    proc.WaitForExit ()

    if proc.ExitCode <> 0 then failwith stderr else stdout
  with :? Win32Exception as ex ->
    failwithf
      "python3 is not installed or not in PATH. GroundTruthExtractor also requires GNU readelf. Run checker/EvaluateAnalyzer/GroundTruthExtractor/setup_venv.sh first. %s"
      ex.Message
