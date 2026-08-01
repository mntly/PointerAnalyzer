module EvaluateAnalyzer.GroundTruthExtractor.Builder

open EvaluateAnalyzer.GroundTruthExtractor.Dwarf

/// <summary>
/// Options used for GroundTruthExtractor.
/// </summary>
/// <remarks>
/// <c>GroundTruthBinary</c> is path of ground truth binary compiled with debug
/// option.
/// <c>OutputSuffix</c> is user given output file name. If user does not give
/// any name, this is set to basename of given binary.
/// <c>LogFilePath</c> is path of log file to store strange log during parsing
/// DWARF.
/// <c>WhiteListPath</c> optionally selects the functions stored in the result.
/// The file contains one function name per line.
/// </remarks>
type BuildOptions =
  { GroundTruthBinary: string
    OutputSuffix: string
    LogFilePath: string option
    WhiteListPath: string option }

/// Run GroundTruthExtractor with given BuildOptions
let build options =
  PyDwarfCommand.run
    options.GroundTruthBinary
    options.LogFilePath
    options.WhiteListPath

/// Generate new output file name
let outputFileName outputSuffix = outputSuffix + "_GT.json"

/// Generate new log file name
let logFileName outputSuffix = outputSuffix + "_GTExtract.log"

/// Since python parser returns JSON foramt, this function just propagate it
let toJson (json: string) = json
