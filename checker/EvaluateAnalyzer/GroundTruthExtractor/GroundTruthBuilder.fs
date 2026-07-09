module EvaluateAnalyzer.GroundTruthExtractor.Builder

open EvaluateAnalyzer.GroundTruthExtractor.Dwarf

type BuildOptions =
  { GroundTruthBinary: string
    OutputSuffix: string
    LogFilePath: string option }

let build options =

  PyDwarfCommand.run options.GroundTruthBinary options.LogFilePath

let outputFileName options = options.OutputSuffix + "_GT.json"

let logFileName options = options.OutputSuffix + "_GT.log"

let toJson (json: string) = json
