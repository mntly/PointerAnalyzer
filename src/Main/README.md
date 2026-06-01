# Prev main
module PointerAnalyzer.Main

// open PointerAnalyzer
open PointerAnalyzer.Utils

let printUsage () =
    println "Usage: 'dotnet [WhichRun???] <infer|???> (options...)'"
    println "  infer: Analyze type of syscall arguments."
    println "        Use 'dotnet [WhichRun???] infer --help' for details."
    println "  ???: Which Task needs???"
    println "        Use 'dotnet [WhichRun???] [???] --help' for details."

let runMode (mode: string) args =
    match mode.ToLower() with
    | "type" -> TypeInference.run args
    // | "???" -> CodeGenerate.run args
    | _ -> printUsage ()

[<EntryPoint>]
let main argv =
    if Array.length argv <= 1 then
        printUsage ()
        1
    else
        runMode argv.[0] argv.[1..]
        0
