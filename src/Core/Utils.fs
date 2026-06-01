module PointerAnalyzer.Utils

open System
open System.Text.RegularExpressions

let private random = System.Random()

let print (s: string) = Console.Write(s)

let println (s: string) = Console.WriteLine(s)

let private logInternal logType fmt =
    let headerStr = sprintf "[System:%s] " logType
    Printf.kprintf (fun str -> println <| headerStr + str) fmt

let logInfo fmt = logInternal "Progress" fmt

// let logWarning fmt = logInternal "Warning" fmt

// let logError fmt = logInternal "Error" fmt

// let readInput () = System.Console.ReadLine()

let checkFileExist file =
    if not (System.IO.File.Exists(file)) then
        printfn "Target file ('%s') does not exist" file
        exit 1
