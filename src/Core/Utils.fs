module PointerAnalyzer.Utils

open System
open System.Diagnostics

let formatElapsed (elapsed: TimeSpan) = sprintf "%.3fs" elapsed.TotalSeconds

/// Track the time consumption of given work
let timed trackTime label work =
  if trackTime then
    let stopwatch = Stopwatch.StartNew ()
    let result = work ()
    stopwatch.Stop ()
    printfn "[Time] %s: %s" label (formatElapsed stopwatch.Elapsed)
    result
  else
    work ()
