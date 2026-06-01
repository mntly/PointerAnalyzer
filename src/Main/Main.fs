module PointerAnalyzer.Main

open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsInt

let private printResult name value = printfn "%s = %s" name value

let private runAbsIntSmokeTest architecture =
  let absInt = AbsIntDomain.create architecture
  let nUInt = (IntTypesLoader.load architecture).NUInt

  let two = absInt.ofNUInt (nUInt.OfInt 2)
  let three = absInt.ofNUInt (nUInt.OfInt 3)
  let four = absInt.ofNUInt (nUInt.OfInt 4)
  let one = absInt.ofNUInt nUInt.One
  let sym = absInt.ofSymbol (SymVal 0)

  printfn "Architecture = %A" architecture
  printResult "2 + 3" (absInt.add two three |> absInt.toString)
  printResult "4 - 3" (absInt.sub four three |> absInt.toString)
  printResult "3 * 4" (absInt.mul three four |> absInt.toString)
  printResult "3 << 1" (absInt.shl three one |> absInt.toString)
  printResult "SymVal(0) + 3" (absInt.add sym three |> absInt.toString)

let private runAbsLocSmokeTest architecture =
  let absInt = AbsIntDomain.create architecture
  let nUInt = (IntTypesLoader.load architecture).NUInt

  let two = absInt.ofNUInt (nUInt.OfInt 2)
  let three = absInt.ofNUInt (nUInt.OfInt 3)
  let four = absInt.ofNUInt (nUInt.OfInt 4)
  let one = absInt.ofNUInt nUInt.One
  let sym = absInt.ofSymbol (SymVal 0)

  printfn "Architecture = %A" architecture
  printResult "2 + 3" (absInt.add two three |> absInt.toString)
  printResult "4 - 3" (absInt.sub four three |> absInt.toString)
  printResult "3 * 4" (absInt.mul three four |> absInt.toString)
  printResult "3 << 1" (absInt.shl three one |> absInt.toString)
  printResult "SymVal(0) + 3" (absInt.add sym three |> absInt.toString)

[<EntryPoint>]
let main argv =
  let architecture =
    if Array.isEmpty argv then
      Architecture.ofString "x86_32"
    else
      Architecture.ofString argv[0]

  runAbsIntSmokeTest architecture
  0
