namespace PointerAnalyzer.Domain.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsInt

[<TestClass>]
type AbsIntTests () =

  let absInt = AbsIntDomain.create ArchX86_32
  let nUInt = (IntTypesLoader.load ArchX86_32).NUInt

  let constInt value = absInt.ofNUInt (nUInt.OfInt value)

  [<TestMethod>]
  member _.``Concrete Addition`` () =
    let actual = absInt.add (constInt 2) (constInt 3)

    Assert.AreEqual<string> ("0x5", absInt.toString actual)
    Assert.IsTrue (absInt.isConst actual)
    Assert.AreEqual<NUInt> (nUInt.OfInt 5, absInt.getConst actual)

  [<TestMethod>]
  member _.``Concrete Subtraction`` () =
    let actual = absInt.sub (constInt 4) (constInt 3)

    Assert.AreEqual<string> ("0x1", absInt.toString actual)
    Assert.AreEqual<NUInt> (nUInt.One, absInt.getConst actual)

  [<TestMethod>]
  member _.``Concrete Multiplication`` () =
    let actual = absInt.mul (constInt 3) (constInt 4)

    Assert.AreEqual<string> ("0xC", absInt.toString actual)
    Assert.AreEqual<NUInt> (nUInt.OfInt 12, absInt.getConst actual)

  [<TestMethod>]
  member _.``Shift Left`` () =
    let actual = absInt.shl (constInt 3) (constInt 1)

    Assert.AreEqual<string> ("0x6", absInt.toString actual)
    Assert.AreEqual<NUInt> (nUInt.OfInt 6, absInt.getConst actual)

  [<TestMethod>]
  member _.``Symbolic Addition`` () =
    let actual = absInt.add (absInt.ofSymbol (SymVal 0)) (constInt 3)

    Assert.IsTrue (absInt.isSymbolic actual)
    Assert.AreEqual<string> ("SymVal(0) + 0x3", absInt.toString actual)
    Assert.AreEqual<SymbolInt> (SymVal 0, absInt.getSymbol actual)
    Assert.AreEqual<NUInt> (nUInt.OfInt 3, absInt.getConstPart actual)
