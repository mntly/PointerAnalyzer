namespace PointerAnalyzer.Domain.Tests

open B2R2
open B2R2.BinIR
open Microsoft.VisualStudio.TestTools.UnitTesting
open PointerAnalyzer
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeState

[<TestClass>]
type AbsValTests () =
  let domain = AbsValDomain.create ArchX86_32
  let i32 = RegType.fromBitWidth 32
  let constant value = domain.ofBitVector (BitVector (uint64 value, i32))

  [<TestMethod>]
  member _.``Concrete addition`` () =
    let actual = domain.binOp BinOpType.ADD (constant 2) (constant 3)
    Assert.AreEqual<string> ("0x5:I32", domain.toString actual)

  [<TestMethod>]
  member _.``Different constants join to top`` () =
    let actual = domain.join (constant 1) (constant 2)
    Assert.AreEqual<AbsVal> (Top, actual)

  [<TestMethod>]
  member _.``Bottom is identity for join`` () =
    let expected = constant 7
    Assert.AreEqual<AbsVal> (expected, domain.join Bot expected)

  [<TestMethod>]
  member _.``Stored value is loaded from destination memory version`` () =
    let stateDomain = AnalysisStateDomain.createDefault ArchX86_32
    let address = constant 0x2000
    let value = constant 0x1234

    let storedTypeId, state =
      stateDomain.store 0 1 address value stateDomain.bot

    let loaded, loadedTypeId, _ = stateDomain.load 1 address state

    Assert.AreEqual<AbsVal> (value, loaded)
    Assert.AreEqual<int option> (storedTypeId, loadedTypeId)

  [<TestMethod>]
  member _.``Every store receives a fresh type ID`` () =
    let stateDomain = AnalysisStateDomain.createDefault ArchX86_32
    let address = constant 0x2000

    let firstTypeId, state =
      stateDomain.store 0 1 address (constant 10) stateDomain.bot

    let secondTypeId, _ =
      stateDomain.store 1 2 address (constant 20) state

    Assert.IsTrue firstTypeId.IsSome
    Assert.IsTrue secondTypeId.IsSome
    Assert.AreNotEqual<int option> (firstTypeId, secondTypeId)

  [<TestMethod>]
  member _.``Add constraint infers address result`` () =
    let typeDomain = TypeStateDomain.createDefault ()

    let state =
      typeDomain.bot
      |> typeDomain.addAddress 0
      |> typeDomain.addValue 1
      |> typeDomain.addAddResult 2 0 1
      |> typeDomain.solve

    Assert.IsTrue (Set.contains (Address 2) state.Constraints)

  [<TestMethod>]
  member _.``Same constraints propagate transitively`` () =
    let typeDomain = TypeStateDomain.createDefault ()

    let state =
      typeDomain.bot
      |> typeDomain.addAddress 0
      |> typeDomain.addSame [ 0; 1 ]
      |> typeDomain.addSame [ 1; 2 ]
      |> typeDomain.solve

    Assert.IsTrue (Set.contains (Address 2) state.Constraints)

  [<TestMethod>]
  member _.``Address and value produce conflict`` () =
    let typeDomain = TypeStateDomain.createDefault ()

    let state =
      typeDomain.bot
      |> typeDomain.addAddress 0
      |> typeDomain.addValue 0
      |> typeDomain.solve

    Assert.IsTrue (Set.contains 0 state.Conflicts)
