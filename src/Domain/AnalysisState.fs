module PointerAnalyzer.AbsDom.AnalysisState

open PointerAnalyzer
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.AbsDom.TypeState
open PointerAnalyzer.AbsDom.SymbolConstraint
open PointerAnalyzer.AbsDom.AbsMem
open PointerAnalyzer.AbsDom.RegMap

type AnalysisState =
  { Memory: AbsMem
    Registers: RegMap
    TypeState: TypeState
    SymbolIdx: SymbolIdx
    SymbolConstraints: SymbolConstraintMap }

type AnalysisStateModule
  (
    architecture: Architecture,
    startTypePtr: int,
    startSymIntIdx: int,
    startSymLocIdx: int
  ) =
  let typeState = TypeStateDomain.create startTypePtr
  let symbolConstraint = SymbolConstraintDomain.create architecture
  let absMem = AbsMemDomain.create architecture
  let regMap = RegMapDomain.create architecture

  member __.TypeState = typeState
  member __.SymbolConstraint = symbolConstraint
  member __.AbsMem = absMem
  member __.RegMap = regMap

  member __.bot =
    { Memory = absMem.bot
      Registers = regMap.bot
      TypeState = typeState.bot
      SymbolIdx = SymbolIdx.create startSymIntIdx startSymLocIdx
      SymbolConstraints = symbolConstraint.bot }

  member __.freshIntSymbol state =
    let sym, symbolIdx = SymbolIdx.freshInt state.SymbolIdx
    sym, { state with SymbolIdx = symbolIdx }

  member __.freshLocSymbol state =
    let sym, symbolIdx = SymbolIdx.freshLoc state.SymbolIdx
    sym, { state with SymbolIdx = symbolIdx }

  member __.addIntSymbolConstraint sym value state =
    { state with
        SymbolConstraints =
          symbolConstraint.addInt sym value state.SymbolConstraints }

  member __.addIntSymbolBot sym state =
    { state with
        SymbolConstraints =
          symbolConstraint.addIntBot sym state.SymbolConstraints }

  member __.addLocSymbolConstraint sym value state =
    { state with
        SymbolConstraints =
          symbolConstraint.addLocSet sym value state.SymbolConstraints }

  member __.load locs state = absMem.findSet locs state.Memory

  member __.store locs value state =
    { state with
        Memory = absMem.store locs value state.Memory }

  member __.findReg reg state =
    regMap.find reg state.Registers

  member __.setReg reg value state =
    { state with Registers = regMap.add reg value state.Registers }

  member __.joinReg reg value state =
    let old = regMap.find reg state.Registers
    __.setReg reg (regMap.joinValue old value) state

  member __.join x y =
    { TypeState = typeState.join x.TypeState y.TypeState
      Registers = regMap.join x.Registers y.Registers
      SymbolIdx =
        { NextSymIntIdx =
            max x.SymbolIdx.NextSymIntIdx y.SymbolIdx.NextSymIntIdx
          NextSymLocIdx =
            max x.SymbolIdx.NextSymLocIdx y.SymbolIdx.NextSymLocIdx }
      SymbolConstraints =
        symbolConstraint.join x.SymbolConstraints y.SymbolConstraints
      Memory = absMem.join x.Memory y.Memory }

module AnalysisStateDomain =
  let create architecture startTypePtr startSymIntIdx startSymLocIdx =
    AnalysisStateModule (
      architecture,
      startTypePtr,
      startSymIntIdx,
      startSymLocIdx
    )

  let createDefault architecture =
    create architecture TypePtr.firstFreshId 0 0
