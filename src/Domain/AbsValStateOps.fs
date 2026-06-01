module PointerAnalyzer.AbsDom.AbsValStateOps

open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.TypeMap
open PointerAnalyzer.AbsDom.TypeState
open PointerAnalyzer.AbsDom.AnalysisState

type AbsValModule with
  member __.joinWithState (state: AnalysisState) x y =
    let typeState = TypeStateDomain.create TypePtr.firstFreshId
    let ix, lx, tx = x
    let iy, ly, ty = y

    let typePtr, typeState' =
      if tx = ty then
        tx, state.TypeState
      else
        let resultPtr, typeState' = typeState.fresh state.TypeState
        let typeState' =
          typeState.addSame (Set.ofList [ resultPtr; tx; ty ]) typeState'

        resultPtr, typeState'

    { state with TypeState = typeState' },
    (__.AbsInt.join ix iy, __.AbsLocSet.join lx ly, typePtr)

  member __.add (state: AnalysisState) x y =
    let typeState = TypeStateDomain.create TypePtr.firstFreshId
    let ix, lx, tx = x
    let iy, ly, ty = y
    let resultPtr, typeState' = typeState.fresh state.TypeState

    let typeState' = typeState.addAddResult resultPtr tx ty typeState'

    { state with TypeState = typeState' },
    (__.AbsInt.add ix iy, __.AbsLocSet.join lx ly, resultPtr)

  member __.sub (state: AnalysisState) x y =
    let typeState = TypeStateDomain.create TypePtr.firstFreshId
    let ix, lx, tx = x
    let iy, ly, ty = y
    let resultPtr, typeState' = typeState.fresh state.TypeState

    let newTypeState = typeState.addSubResult resultPtr tx ty typeState'

    { state with TypeState = newTypeState },
    (__.AbsInt.sub ix iy, __.AbsLocSet.join lx ly, resultPtr)
