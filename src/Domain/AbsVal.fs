module PointerAnalyzer.AbsDom.AbsVal

open PointerAnalyzer
open PointerAnalyzer.AbsDom.Signature
open PointerAnalyzer.AbsDom.AbsInt
open PointerAnalyzer.AbsDom.AbsLoc
open PointerAnalyzer.AbsDom.TypeMap

type AbsVal = AbsInt * AbsLocSet * TypePtr

type AbsValModule (architecture: Architecture) =
  inherit AbstractDomain<AbsVal> ()

  let absInt = AbsIntDomain.create architecture
  let absLocSet = AbsLocDomain.createSet architecture
  let intTypes = IntTypesLoader.load architecture
  let nUInt = intTypes.NUInt

  let mk intPart locPart typePtr = intPart, locPart, typePtr

  member __.architecture = architecture
  member __.AbsInt = absInt
  member __.AbsLocSet = absLocSet

  override __.bot = mk absInt.bot absLocSet.bot TypePtr.bot

  override __.leq x y =
    let ix, lx, tx = x
    let iy, ly, ty = y
    absInt.leq ix iy && absLocSet.leq lx ly && tx = ty

  override __.join x y =
    let ix, lx, tx = x
    let iy, ly, ty = y
    let t = if tx = ty then tx else TypePtr.bot
    mk (absInt.join ix iy) (absLocSet.join lx ly) t

  override __.toString x =
    let intPart, locPart, typePtr = x

    sprintf
      "<%s, %s, %s>"
      (absInt.toString intPart)
      (absLocSet.toString locPart)
      (TypePtr.toString typePtr)

  override __.isBot x =
    let intPart, locPart, typePtr = x
    absInt.isBot intPart && absLocSet.isBot locPart && typePtr = TypePtr.bot

  member __.getInt (x: AbsVal) =
    let intPart, _, _ = x
    intPart

  member __.getLoc (x: AbsVal) =
    let _, locPart, _ = x
    locPart

  member __.getTypePtr (x: AbsVal) =
    let _, _, typePtr = x
    typePtr

  member __.setInt intPart (x: AbsVal) =
    let _, locPart, typePtr = x
    mk intPart locPart typePtr

  member __.setLoc locPart (x: AbsVal) =
    let intPart, _, typePtr = x
    mk intPart locPart typePtr

  member __.setTypePtr typePtr (x: AbsVal) =
    let intPart, locPart, _ = x
    mk intPart locPart typePtr

  member __.ofUInt64 width i64 : AbsVal =
    let absInt = absInt.ofNUInt i64
    let lowBound = nUInt.Zero
    let upperBound = nUInt.MaxLikelyInt

    let typePtr =
      if width < nUInt.WordWidth then
        TypePtr.baseVal
      elif lowBound < i64 && i64 < upperBound then
        TypePtr.baseVal
      else
        TypePtr.bot

    __.bot |> __.setInt absInt |> __.setTypePtr typePtr

  member __.ofLoc loc =
    mk absInt.bot (absLocSet.make [ loc ]) TypePtr.bot

  member __.ofTypePtr ptr =
    mk absInt.bot absLocSet.bot ptr

  member __.unknownInt =
    mk absInt.top absLocSet.bot TypePtr.bot

  member __.unknownGlobal =
    mk absInt.top absLocSet.bot TypePtr.bot

  member __.zero =
    mk absInt.zero absLocSet.bot TypePtr.bot

  member __.isZero x =
    absInt.isZero (__.getInt x) && absLocSet.isBot (__.getLoc x)

  member __.isConst x =
    absInt.isConst (__.getInt x) && absLocSet.isBot (__.getLoc x)

  member __.isAndMask x =
    absInt.isAndMask (__.getInt x) && absLocSet.isBot (__.getLoc x)

module AbsValDomain =
  let create architecture = AbsValModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
