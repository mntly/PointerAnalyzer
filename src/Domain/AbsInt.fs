module PointerAnalyzer.AbsDom.AbsInt

open PointerAnalyzer
open PointerAnalyzer.AbsDom.Signature

exception NotConstException
exception NotSymbolicException
exception NotSingularException

type Coeff = NUInt
type Const = NUInt

type LinearInt =
  | Bot
  | Top
  | ConcInt of Const
  | SymInt of Coeff * SymbolInt * Const

module private LinearInt =
  let normalize (nUInt: NativeNUInt) x =
    match x with
    | SymInt (a, _, b) when a = nUInt.Zero -> ConcInt b
    | _ -> x

  let mkSym (nUInt: NativeNUInt) coeff sym const_ =
    if coeff = nUInt.Zero then
      ConcInt const_
    else
      SymInt (coeff, sym, const_)

  let mkSymWIdx (nUInt: NativeNUInt) coeff symidx const_ =
    if coeff = nUInt.Zero then
      ConcInt const_
    else
      SymInt (coeff, SymVal symidx, const_)

  let leq nUInt x y =
    match normalize nUInt x, normalize nUInt y with
    | Bot, _
    | _, Top -> true
    | ConcInt a, ConcInt b -> a = b
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) -> a1 = a2 && s1 = s2 && b1 = b2
    | _ -> false

  let rec join nUInt x y =
    match normalize nUInt x, normalize nUInt y with
    | Bot, _
    | _, Top -> y
    | _, Bot
    | Top, _ -> x
    | ConcInt a, ConcInt b -> if a = b then x else Top
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) ->
      if a1 = a2 && s1 = s2 && b1 = b2 then x else Top
    | ConcInt _, SymInt (_, _, _)
    | SymInt (_, _, _), ConcInt _ -> Top

  let toString nUInt x =
    match normalize nUInt x with
    | Bot -> "_"
    | Top -> "T"
    | ConcInt c -> sprintf "0x%X" c
    | SymInt (a, sym, b) ->
      if a = nUInt.Zero then
        sprintf "0x%x" b
      elif a = nUInt.One && b = nUInt.Zero then
        SymbolInt.toString sym
      elif a = nUInt.One then
        sprintf "%s + 0x%x" (SymbolInt.toString sym) b
      elif b = nUInt.Zero then
        sprintf "0x%x * %s" a (SymbolInt.toString sym)
      else
        sprintf "0x%x * %s + 0x%x" a (SymbolInt.toString sym) b

  let isBot =
    function
    | Bot -> true
    | _ -> false

  let isZero nUInt x =
    match normalize nUInt x with
    | ConcInt c when c = nUInt.Zero -> true
    | _ -> false

  let isAndMask (nUInt: NativeNUInt) x =
    match normalize nUInt x with
    | Bot
    | Top -> false
    | ConcInt i -> nUInt.IsAndMask i
    | SymInt (a, _, b) -> a = nUInt.Zero && nUInt.IsAndMask b

  let isConst nUInt x =
    match normalize nUInt x with
    | ConcInt _ -> true
    | _ -> false

  let isSymbolic nUInt x =
    match normalize nUInt x with
    | SymInt _ -> true
    | _ -> false

  let isSingularSymbolic nUInt x =
    match normalize nUInt x with
    | Bot
    | Top
    | ConcInt _ -> false
    | SymInt (a, _, _) -> a = nUInt.One

  let getConst nUInt x =
    match normalize nUInt x with
    | ConcInt c -> c
    | _ -> raise NotConstException

  let getSymbol nUInt x =
    match normalize nUInt x with
    | SymInt (_, s, _) -> s
    | _ -> raise NotSymbolicException

  let getConstPart nUInt x =
    match normalize nUInt x with
    | ConcInt c
    | SymInt (_, _, c) -> c
    | _ -> raise NotConstException

  let getSingularSymbol nUInt x =
    match normalize nUInt x with
    | Bot
    | Top
    | ConcInt _ -> raise NotSingularException
    | SymInt (a, s, _) ->
      if a = nUInt.One then s else raise NotSingularException

  let simpleBinOp nUInt op x y =
    match normalize nUInt x, normalize nUInt y with
    | Bot, _
    | _, Bot -> Bot
    | Top, _
    | _, Top -> Top
    | ConcInt a, ConcInt b -> ConcInt (op a b)
    | SymInt (a, s, b), ConcInt c -> SymInt (a, s, op b c)
    | ConcInt c, SymInt (a, s, b) -> SymInt (a, s, op c b)
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) when s1 = s2 ->
      mkSym nUInt (op a1 a2) s1 (op b1 b2)
    | SymInt _, SymInt _ -> Top

  let add nUInt = simpleBinOp nUInt (+)
  let sub nUInt = simpleBinOp nUInt (-)

  let mul nUInt x y =
    match normalize nUInt x, normalize nUInt y with
    | Bot, _
    | _, Bot -> Bot
    | Top, _
    | _, Top -> Top
    | ConcInt a, ConcInt b -> ConcInt (a * b)
    | SymInt (a, s, b), ConcInt c
    | ConcInt c, SymInt (a, s, b) -> mkSym nUInt (a * c) s (b * c)
    | SymInt _, SymInt _ -> Top

  let shl (nUInt: NativeNUInt) x y =
    // Handle it as x * (2 ^ y) if y is a constant.
    if isConst nUInt y && getConst nUInt y < nUInt.ShlLimit then
      mul nUInt x (ConcInt (nUInt.PowOfTwo (getConst nUInt y)))
    else
      Top

  let shr nUInt x y =
    // Handle it as x / (2 ^ y) if both x and y are constants.
    if isConst nUInt x && isConst nUInt y then
      ConcInt (getConst nUInt x >>> int32 (getConst nUInt y))
    else
      Top

  // ax + b, y -> ay + b
  let substSymbol nUInt newLin x =
    match x with
    | Bot
    | Top
    | ConcInt _ -> x
    | SymInt (a, _, b) -> add nUInt (mul nUInt (ConcInt a) newLin) (ConcInt b)

type AbsInt = { IntPart: LinearInt }

type AbsIntModule (architecture: Architecture) =
  inherit AbstractDomain<AbsInt> ()

  let intTypes = IntTypesLoader.load architecture
  let nUInt = intTypes.NUInt

  let mk intPart = { IntPart = intPart }

  member __.architecture = architecture

  override __.bot = mk Bot

  override __.leq x y = LinearInt.leq nUInt x.IntPart y.IntPart

  override __.join x y =
    mk (LinearInt.join nUInt x.IntPart y.IntPart)

  override __.toString x = LinearInt.toString nUInt x.IntPart

  override __.isBot x = LinearInt.isBot x.IntPart

  member __.top = mk Top

  member __.zero = mk (ConcInt nUInt.Zero)

  member __.isZero x = LinearInt.isZero nUInt x.IntPart

  member __.isConst x = LinearInt.isConst nUInt x.IntPart

  member __.isSymbolic x = LinearInt.isSymbolic nUInt x.IntPart

  member __.isSingularSymbolic x =
    LinearInt.isSingularSymbolic nUInt x.IntPart

  member __.isAndMask x = LinearInt.isAndMask nUInt x.IntPart

  member __.getConst x = LinearInt.getConst nUInt x.IntPart

  member __.getSymbol x = LinearInt.getSymbol nUInt x.IntPart

  member __.getSingularSymbol x =
    LinearInt.getSingularSymbol nUInt x.IntPart

  member __.getConstPart x = LinearInt.getConstPart nUInt x.IntPart

  member __.add x y =
    mk (LinearInt.add nUInt x.IntPart y.IntPart)

  member __.sub x y =
    mk (LinearInt.sub nUInt x.IntPart y.IntPart)

  member __.mul x y =
    mk (LinearInt.mul nUInt x.IntPart y.IntPart)

  member __.shl x y =
    mk (LinearInt.shl nUInt x.IntPart y.IntPart)

  member __.shr x y =
    mk (LinearInt.shr nUInt x.IntPart y.IntPart)

  member __.sar x y =
    mk (LinearInt.shr nUInt x.IntPart y.IntPart)

  member __.ofNUInt ui = mk (ConcInt ui)

  member __.ofSymbol sym =
    mk (SymInt (nUInt.One, sym, nUInt.Zero))

  /// Symbol substitution function.
  member __.substitute x substMap =
    let intPart = x.IntPart

    if not (LinearInt.isSymbolic nUInt intPart) then
      x
    else
      match Map.tryFind (LinearInt.getSymbol nUInt intPart) substMap with
      | None -> mk Top
      | Some substInt ->
        let newIntPart = LinearInt.substSymbol nUInt substInt.IntPart intPart

        mk newIntPart

  /// Collect symbols used in 'x'.
  member __.collectSymbols x =
    if LinearInt.isSymbolic nUInt x.IntPart then
      Set.singleton (LinearInt.getSymbol nUInt x.IntPart)
    else
      Set.empty

module AbsIntDomain =
  let create architecture = AbsIntModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
