module PointerAnalyzer.AbsDom.AbsVal

open B2R2
open B2R2.BinIR
open PointerAnalyzer
open PointerAnalyzer.AbsDom.Signature

/// V = n | bottom | top
type AbsVal =
  | Bot
  | Const of BitVector
  | Top

type AbsValModule (_architecture: Architecture) =
  inherit AbstractDomain<AbsVal> ()

  let unary op =
    function
    | Bot -> Bot
    | Const value ->
      try
        Const (op value)
      with _ ->
        Top
    | Top -> Top

  let binary op left right =
    match left, right with
    | Bot, _
    | _, Bot -> Bot
    | Const left, Const right ->
      try
        Const (op (left, right))
      with _ ->
        Top
    | _ -> Top

  override _.bot = Bot

  override _.leq left right =
    match left, right with
    | Bot, _ -> true
    | _, Top -> true
    | Const left, Const right -> left = right
    | _ -> false

  override _.join left right =
    match left, right with
    | Bot, value
    | value, Bot -> value
    | Top, _
    | _, Top -> Top
    | Const left, Const right when left = right -> Const left
    | Const _, Const _ -> Top

  override _.toString value =
    match value with
    | Bot -> "_"
    | Const value -> value.ToString ()
    | Top -> "T"

  override _.isBot value =
    match value with
    | Bot -> true
    | Const _
    | Top -> false

  member _.top = Top

  member _.ofBitVector value = Const value

  member _.ofUInt64 (width: uint64) (value: uint64) =
    let regType = RegType.fromBitWidth (int width)
    Const (BitVector (value, regType))

  member _.tryGetConst =
    function
    | Const value -> Some value
    | Bot
    | Top -> None

  member _.tryGetUInt64 value =
    match value with
    | Const bv ->
      try
        Some (BitVector.ToUInt64 bv)
      with _ ->
        None
    | Bot
    | Top -> None

  member _.neg value = unary BitVector.Neg value

  member _.not value = unary BitVector.Not value

  member _.cast kind regType value =
    match kind with
    | CastKind.SignExt -> unary (fun bv -> BitVector.SExt (bv, regType)) value
    | CastKind.ZeroExt -> unary (fun bv -> BitVector.ZExt (bv, regType)) value
    | _ -> Top

  member _.extract regType pos value =
    unary (fun bv -> BitVector.Extract (bv, regType, pos)) value

  member _.binOp op left right =
    match op with
    | BinOpType.ADD -> binary BitVector.Add left right
    | BinOpType.SUB -> binary BitVector.Sub left right
    | BinOpType.MUL -> binary BitVector.Mul left right
    | BinOpType.DIV -> binary BitVector.Div left right
    | BinOpType.SDIV -> binary BitVector.SDiv left right
    | BinOpType.MOD -> binary BitVector.Modulo left right
    | BinOpType.SMOD -> binary BitVector.SModulo left right
    | BinOpType.SHL -> binary BitVector.Shl left right
    | BinOpType.SHR -> binary BitVector.Shr left right
    | BinOpType.SAR -> binary BitVector.Sar left right
    | BinOpType.AND -> binary BitVector.And left right
    | BinOpType.OR -> binary BitVector.Or left right
    | BinOpType.XOR -> binary BitVector.Xor left right
    | BinOpType.CONCAT -> binary BitVector.Concat left right
    | _ -> Top

  member _.relOp op left right =
    match op with
    | RelOpType.EQ -> binary BitVector.Eq left right
    | RelOpType.NEQ -> binary BitVector.Neq left right
    | RelOpType.GT -> binary (fun (a, b) -> BitVector.Lt (b, a)) left right
    | RelOpType.GE -> binary (fun (a, b) -> BitVector.Le (b, a)) left right
    | RelOpType.LT -> binary BitVector.Lt left right
    | RelOpType.LE -> binary BitVector.Le left right
    | RelOpType.SGT -> binary (fun (a, b) -> BitVector.SLt (b, a)) left right
    | RelOpType.SGE -> binary (fun (a, b) -> BitVector.SLe (b, a)) left right
    | RelOpType.SLT -> binary BitVector.SLt left right
    | RelOpType.SLE -> binary BitVector.SLe left right
    | _ -> Top

module AbsValDomain =
  let create architecture = AbsValModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
