module PointerAnalyzer.AbsDom.AbsType

open PointerAnalyzer
open PointerAnalyzer.AbsDom.Signature


type TwoType =
  | Bot
  | Top
  | Value
  | Address

module private TwoType =
  let leq x y =
    match x, y with
    | Bot, _
    | _, Top -> true
    | _, _ -> x = y

  let rec join x y =
    match x, y with
    | Bot, _
    | _, Top -> y
    | _, Bot
    | Top, _ -> x
    | _, _ when x = y -> x
    | _, _ when x <> y -> Top

  let toString x =
    match x with
    | Bot -> "_"
    | Top -> "T"
    | Value -> "Value"
    | Address -> "Address"

  let isBot =
    function
    | Bot -> true
    | _ -> false

type AbsTypeModule () =
  inherit AbstractDomain<TwoType> ()

  override __.bot = Bot

  override __.leq x y = TwoType.leq x y

  override __.join x y = TwoType.join x y

  override __.toString x = TwoType.toString x

  override __.isBot x = TwoType.isBot x

  member __.top = Top

module AbsTypeDomain =
  let create () = AbsTypeModule ()
