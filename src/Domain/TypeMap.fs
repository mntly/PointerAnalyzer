module PointerAnalyzer.AbsDom.TypeMap

open PointerAnalyzer.AbsDom.Signature
open PointerAnalyzer.AbsDom.Functor
open PointerAnalyzer.AbsDom.AbsType

/// Type variable used as a key in the abstract type map.
type TypePtr = TypePtr of int

module TypePtr =
  let bot = TypePtr 0
  let baseAddr = TypePtr 1
  let baseVal = TypePtr 2
  let top = TypePtr 3
  let firstFreshId = 4

  let toString =
    function
    | TypePtr id -> sprintf "TypePtr(%d)" id

type private TypePtrElem () =
  inherit Elem<TypePtr> ()

  override __.toString tptr = TypePtr.toString tptr

/// Map from type variables to the two-point abstract type domain.
type TypeMap = Map<TypePtr, TwoType>

type TypeMapModule () =
  inherit MapDomain<TypePtr, TwoType> (TypePtrElem (), AbsTypeDomain.create ())

  member __.initial =
    Map.ofList
      [ TypePtr.bot, Bot
        TypePtr.baseAddr, Address
        TypePtr.baseVal, Value
        TypePtr.top, Top ]

  override this.bot = this.initial

  member __.make entries : TypeMap = Map.ofList entries

module TypeMapDomain =
  let create () = TypeMapModule ()
