module PointerAnalyzer.AbsDom.RegMap

open B2R2.BinIR.SSA
open PointerAnalyzer
open PointerAnalyzer.AbsDom.Signature
open PointerAnalyzer.AbsDom.Functor
open PointerAnalyzer.AbsDom.AbsVal

type SsaRegister = Variable

type private SsaRegisterElem () =
  inherit Elem<SsaRegister> ()

  override __.toString reg = Variable.ToString reg

type RegMap = Map<SsaRegister, AbsVal>

type RegMapModule (architecture: Architecture) =
  inherit MapDomain<SsaRegister, AbsVal>
    (SsaRegisterElem (), AbsValDomain.create architecture)

  let absVal = AbsValDomain.create architecture

  member __.make entries : RegMap = Map.ofList entries

  member __.joinValue x y = absVal.join x y

module RegMapDomain =
  let create architecture = RegMapModule architecture

  let createFromString architecture =
    Architecture.ofString architecture |> create
