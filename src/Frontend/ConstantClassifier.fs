module PointerAnalyzer.Frontend.ConstantClassifier

open B2R2
open B2R2.FrontEnd
open PointerAnalyzer.Analysis.ExprEval

module ConstantClassifier =
  let forBinary (handle: BinHandle) (value: BitVector) =
    try
      let address = BitVector.ToUInt64 value

      if handle.File.IsValidAddr address then
        AddressConstant
      else
        UnknownConstant
    with _ ->
      UnknownConstant
