module PointerAnalyzer.Frontend.ConstantClassifier

open B2R2
open B2R2.FrontEnd
open PointerAnalyzer.Analysis.ExprEval

module ConstantClassifier =
  (* If constant value is trivial address or value, use its type *)
  (*
    ToDo
      Do not use now
  *)
  let forBinary (handle: BinHandle) (value: BitVector) =
    // try
    //   let address = BitVector.ToUInt64 value

    //   if handle.File.IsValidAddr address then
    //     AddressConstant
    //   else
    //     ValueConstant
    // with _ ->
    //   UnknownConstant
    UnknownConstant
