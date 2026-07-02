module PointerAnalyzer.Analysis.ExprEval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeIdMap

/// <summary>
/// Type of constant value. This is checked by B2R2's
/// <see cref="B2R2.FrontEnd.BinFile.IContentAddressable.IsValidAddr" />.
/// </summary>
/// <remarks>
/// <c>AddressConstant</c> indicates the integer is possible address.
/// <c>ValueCount</c> indicates the integer is not address.
/// <c>UnknownConstant</c> indicates the case that it can not distinguish one
/// of above.
/// </remarks>
(*
  ToDo
    Do not use now
*)
type ConstantType =
  | AddressConstant
  | ValueConstant
  | UnknownConstant

/// <summary>
/// Classify the type of given bitvector. This is defined at PointerAnalyzer's
/// <see cref="PointerAnalyzer.Frontend.ConstantClassifier.ConstantClassifier.forBinary" />
/// </summary>
type ExprEvalConfig =
  { ClassifyConstant: BitVector -> ConstantType }

module ExprEvalConfig =
  let empty = { ClassifyConstant = fun _ -> ValueConstant }

type ExprEvalModule (platform: Platform, config: ExprEvalConfig) =

  let absVal = AbsValDomain.create platform
  let stateDom = AnalysisStateDomain.createDefault platform

  new (platform: Platform) = ExprEvalModule (platform, ExprEvalConfig.empty)

  /// Add Value type constraint to given type Id
  member private _.markValue typeId state =
    match typeId with
    | Some typeId -> stateDom.addValue typeId state
    | None -> state

  /// Add Address type constraint to given type Id
  member private _.markAddress typeId state =
    match typeId with
    | Some typeId -> stateDom.addAddress typeId state
    | None -> state

  (* Evaluate Store. But, this should handled during evaluating stmt *)
  member this.EvalStore
    state
    newMemVersion
    prevMemVersion
    addressExpr
    valueExpr
    =
    let address, addressTypeId, state = this.Eval state addressExpr
    let state = this.markAddress addressTypeId state
    let value, valueTypeId, state = this.Eval state valueExpr

    let storedTypeId, state =
      stateDom.store
        prevMemVersion
        newMemVersion
        address
        value
        valueTypeId
        state

    value, storedTypeId, state

  /// Evaluate expression
  member this.Eval state expr : AbsVal * TypeId option * AnalysisState =
    match expr with
    | Num bv ->
      let typeId, state =
        match config.ClassifyConstant bv with
        | AddressConstant -> TypeIds.address, state
        | ValueConstant -> TypeIds.value, state
        | UnknownConstant -> stateDom.freshTypeId state

      (* absVal.ofBitVector bv, Some typeId, state *)
      (*
        Do not add constraint for constant integer:
        it can be used both address and Value
      *)
      absVal.ofBitVector bv, None, state

    | Var variable ->
      let typeId, state = stateDom.getOrFreshTypeId variable state
      let value = stateDom.findRegister variable state
      value, Some typeId, state

    | BinOp (op, _, leftExpr, rightExpr) ->
      let left, leftTypeId, state = this.Eval state leftExpr
      let right, rightTypeId, state = this.Eval state rightExpr
      let value = absVal.binOp op left right

      match op with
      | BinOpType.ADD
      | BinOpType.SUB ->
        let resultTypeId, state = stateDom.freshTypeId state

        let state =
          match op, leftTypeId, rightTypeId with
          | BinOpType.ADD, Some left, Some right ->
            stateDom.addAddResult resultTypeId left right state
          | BinOpType.SUB, Some left, Some right ->
            stateDom.addSubResult resultTypeId left right state
          | _ -> state

        value, Some resultTypeId, state
      | _ -> value, None, state

    | Load (memoryVariable, _, addressExpr) ->
      let address, addressTypeId, state = this.Eval state addressExpr
      let state = this.markAddress addressTypeId state
      stateDom.load memoryVariable.Identifier address state

    | Store (sourceMemory, _, addressExpr, valueExpr) ->
      // this.EvalStore
      //   state
      //   sourceMemory.Identifier
      //   sourceMemory
      //   addressExpr
      //   valueExpr
      // printfn "This must handled by Def(Mem, Store)"
      failwith "[ExprEval.fs] Store: This must handled by Def(Mem, Store)"

    | Cast (kind, regType, innerExpr) ->
      let value, _, state = this.Eval state innerExpr
      let result = absVal.cast kind regType value
      let typeId, state = stateDom.freshTypeId state
      // result, Some typeId, stateDom.addValue typeId state
      result, Some typeId, state

    | Extract (innerExpr, regType, pos) ->
      let value, _, state = this.Eval state innerExpr
      let result = absVal.extract regType pos value
      let typeId, state = stateDom.freshTypeId state
      // result, Some typeId, stateDom.addValue typeId state
      result, Some typeId, state

    | RelOp (op, _, leftExpr, rightExpr) ->
      let left, leftTypeId, state = this.Eval state leftExpr
      let right, rightTypeId, state = this.Eval state rightExpr
      let result = absVal.relOp op left right
      let resultTypeId, state = stateDom.freshTypeId state

      // let state =
      //   state
      //   |> this.markValue leftTypeId
      //   |> this.markValue rightTypeId
      //   |> stateDom.addValue resultTypeId

      // result, Some resultTypeId, state
      result, Some resultTypeId, stateDom.addValue resultTypeId state

    | UnOp (op, _, innerExpr) ->
      let value, innerTypeId, state = this.Eval state innerExpr

      let result =
        match op with
        | UnOpType.NEG -> absVal.neg value
        | UnOpType.NOT -> absVal.not value
        | _ -> absVal.top

      let resultTypeId, state = stateDom.freshTypeId state

      // let state =
      //   state |> this.markValue innerTypeId |> stateDom.addValue resultTypeId

      result, Some resultTypeId, state

    | Ite (conditionExpr, _, trueExpr, falseExpr) ->
      let _, conditionTypeId, state = this.Eval state conditionExpr
      let state = this.markValue conditionTypeId state
      let trueValue, trueTypeId, trueState = this.Eval state trueExpr
      let falseValue, falseTypeId, falseState = this.Eval state falseExpr
      let state = stateDom.join trueState falseState
      let resultTypeId, state = stateDom.freshTypeId state

      let relatedTypeIds =
        [ Some resultTypeId; trueTypeId; falseTypeId ] |> List.choose id

      let state = stateDom.addSame relatedTypeIds state
      absVal.join trueValue falseValue, Some resultTypeId, state

    | ExprList exprs ->
      // let values, typeIds, state = this.evalMany state exprs
      // let value = List.fold absVal.join absVal.bot values
      // let presentTypeIds = List.choose id typeIds

      // match presentTypeIds with
      // | [] -> value, None, state
      // | _ ->
      //   let typeId, state = stateDom.freshTypeId state
      //   value, Some typeId, stateDom.addSame (typeId :: presentTypeIds) state
      (*
        ToDo
          When ExprList comes, and how to handle?
      *)
      failwith "[ExprEval.fs] ExprList: How should I handle this?"

    | FuncName _ -> absVal.bot, None, state

    | Undefined _ -> absVal.bot, None, state

module ExprEvalDomain =
  let createWithConfig platform config = ExprEvalModule (platform, config)

  let create platform = ExprEvalModule platform

  let createFromString platform =
    PointerAnalyzer.Platform.Platform.ofString platform |> create
