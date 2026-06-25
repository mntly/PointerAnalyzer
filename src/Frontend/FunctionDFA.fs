module PointerAnalyzer.Frontend.FunctionDFA

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open B2R2.MiddleEnd.DataFlow.SSASparseDataFlow

type FunctionDFA =
  { Statements: (ProgramPoint * Stmt) list
    PointerUse: Variable -> bool
    ConstValue: Variable -> BitVector option }

module FunctionDFA =
  let private constantValueFrom handle (ssaCFG: SSACFG) =
    let dfa =
      SSAConstantPropagation handle
      :> IDataFlowComputable<
        SSAVarPoint,
        ConstantDomain.Lattice,
        State<ConstantDomain.Lattice>,
        SSABasicBlock
       >

    let provider =
      dfa.Compute ssaCFG :> IAbsValProvider<SSAVarPoint, ConstantDomain.Lattice>

    fun variable ->
      match provider.GetAbsValue (RegularSSAVar variable) with
      | ConstantDomain.Const value -> Some value
      | ConstantDomain.NotAConst
      | ConstantDomain.Undef -> None

  let private exprContainsVar variable expression =
    let rec variablesInExpr expr ret =
      if ret then
        ret
      else
        match expr with
        | Var vari -> vari = variable
        | UnOp (_, _, expr)
        | Cast (_, _, expr)
        | Extract (expr, _, _) -> variablesInExpr expr ret
        | BinOp (_, _, left, right)
        | RelOp (_, _, left, right) ->
          variablesInExpr left ret |> variablesInExpr right
        | Ite (condition, _, trueExpr, falseExpr) ->
          variablesInExpr condition ret
          |> variablesInExpr trueExpr
          |> variablesInExpr falseExpr
        | Load (_, _, _)
        | Store (_, _, _, _)
        | ExprList _
        | Num _
        | FuncName _
        | Undefined _ -> ret

    variablesInExpr expression false

  let private pointerUseInExpr variable expression =
    let rec pointerUseInExprInner expr ret =
      if ret then
        ret
      else
        match expr with
        | Load (_, _, address) ->
          exprContainsVar variable address |> pointerUseInExprInner address
        | Store (_, _, address, value) ->
          exprContainsVar variable address
          |> pointerUseInExprInner address
          |> pointerUseInExprInner value
        | UnOp (_, _, expr)
        | Cast (_, _, expr)
        | Extract (expr, _, _) -> pointerUseInExprInner expr ret
        | BinOp (_, _, left, right)
        | RelOp (_, _, left, right) ->
          pointerUseInExprInner left ret |> pointerUseInExprInner right
        | Ite (condition, _, trueExpr, falseExpr) ->
          pointerUseInExprInner condition ret
          |> pointerUseInExprInner trueExpr
          |> pointerUseInExprInner falseExpr
        | ExprList _
        | Var _
        | Num _
        | FuncName _
        | Undefined _ -> ret

    pointerUseInExprInner expression false

  let private pointerUseInStmt variable stmt =
    match stmt with
    | Def (_, expression) -> pointerUseInExpr variable expression
    | Jmp (InterJmp target) -> exprContainsVar variable target
    | Jmp (InterCJmp (condition, trueTarget, falseTarget)) ->
      pointerUseInExpr variable condition
      || exprContainsVar variable trueTarget
      || exprContainsVar variable falseTarget
    | Jmp (IntraCJmp (condition, _, _)) -> pointerUseInExpr variable condition
    | ExternalCall (callee, _, _) -> exprContainsVar variable callee
    | _ -> false

  let private statementMap (ssaCFG: SSACFG) =
    ssaCFG.Vertices
    |> Seq.collect (fun vertex ->
      vertex.VData.Internals.Statements
      |> Seq.mapi (fun index (_, stmt) -> (vertex.ID, index), stmt))
    |> Map.ofSeq

  let private pointerUseFrom (ssaCFG: SSACFG) =
    let edges = SSAEdges ssaCFG
    let statements = statementMap ssaCFG

    let isPointerUse variable location =
      match Map.tryFind location statements with
      | Some stmt -> pointerUseInStmt variable stmt
      | None -> false

    let pointerVariables =
      edges.Uses
      |> Seq.choose (fun (KeyValue (variable, uses)) ->
        if Seq.exists (isPointerUse variable) uses then
          Some variable
        else
          None)
      |> Set.ofSeq

    fun variable -> Set.contains variable pointerVariables

  let create handle (ssaCFG: SSACFG) =
    let statements =
      ssaCFG.Vertices
      |> Array.sortBy (fun vertex -> vertex.VData.Internals.PPoint.Address)
      |> Array.collect (fun vertex -> vertex.VData.Internals.Statements)
      |> Array.toList

    { Statements = statements
      PointerUse = pointerUseFrom ssaCFG
      ConstValue = constantValueFrom handle ssaCFG }
