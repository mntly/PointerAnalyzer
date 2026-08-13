module PointerAnalyzer.Frontend.FunctionDFA

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open B2R2.MiddleEnd.DataFlow.SSASparseDataFlow

/// Represents the program point and corresponding statement that target SSA
/// variable is used as pointer
type PointerUseEvidence =
  { ProgramPoint: ProgramPoint
    Statement: Stmt }

/// <summary>
/// Pre-analysis result of single function.
/// </summary>
/// <remarks>
/// <c>Statements</c> is the list of <see cref="B2R2.BinIR.SSA.Stmt" /> values
/// ordered by <see cref="B2R2.ProgramPoint" />.
/// <c>PointerUse</c> tells given <see cref="B2R2.BinIR.SSA.Variable" />
/// will be used as pointer or not. It also tells the statement that given
/// <see cref="B2R2.BinIR.SSA.Variable" /> is used as pointer.
/// <c>ConstValue</c> returns <c>Some</c> <see cref="B2R2.BitVector" />
/// when B2R2 data-flow analysis proves a constant value for the given SSA
/// variable.
/// </remarks>
type FunctionDFA =
  { Statements: (ProgramPoint * Stmt) list
    PointerUse: Variable -> PointerUseEvidence option
    ConstValue: Variable -> BitVector option }

module FunctionDFA =
  /// Return the constant value from DFA result
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

  (* Check given variable is in given expression *)
  let private exprContainsVar variable expression =
    let rec variablesInExpr expr =
      match expr with
      | Var vari -> vari = variable
      | UnOp (_, _, expr)
      | Cast (_, _, expr)
      | Extract (expr, _, _) -> variablesInExpr expr
      | BinOp (_, _, left, right)
      | RelOp (_, _, left, right) ->
        variablesInExpr left || variablesInExpr right
      | Ite (condition, _, trueExpr, falseExpr) ->
        variablesInExpr condition
        || variablesInExpr trueExpr
        || variablesInExpr falseExpr
      | Load (_, _, _)
      | Store (_, _, _, _)
      | ExprList _
      | Num _
      | FuncName _
      | Undefined _ -> false

    variablesInExpr expression

  (*
    Check syntactically given variable is used as pointer in given expression
  *)
  let private pointerUseInExpr variable expression =
    let rec pointerUseInExprInner expr =
      match expr with
      | Load (_, _, address) ->
        exprContainsVar variable address || pointerUseInExprInner address
      | Store (_, _, address, value) ->
        exprContainsVar variable address
        || pointerUseInExprInner address
        || pointerUseInExprInner value
      | UnOp (_, _, expr)
      | Cast (_, _, expr)
      | Extract (expr, _, _) -> pointerUseInExprInner expr
      | BinOp (_, _, left, right)
      | RelOp (_, _, left, right) ->
        pointerUseInExprInner left || pointerUseInExprInner right
      | Ite (condition, _, trueExpr, falseExpr) ->
        pointerUseInExprInner condition
        || pointerUseInExprInner trueExpr
        || pointerUseInExprInner falseExpr
      | ExprList _
      | Var _
      | Num _
      | FuncName _
      | Undefined _ -> false

    pointerUseInExprInner expression

  (* Check syntactically given variable is used as pointer in given statement *)
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

  (* Mapping each statment with the program point as key *)
  let private statementMap (ssaCFG: SSACFG) =
    ssaCFG.Vertices
    |> Seq.collect (fun vertex ->
      vertex.VData.Internals.Statements
      |> Seq.mapi (fun index (programPoint, stmt) ->
        (vertex.ID, index), (programPoint, stmt)))
    |> Map.ofSeq

  (* Check use chain of DFA to answer given variable is used as pointer *)
  (* If given variable is used as pointer, this returns PointerUseEvidence *)
  let private pointerUseFrom (ssaCFG: SSACFG) =
    let edges = SSAEdges ssaCFG
    let statements = statementMap ssaCFG

    (* Check given variable is used as pointer at given location *)
    let isPointerUse variable location =
      match Map.tryFind location statements with
      | Some (_, stmt) -> pointerUseInStmt variable stmt
      | None -> false

    (* Construct PointerUseEvidence Map with variable as Key *)
    let pointerEvidence =
      edges.Uses
      |> Seq.choose (fun (KeyValue (variable, uses)) ->
        uses
        |> Seq.filter (isPointerUse variable)
        |> Seq.sort
        |> Seq.tryPick (fun location ->
          Map.tryFind location statements
          |> Option.map (fun (programPoint, statement) ->
            variable,
            { ProgramPoint = programPoint
              Statement = statement })))
      |> Map.ofSeq

    fun variable -> Map.tryFind variable pointerEvidence

  /// Collect pointer usage and proved constant value of given varaible
  let create handle (ssaCFG: SSACFG) =
    let statements =
      ssaCFG.Vertices
      |> Array.sortBy (fun vertex -> vertex.VData.Internals.PPoint.Address)
      |> Array.collect (fun vertex -> vertex.VData.Internals.Statements)
      |> Array.toList

    { Statements = statements
      PointerUse = pointerUseFrom ssaCFG
      ConstValue = constantValueFrom handle ssaCFG }
