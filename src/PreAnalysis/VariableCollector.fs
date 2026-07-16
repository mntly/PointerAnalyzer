module PointerAnalyzer.PreAnalysis.VariableCollector

open B2R2.BinIR
open B2R2.BinIR.SSA

/// Check given variable indicates Memory or not
let isMemoryVariable (variable: Variable) =
  match variable.Kind with
  | MemVar -> true
  | _ -> false

/// Check given variable indicates non-Memory or not
let isNonMemoryVariable variable = not (isMemoryVariable variable)

/// Check given variable indicates StackVar or not.
let isStackVariable (variable: Variable) =
  match variable.Kind with
  | StackVar _ -> true
  | _ -> false

/// If given variable is RegVar, return its register ID.
let tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

/// If given variable is non-Memory, then return as signleton set.
/// If not, return empty set.
let singletonNonMemory variable =
  if isNonMemoryVariable variable then
    Set.singleton variable
  else
    Set.empty

/// Extract all SSA Variables in given expression.
let rec variablesInExpr expr =
  match expr with
  | Var variable -> singletonNonMemory variable
  | UnOp (_, _, expr)
  | Cast (_, _, expr)
  | Extract (expr, _, _) -> variablesInExpr expr
  | BinOp (_, _, left, right)
  | RelOp (_, _, left, right) ->
    Set.union (variablesInExpr left) (variablesInExpr right)
  | Ite (condition, _, trueExpr, falseExpr) ->
    Set.unionMany
      [ variablesInExpr condition
        variablesInExpr trueExpr
        variablesInExpr falseExpr ]
  | Load (_, _, address) -> variablesInExpr address
  | Store (_, _, address, value) ->
    Set.union (variablesInExpr address) (variablesInExpr value)
  | ExprList exprs ->
    if List.isEmpty exprs then
      Set.empty
    else
      exprs |> List.map variablesInExpr |> Set.unionMany
  | Num _
  | FuncName _
  | Undefined _ -> Set.empty

/// Return non-memory SSA Variable if it is defined in the given stmt.
let definedVariable =
  function
  | Def (variable, _)
  | Phi (variable, _) when isNonMemoryVariable variable -> Some variable
  | _ -> None

/// Extract all non-memory SSA Variables in given stmt.
/// This extracts only the used variables.
let usedVariablesInStmt =
  function
  | Def (_, expr) -> variablesInExpr expr
  | Phi ({ Kind = MemVar }, _) -> Set.empty
  | Phi (variable, sourceIds) ->
    sourceIds
    |> Array.map (fun sourceId -> { variable with Identifier = sourceId })
    |> Array.filter isNonMemoryVariable
    |> Set.ofArray
  | Jmp (IntraJmp _) -> Set.empty
  | Jmp (IntraCJmp (condition, _, _)) -> variablesInExpr condition
  | Jmp (InterJmp target) -> variablesInExpr target
  | Jmp (InterCJmp (condition, trueTarget, falseTarget)) ->
    Set.unionMany
      [ variablesInExpr condition
        variablesInExpr trueTarget
        variablesInExpr falseTarget ]
  | ExternalCall (callee, inputs, outputs) ->
    Set.unionMany
      [ variablesInExpr callee
        inputs |> List.filter isNonMemoryVariable |> Set.ofList
        outputs |> List.filter isNonMemoryVariable |> Set.ofList ]
  | LMark _
  | SideEffect _ -> Set.empty

/// Extract all non-memory SSA Variables in given stmt, including both defined
/// and used variables.
let variablesInStmt stmt =
  let defined =
    match definedVariable stmt with
    | Some variable -> Set.singleton variable
    | None -> Set.empty

  Set.union defined (usedVariablesInStmt stmt)
