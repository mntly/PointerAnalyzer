namespace PointerAnalyzer.Summary

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

type ResolvedType =
  | Address
  | Value
  | Conflict
  | Unknown

  member this.ToOutputString =
    match this with
    | Address -> "Address"
    | Value -> "Value"
    | Conflict -> "Conflict"
    | Unknown -> "Unknown"

type VarTypeMap = Map<Variable, ResolvedType>

module VarTypeMap =
  let empty: VarTypeMap = Map.empty

  let tryFind variable variableTypes = Map.tryFind variable variableTypes

  let add variable resolvedType variableTypes =
    Map.add variable resolvedType variableTypes

type PP = string
type SSARegName = string
type InferredType = string
type TypePerInst = Map<PP, Map<SSARegName, InferredType>>

module TypePerInst =
  let empty: TypePerInst = Map.empty

  let private programPointToString (programPoint: ProgramPoint) =
    // sprintf "0x%08x+%d" programPoint.Address programPoint.Position
    sprintf "0x%08x" programPoint.Address

  let findVarMap programPoint (types: TypePerInst) =
    Map.tryFind programPoint types |> Option.defaultValue Map.empty

  let tryFind programPoint variable types =
    types |> Map.tryFind programPoint |> Option.bind (Map.tryFind variable)

  let add programPoint variable inferredType types =
    let variableTypes = findVarMap programPoint types
    Map.add programPoint (Map.add variable inferredType variableTypes) types

  let addMany programPoint entries types =
    entries
    |> Seq.fold
      (fun result (variable, inferredType) ->
        add programPoint variable inferredType result)
      types

  let private resolveType constraints conflicts typeId =
    if Set.contains typeId conflicts then
      Conflict
    else
      let isAddress = Set.contains (TypeConstraint.Address typeId) constraints

      let isValue = Set.contains (TypeConstraint.Value typeId) constraints

      match isAddress, isValue with
      | true, false -> Address
      | false, true -> Value
      | true, true -> Conflict
      | false, false -> Unknown

  let private varsInExpr expr =
    let rec collect acc expr =
      match expr with
      | Num _
      | FuncName _
      | Undefined _ -> acc
      | Var variable -> Set.add variable acc
      | ExprList expressions -> List.fold collect acc expressions
      | Load (_, _, address) -> collect acc address
      | Store (_, _, address, value) ->
        let accNew = collect acc address
        collect accNew value
      | UnOp (_, _, expr)
      | Cast (_, _, expr)
      | Extract (expr, _, _) -> collect acc expr
      | BinOp (_, _, left, right)
      | RelOp (_, _, left, right) ->
        let accNew = collect acc left
        collect accNew right
      | Ite (condition, _, trueExpr, falseExpr) ->
        let accCond = collect acc condition
        let accTrue = collect accCond trueExpr
        collect accTrue falseExpr

    collect Set.empty expr

  let private varsInJmp jmpType =
    match jmpType with
    | IntraJmp _ -> Set.empty
    | IntraCJmp (condition, _, _) -> varsInExpr condition
    | InterJmp target -> varsInExpr target
    | InterCJmp (condition, trueTarget, falseTarget) ->
      Set.unionMany
        [ varsInExpr condition; varsInExpr trueTarget; varsInExpr falseTarget ]

  let private varsInStmt stmt =
    match stmt with
    | LMark _
    | SideEffect _ -> Set.empty
    | Def (variable, expression) -> Set.add variable (varsInExpr expression)
    | Phi (variable, _) -> Set.singleton variable
    | Jmp jmpType -> varsInJmp jmpType
    | ExternalCall (callee, inputVariables, outputVariables) ->
      Set.unionMany
        [ varsInExpr callee
          Set.ofList inputVariables
          Set.ofList outputVariables ]

  let build
    constraints
    conflicts
    typeIndicators
    (statements: (ProgramPoint * Stmt) seq)
    =
    let trackTypPerInst (tracking: TypePerInst) (programPoint, stmt) =
      let filterInTypeIndicator variable =
        match Map.tryFind variable typeIndicators with
        | Some typeId ->
          let variableStr = Variable.ToString variable
          let resolvedType = resolveType constraints conflicts typeId
          let typeStr = resolvedType.ToOutputString
          Some (variableStr, typeStr)
        | None -> None

      let variableInStmt = varsInStmt stmt

      let entries = Seq.choose filterInTypeIndicator variableInStmt

      let ppStr = programPointToString programPoint

      addMany ppStr entries tracking

    Seq.fold trackTypPerInst empty statements

  let toString (types: TypePerInst) =
    types
    |> Map.toList
    |> List.collect (fun (programPoint, variableTypes) ->
      variableTypes
      |> Map.toList
      |> List.map (fun (variable, inferredType) ->
        sprintf "%O: %s -> %s" programPoint variable inferredType))
    |> String.concat "\n"
