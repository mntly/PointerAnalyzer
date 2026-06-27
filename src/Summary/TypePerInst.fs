namespace PointerAnalyzer.Summary

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.TypeInference.ResolvedType

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

  let build resolvedTypes (statements: (ProgramPoint * Stmt) seq) =
    let trackTypPerInst (tracking: TypePerInst) (programPoint, stmt) =
      let filterInTypeIndicator variable =
        match Map.tryFind variable resolvedTypes with
        | Some typeInfo ->
          let variableStr = Variable.ToString variable
          let typeStr = typeInfo.Type.ToOutputString
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
