module PointerAnalyzer.Main

open B2R2
open B2R2.Assembly
open B2R2.BinIR
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd
open B2R2.MiddleEnd.DataFlow
open B2R2.MiddleEnd.DataFlow.SSASparseDataFlow
open B2R2.MiddleEnd.SSA
open PointerAnalyzer
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.Analysis.Analyzer
open PointerAnalyzer.Analysis.ExprEval
open PointerAnalyzer.Analysis.StmtEval

// let private sampleAssembly =
//   """
//   push 10
// push 20

// pop eax
// pop ebx

// add eax, ebx

// push eax
// push ebx
// ret
// """

let private sampleAssembly =
  """
  mov edx, 0x2000
mov ecx, 0x4
mov eax, dword ptr [edx]
sub edx, 4
add ecx, edx
ret
"""

let private startAddress = 0x1000UL

let private assemble assembly =
  let isa = ISA (B2R2.Architecture.Intel, WordSize.Bit32)

  match Assembler(isa, startAddress).Lower assembly with
  | Ok instructions -> isa, instructions |> List.toArray |> Array.concat
  | Error message -> invalidArg (nameof assembly) message

let private recoverSSA isa bytes =
  let handle = BinHandle (bytes, isa, Some startAddress, false)
  let brew = BinaryBrew handle

  let cfg =
    brew.Functions.Sequence
    |> Seq.tryFind (fun function_ -> function_.EntryPoint = startAddress)
    |> function
      | Some function_ -> function_.CFG
      | None ->
        let recoveredAddresses =
          brew.Functions.Addresses
          |> Seq.map (sprintf "0x%x")
          |> String.concat ", "

        let recoveredAddresses =
          if System.String.IsNullOrEmpty recoveredAddresses then
            "<none>"
          else
            recoveredAddresses

        failwithf
          "B2R2 did not recover a function at 0x%x. Recovered functions: %s. \
            Ensure the assembly contains a terminating instruction such as ret."
          startAddress
          recoveredAddresses

  let ssaCFG = SSALifterFactory.Create(handle).Lift cfg

  let statementInfos =
    ssaCFG.Vertices
    |> Array.sortBy (fun vertex -> vertex.VData.Internals.PPoint.Address)
    |> Array.collect (fun vertex -> vertex.VData.Internals.Statements)
    |> Array.toList

  handle, ssaCFG, statementInfos

let private constantValueFrom handle ssaCFG =
  let analysis =
    SSAConstantPropagation handle
    :> IDataFlowComputable<
      SSAVarPoint,
      ConstantDomain.Lattice,
      State<ConstantDomain.Lattice>,
      B2R2.MiddleEnd.ControlFlowGraph.SSABasicBlock
     >

  let state = analysis.Compute ssaCFG
  let provider = state :> IAbsValProvider<SSAVarPoint, ConstantDomain.Lattice>

  fun variable ->
    match provider.GetAbsValue (RegularSSAVar variable) with
    | ConstantDomain.Const value -> Some value
    | ConstantDomain.NotAConst
    | ConstantDomain.Undef -> None

let rec private variablesInExpr expr =
  seq {
    match expr with
    | Var variable -> yield variable
    | Load (_, _, address) -> yield! variablesInExpr address
    | Store (_, _, address, value) ->
      yield! variablesInExpr address
      yield! variablesInExpr value
    | ExprList expressions ->
      for expression in expressions do
        yield! variablesInExpr expression
    | UnOp (_, _, expression)
    | Cast (_, _, expression)
    | Extract (expression, _, _) -> yield! variablesInExpr expression
    | BinOp (_, _, left, right)
    | RelOp (_, _, left, right) ->
      yield! variablesInExpr left
      yield! variablesInExpr right
    | Ite (condition, _, trueExpr, falseExpr) ->
      yield! variablesInExpr condition
      yield! variablesInExpr trueExpr
      yield! variablesInExpr falseExpr
    | Num _
    | FuncName _
    | Undefined _ -> ()
  }

let rec private pointerVariablesInExpr expr =
  seq {
    match expr with
    | Load (_, _, address) ->
      yield! variablesInExpr address
      yield! pointerVariablesInExpr address
    | Store (_, _, address, value) ->
      yield! variablesInExpr address
      yield! pointerVariablesInExpr address
      yield! pointerVariablesInExpr value
    | ExprList expressions ->
      for expression in expressions do
        yield! pointerVariablesInExpr expression
    | UnOp (_, _, expression)
    | Cast (_, _, expression)
    | Extract (expression, _, _) -> yield! pointerVariablesInExpr expression
    | BinOp (_, _, left, right)
    | RelOp (_, _, left, right) ->
      yield! pointerVariablesInExpr left
      yield! pointerVariablesInExpr right
    | Ite (condition, _, trueExpr, falseExpr) ->
      yield! pointerVariablesInExpr condition
      yield! pointerVariablesInExpr trueExpr
      yield! pointerVariablesInExpr falseExpr
    | Var _
    | Num _
    | FuncName _
    | Undefined _ -> ()
  }

let private pointerUseFrom statements =
  let pointerVariables =
    statements
    |> Seq.collect (function
      | Def (_, expression) -> pointerVariablesInExpr expression
      | Jmp (InterJmp target) -> variablesInExpr target
      | Jmp (InterCJmp (_, trueTarget, falseTarget)) ->
        Seq.append (variablesInExpr trueTarget) (variablesInExpr falseTarget)
      | ExternalCall (callee, _, _) -> variablesInExpr callee
      | _ -> Seq.empty)
    |> Seq.map Variable.ToString
    |> Set.ofSeq

  fun variable -> Set.contains (Variable.ToString variable) pointerVariables

let private classifyConstant byteCount (value: BitVector) =
  // let bitWidth = value |> BitVector.GetType |> RegType.toBitWidth

  try
    let address = BitVector.ToUInt64 value
    let endAddress = startAddress + uint64 byteCount

    // printfn "Bit width: %d" bitWidth

    if startAddress <= address && address < endAddress then
      AddressConstant
    else if address < startAddress then
      ValueConstant
    else
      UnknownConstant
  with _ ->
    UnknownConstant

let private formatPoint address position =
  sprintf "0x%04x+%-8d" address position

let private constraintToString =
  function
  | Address typeId -> sprintf "Address(t%d)" typeId
  | Value typeId -> sprintf "Value(t%d)" typeId
  | Same typeIds ->
    typeIds
    |> Set.toList
    |> List.map (sprintf "t%d")
    |> String.concat ", "
    |> sprintf "Same({%s})"
  | AddResult (result, left, right) ->
    sprintf "AddResult(t%d, t%d, t%d)" result left right
  | SubResult (result, left, right) ->
    sprintf "SubResult(t%d, t%d, t%d)" result left right

let private registerTypeToString constraints conflicts typeId =
  if Set.contains typeId conflicts then
    "Conflict"
  else
    let isAddress = Set.contains (Address typeId) constraints
    let isValue = Set.contains (Value typeId) constraints

    match isAddress, isValue with
    | true, false -> "Address"
    | false, true -> "Value"
    | false, false -> "Unknown"
    | true, true -> "Conflict"

[<EntryPoint>]
let main argv =
  let architecture =
    if Array.isEmpty argv then
      Architecture.ofString "x86_32"
    else
      Architecture.ofString argv[0]

  printfn "Assembly:%s" sampleAssembly

  let isa, bytes = assemble sampleAssembly
  let handle, ssaCFG, statementInfos = recoverSSA isa bytes
  let statements = List.map snd statementInfos

  let config =
    { StmtEvalConfig.empty with
        PointerUse = pointerUseFrom statements
        ConstValue = constantValueFrom handle ssaCFG
        ClassifyConstant = classifyConstant bytes.Length }

  let result = AnalyzerDomain.analyze architecture config ssaCFG

  printfn "B2R2 SSA:"

  statementInfos
  |> List.iter (fun (programPoint, statement) ->
    printfn
      "  %s %s"
      (formatPoint programPoint.Address programPoint.Position)
      ((PrettyPrinter.ToString [| statement |]).Trim ()))

  printfn "B2R2 SSA Raw:"

  statementInfos
  |> List.iter (fun (programPoint, statement) ->
    printfn
      "  %s%A"
      (formatPoint programPoint.Address programPoint.Position)
      statement)

  printfn "SSARegisterTypes:"

  result.FinalState.Types.TypeIndicators
  |> Map.iter (fun variable typeId ->
    let inferredType =
      registerTypeToString result.TypeConstraints result.TypeConflicts typeId

    printfn "  %s -> %s (t%d)" (Variable.ToString variable) inferredType typeId)

  printfn "TypeConstraints:"
  result.TypeConstraints |> Set.iter (constraintToString >> printfn "  %s")

  printfn "TypeConflicts:"

  if Set.isEmpty result.TypeConflicts then
    printfn "  <empty>"
  else
    result.TypeConflicts |> Set.iter (printfn "  t%d")

  0
