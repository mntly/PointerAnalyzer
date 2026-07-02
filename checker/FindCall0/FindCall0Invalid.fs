module FindCall0Invalid

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd
open B2R2.MiddleEnd.ControlFlowAnalysis
open B2R2.MiddleEnd.ControlFlowAnalysis.Strategies
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open B2R2.MiddleEnd.DataFlow.SSASparseDataFlow
open B2R2.MiddleEnd.SSA

type BuilderFunction =
  { Address: Addr
    Name: string option
    State: CFGBuilderState }

type JumpZeroSite =
  { FunctionAddress: Addr
    FunctionState: CFGBuilderState
    ProgramPoint: ProgramPoint
    Statement: Stmt }

type FindCall0InvalidResult =
  { BinaryPath: string
    ValidFunctions: BuilderFunction list
    InvalidFunctions: BuilderFunction list
    LiftFailures: (BuilderFunction * string) list
    Sites: JumpZeroSite list }

let private tryUInt64 bv =
  try
    Some (BitVector.ToUInt64 bv)
  with _ ->
    None

/// Read the constant value of given variable
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

let private isZeroExpr constValue =
  function
  | Num bv -> tryUInt64 bv = Some 0UL
  | Var variable ->
    match constValue variable with
    | Some bv -> tryUInt64 bv = Some 0UL
    | None -> false
  | _ -> false

/// Check jump taret is 0
let private isJumpTargetZero constValue =
  function
  | Jmp (InterJmp target) -> isZeroExpr constValue target
  | Jmp (InterCJmp (_, trueTarget, falseTarget)) ->
    isZeroExpr constValue trueTarget || isZeroExpr constValue falseTarget
  | _ -> false

/// Find all statements that try to jump to 0 in given function
let private collectJumpZeroSites
  (hdl: BinHandle)
  (lifter: ISSALiftable)
  (builder: ICFGBuildable<DummyContext, DummyContext>)
  =
  let ssaCfg = lifter.Lift builder.Context.CFG
  let constValue = constantValueFrom hdl ssaCfg

  let filterTarget0Jump (pp, stmt) =
    if isJumpTargetZero constValue stmt then
      Some
        { FunctionAddress = builder.EntryPoint
          FunctionState = builder.BuilderState
          ProgramPoint = pp
          Statement = stmt }
    else
      None

  let ppStatements =
    ssaCfg.Vertices
    |> Seq.collect (fun vertex -> vertex.VData.Internals.Statements)

  ppStatements |> Seq.choose filterTarget0Jump |> Seq.toList

let private builderFunction (builder: ICFGBuildable<_, _>) =
  { Address = builder.EntryPoint
    Name = Some builder.Context.FunctionName
    State = builder.BuilderState }

/// Check given function is valid from B2R2
let private isValidBuilder
  (builder: ICFGBuildable<DummyContext, DummyContext>)
  =
  match builder.BuilderState with
  | Finished
  | ForceFinished -> true
  | _ -> false

/// Find all statements that try to jump to 0 in given binary
let run (binaryPath: string) =
  let hdl = BinHandle binaryPath
  let brew = BinaryBrew hdl
  let lifter = SSALifterFactory.Create hdl
  let builders = brew.Builders.Values |> Array.sortBy (fun b -> b.EntryPoint)

  (* Valid from B2R2 Function Recovery *)
  let validFunctions =
    builders
    |> Array.filter isValidBuilder
    |> Array.map builderFunction
    |> Array.toList

  (* Invalid from B2R2 Function Recovery *)
  let invalidBuilders =
    builders |> Array.filter (fun builder -> builder.BuilderState = Invalid)

  let folder (failures, sites) builder =
    let fn = builderFunction builder

    try
      let newSites = collectJumpZeroSites hdl lifter builder

      failures, List.rev newSites @ sites
    with ex ->
      (fn, ex.Message) :: failures, sites

  let failures, sites = builders |> Array.fold folder ([], [])

  { BinaryPath = binaryPath
    ValidFunctions = validFunctions
    InvalidFunctions =
      invalidBuilders |> Array.map builderFunction |> Array.toList
    LiftFailures = List.rev failures
    Sites = List.rev sites }

let toText result =
  let ppToString (pp: ProgramPoint) =
    sprintf "0x%08x+%d" pp.Address pp.Position

  let functionNameToString =
    function
    | Some name -> name
    | None -> "<unknown>"

  let builderFunctionToString fn =
    sprintf "  0x%08x  %s" fn.Address (functionNameToString fn.Name)

  (* Valid function list to String *)
  let validFunctions =
    if List.isEmpty result.ValidFunctions then
      [ "  <none>" ]
    else
      result.ValidFunctions
      |> List.sortBy (fun fn -> fn.Address)
      |> List.map builderFunctionToString

  (* Invalid function list to String *)
  let invalidFunctions =
    if List.isEmpty result.InvalidFunctions then
      [ "  <none>" ]
    else
      result.InvalidFunctions
      |> List.sortBy (fun fn -> fn.Address)
      |> List.map builderFunctionToString

  (* Make string of jmp 0 statements *)
  let groupedSites =
    result.Sites
    |> List.groupBy (fun site -> site.FunctionAddress)
    |> List.sortBy fst

  (* Check among all functions (regardless valid/invalid) *)
  let functionName address =
    result.ValidFunctions @ result.InvalidFunctions
    |> List.tryFind (fun fn -> fn.Address = address)
    |> Option.bind (fun fn -> fn.Name)
    |> functionNameToString

  (* Jump Target 0 to String*)
  let siteGroupToString (address, sites) =
    let state = sites |> List.head |> (fun site -> site.FunctionState)

    let header =
      sprintf "Function 0x%08x (%s, %A)" address (functionName address) state

    let body =
      sites
      |> List.sortBy (fun site -> site.ProgramPoint)
      |> List.map (fun site ->
        sprintf "  %-16s %A" (ppToString site.ProgramPoint) site.Statement)

    header :: body

  let jumpZeroSites =
    if List.isEmpty groupedSites then
      [ "  <none>" ]
    else
      groupedSites |> List.collect siteGroupToString

  (* Failure Cases to String*)
  let liftFailures =
    if List.isEmpty result.LiftFailures then
      [ "  <none>" ]
    else
      result.LiftFailures
      |> List.sortBy (fun (fn, _) -> fn.Address)
      |> List.map (fun (fn, reason) ->
        sprintf
          "  0x%08x  %s: %s"
          fn.Address
          (functionNameToString fn.Name)
          reason)

  [ [ sprintf "Binary: %s" result.BinaryPath
      sprintf "Valid B2R2 builders: %d" result.ValidFunctions.Length
      sprintf "Invalid B2R2 builders: %d" result.InvalidFunctions.Length
      sprintf "Functions with jump target 0: %d" groupedSites.Length
      sprintf "Jump target 0 sites: %d" result.Sites.Length
      "" ]
    "Valid B2R2 builders:" :: validFunctions
    [ "" ]
    "Invalid B2R2 builders:" :: invalidFunctions
    [ "" ]
    "Jump target 0 sites:" :: jumpZeroSites
    [ "" ]
    "SSA lift failures:" :: liftFailures ]
  |> List.concat
  |> String.concat "\n"
