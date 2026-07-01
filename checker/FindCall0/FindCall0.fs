module FindCall0

open System
open System.Diagnostics
open System.IO
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

type SymbolFunction =
  { Address: Addr
    Size: uint64 option
    Name: string }

type JumpZeroSite =
  { FunctionAddress: Addr
    FunctionName: string
    ProgramPoint: ProgramPoint
    Statement: Stmt }

type FindCall0Result =
  { BinaryPath: string
    SymbolFunctions: SymbolFunction list
    LiftedFunctions: int
    MissingB2R2Builder: SymbolFunction list
    LiftFailures: (SymbolFunction * string) list
    Sites: JumpZeroSite list }

/// Execute readelf to get functions in symbol table
let private runReadElf binaryPath =
  let startInfo =
    ProcessStartInfo (
      FileName = "readelf",
      Arguments = sprintf "-Ws %s" binaryPath,
      RedirectStandardOutput = true,
      RedirectStandardError = true,
      UseShellExecute = false
    )

  use proc = new Process (StartInfo = startInfo)
  proc.Start () |> ignore
  let stdout = proc.StandardOutput.ReadToEnd ()
  let stderr = proc.StandardError.ReadToEnd ()
  proc.WaitForExit ()

  if proc.ExitCode = 0 then
    stdout
  else
    failwithf "readelf failed with exit code %d: %s" proc.ExitCode stderr

let private tryParseHexUInt64 (text: string) =
  match UInt64.TryParse (text, Globalization.NumberStyles.HexNumber, null) with
  | true, value -> Some value
  | false, _ -> None

let private tryParseUInt64 (text: string) =
  match UInt64.TryParse text with
  | true, value -> Some value
  | false, _ -> None

/// From the result of readelf, extract functions in binary
let private parseSymbolLine (line: string) =
  let parts =
    line.Split ([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

  if parts.Length < 8 then
    None
  else
    let value = parts[1]
    let size = parts[2]
    let typ = parts[3]
    let name = parts[7]

    if typ <> "FUNC" then
      None
    else
      match tryParseHexUInt64 value with
      | Some 0UL
      | None -> None
      | Some address ->
        Some
          { Address = address
            Size = tryParseUInt64 size
            Name = name }

/// Construct the data structure to handle each function
let private symbolFunctions binaryPath =
  let extract1FuncFromAlias (_, symbols) =
    let sortedAlias =
      Seq.sortBy (fun symbol -> symbol.Name.Length, symbol.Name) symbols

    Seq.head sortedAlias

  let readElfResult = runReadElf binaryPath

  let parsedResult =
    readElfResult.Split (
      [| '\n'; '\r' |],
      StringSplitOptions.RemoveEmptyEntries
    )

  parsedResult
  |> Seq.choose parseSymbolLine
  |> Seq.groupBy (fun symbol -> symbol.Address)
  |> Seq.map extract1FuncFromAlias
  |> Seq.sortBy (fun symbol -> symbol.Address, symbol.Name)
  |> Seq.toList

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
  (symbol: SymbolFunction)
  (func: ICFGBuildable<DummyContext, DummyContext>)
  =
  let ssaCfg = lifter.Lift func.Context.CFG
  let constValue = constantValueFrom hdl ssaCfg

  let filterTarget0Jump (pp, stmt) =
    if isJumpTargetZero constValue stmt then
      Some
        { FunctionAddress = symbol.Address
          FunctionName = symbol.Name
          ProgramPoint = pp
          Statement = stmt }
    else
      None

  let ppStatements =
    ssaCfg.Vertices
    |> Seq.collect (fun vertex -> vertex.VData.Internals.Statements)

  ppStatements |> Seq.choose filterTarget0Jump |> Seq.toList

/// Find all statements that try to jump to 0 in given binary
let run binaryPath =
  let symbols = symbolFunctions binaryPath
  let hdl = BinHandle binaryPath
  let brew = BinaryBrew hdl
  let lifter = SSALifterFactory.Create hdl

  let folder (lifted, missing, failures, sites) symbol =
    match brew.Builders.TryGetBuilder symbol.Address with
    | Error _ -> lifted, symbol :: missing, failures, sites
    | Ok builder ->
      try
        let newSites = collectJumpZeroSites hdl lifter symbol builder

        lifted + 1, missing, failures, List.rev newSites @ sites
      with ex ->
        lifted, missing, (symbol, ex.Message) :: failures, sites

  let lifted, missing, failures, sites =
    symbols |> List.fold folder (0, [], [], [])

  { BinaryPath = binaryPath
    SymbolFunctions = symbols
    LiftedFunctions = lifted
    MissingB2R2Builder = List.rev missing
    LiftFailures = List.rev failures
    Sites = List.rev sites }

let toText result =
  let ppToString (pp: ProgramPoint) =
    sprintf "0x%08x+%d" pp.Address pp.Position

  let groupedSites =
    result.Sites
    |> List.groupBy (fun site -> site.FunctionAddress, site.FunctionName)
    |> List.sortBy fst

  (* Jump Target 0 to String *)
  let jmp0PerFuncStr ((address, name), sites) =
    let sortedSites =
      List.sortByDescending (fun site -> site.ProgramPoint) sites

    let funcHeder = sprintf "Function 0x%08x (%s)" address name

    let funcBody =
      List.fold
        (fun acc site ->
          let siteStr =
            sprintf "  %-16s %A" (ppToString site.ProgramPoint) site.Statement

          siteStr :: acc)
        []
        sortedSites

    funcHeder :: funcBody

  let jmp0Header = "Invalid jump target 0 sites:"

  let jmp0Content =
    if List.isEmpty groupedSites then
      [ "  <none>" ]
    else
      List.fold (fun acc group -> jmp0PerFuncStr group @ acc) [] groupedSites

  (* Missing Functions to String *)
  let missHeader = "Missing B2R2 builder symbols:"

  let missContent =
    if List.isEmpty result.MissingB2R2Builder then
      [ "  <none>" ]
    else
      List.fold
        (fun acc symbol ->
          let funcStr = sprintf "  0x%08x  %s" symbol.Address symbol.Name
          funcStr :: acc)
        []
        result.MissingB2R2Builder

  (* Failure Cases to String*)
  let faileHeader = "Lift fails:"

  let failContent =
    if List.isEmpty result.LiftFailures then
      [ "  <none>" ]
    else
      List.fold
        (fun acc (symbol, reason) ->
          let funcStr =
            sprintf "  0x%08x  %s: %s" symbol.Address symbol.Name reason

          funcStr :: acc)
        []
        result.LiftFailures

  [ [ sprintf "Binary: %s" result.BinaryPath
      sprintf "Symbol functions: %d" result.SymbolFunctions.Length
      sprintf "Lifted functions: %d" result.LiftedFunctions
      sprintf "Functions with invalid jump target 0: %d" groupedSites.Length
      sprintf "Invalid jump target 0 sites: %d" result.Sites.Length
      "" ]
    jmp0Header :: jmp0Content
    [ "" ]
    missHeader :: missContent
    [ "" ]
    faileHeader :: failContent ]
  |> List.concat
  |> String.concat "\n"
