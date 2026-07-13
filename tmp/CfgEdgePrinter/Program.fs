open System
open System.IO
open B2R2
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Frontend.BinaryLoader
open PointerAnalyzer.Frontend.ProgramDFA

let usage () =
  eprintfn
    "Usage: dotnet run --project tmp/CfgEdgePrinter/CfgEdgePrinter.fsproj -- <binary> <function-name-or-address> [output-file]"
  eprintfn
    "Example: dotnet run --project tmp/CfgEdgePrinter/CfgEdgePrinter.fsproj -- datas/binaries/helloword-x86_32-i586-uclibc-O0 _dl_aux_init"

let repoRoot = Directory.GetCurrentDirectory()

let resolvePath (path: string) =
  if Path.IsPathRooted path then path
  else Path.Combine(repoRoot, path)

let tryParseAddress (text: string) =
  let normalized =
    if text.StartsWith("0x", StringComparison.OrdinalIgnoreCase) then
      text.Substring 2
    else
      text

  match UInt64.TryParse(
          normalized,
          Globalization.NumberStyles.HexNumber,
          Globalization.CultureInfo.InvariantCulture
        )
    with
  | true, value -> Some value
  | false, _ -> None

let addrStr (addr: Addr) = sprintf "0x%08x" addr

let edgeKindStr (kind: CFGEdgeKind) = CFGEdgeKind.toString kind

let blockAddr (block: IVertex<SSABasicBlock>) =
  block.VData.Internals.BlockAddress

let blockInfo (block: IVertex<SSABasicBlock>) =
  sprintf
    "Node%d(%s, abstract=%b)"
    block.ID
    (addrStr (blockAddr block))
    block.VData.Internals.IsAbstract

let lastStmtLocation (block: IVertex<SSABasicBlock>) =
  let statements = block.VData.Internals.Statements

  if statements.Length = 0 then
    "<no stmt>"
  else
    let programPoint, _ = statements[statements.Length - 1]
    sprintf "%s+%d" (addrStr programPoint.Address) programPoint.Position

let edgeLines (cfg: SSACFG) (vertices: IVertex<SSABasicBlock> array) =
  [ for block in vertices do
      let succEdges = cfg.GetSuccEdges block
      let predEdges = cfg.GetPredEdges block

      yield sprintf "%s" (blockInfo block)
      yield sprintf "  preds: %d" predEdges.Length
      yield sprintf "  succs: %d" succEdges.Length
      yield sprintf "  last-pp: %s" (lastStmtLocation block)

      if succEdges.Length = 0 then
        yield "  edge: <leaf/no successor>"
      else
        for edge in succEdges do
          yield
            sprintf
              "  edge: -[%s]-> %s"
              (edgeKindStr edge.Label)
              (blockInfo edge.Second)

      yield "" ]

let leafLines (cfg: SSACFG) (vertices: IVertex<SSABasicBlock> array) =
  vertices
  |> Array.filter (fun block -> cfg.GetSuccs block |> Array.isEmpty)
  |> Array.map (fun block ->
    sprintf "%s  last-pp: %s" (blockInfo block) (lastStmtLocation block))
  |> Array.toList

let retEdgeLines (cfg: SSACFG) (vertices: IVertex<SSABasicBlock> array) =
  vertices
  |> Array.choose (fun block ->
    let retTargets =
      cfg.GetSuccEdges block
      |> Array.filter (fun edge -> edge.Label = CFGEdgeKind.RetEdge)
      |> Array.map (fun edge -> blockInfo edge.Second)

    if retTargets.Length = 0 then
      None
    else
      Some
        (sprintf
          "%s  retTargets=[%s]  last-pp: %s"
          (blockInfo block)
          (String.concat ", " retTargets)
          (lastStmtLocation block)))
  |> Array.toList

let edgeKindCountLines (cfg: SSACFG) (vertices: IVertex<SSABasicBlock> array) =
  vertices
  |> Seq.collect cfg.GetSuccEdges
  |> Seq.countBy (fun edge -> edge.Label)
  |> Seq.sortBy (fun (kind, _) -> edgeKindStr kind)
  |> Seq.map (fun (kind, count) -> sprintf "%s: %d" (edgeKindStr kind) count)
  |> Seq.toList

let selectFunction selector (program: ProgramDFAResult) =
  match tryParseAddress selector with
  | Some address ->
    program.Functions
    |> Map.tryFind address
    |> Option.defaultWith (fun () ->
      failwithf "Function address not found: %s" (addrStr address))
  | None ->
    program.Functions
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.tryFind (fun func -> func.Name = selector)
    |> Option.defaultWith (fun () -> failwithf "Function name not found: %s" selector)

let sanitizeFileName (name: string) =
  Path.GetInvalidFileNameChars()
  |> Array.fold (fun (acc: string) ch -> acc.Replace(ch, '_')) name

[<EntryPoint>]
let main argv =
  if argv.Length < 2 || argv.Length > 3 then
    usage ()
    1
  else
    let binaryPath = resolvePath argv[0]
    let selector = argv[1]

    let binary = BinaryLoader.load binaryPath
    let program = ProgramDFA.runDFA binary
    let func = selectFunction selector program
    let cfg: SSACFG = func.CFG

    let vertices: IVertex<SSABasicBlock> array =
      cfg.Vertices
      |> Array.sortBy (fun (block: IVertex<SSABasicBlock>) ->
        blockAddr block, block.ID)

    let leaves = leafLines cfg vertices
    let retEdges = retEdgeLines cfg vertices

    let outputPath =
      if argv.Length = 3 then
        resolvePath argv[2]
      else
        let safeName = sanitizeFileName func.Name
        Path.Combine(repoRoot, "tmp", sprintf "%s_edges.txt" safeName)

    Directory.CreateDirectory(Path.GetDirectoryName outputPath) |> ignore

    let lines =
      [ yield sprintf "Binary: %s" binaryPath
        yield sprintf "Function: %s (%s)" func.Name (addrStr func.Address)
        yield sprintf "Total blocks: %d" vertices.Length
        yield ""

        yield "====== Edge Kind Counts ======"
        yield! edgeKindCountLines cfg vertices
        yield ""

        yield "====== Leaf Nodes ======"
        if List.isEmpty leaves then yield "<none>" else yield! leaves
        yield ""

        yield "====== RetEdge Source Nodes ======"
        if List.isEmpty retEdges then yield "<none>" else yield! retEdges
        yield ""

        yield "====== Edges Per Block ======"
        yield! edgeLines cfg vertices ]

    File.WriteAllLines(outputPath, lines)

    printfn "Stored: %s" outputPath
    printfn "Function: %s (%s)" func.Name (addrStr func.Address)
    printfn "Total blocks: %d" vertices.Length
    printfn "Leaf nodes: %d" leaves.Length
    printfn "RetEdge source nodes: %d" retEdges.Length
    0
