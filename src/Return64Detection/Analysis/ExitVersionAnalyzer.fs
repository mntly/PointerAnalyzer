module PointerAnalyzer.Return64Detection.Analysis.ExitVersionAnalyzer

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Return64Detection.Return64Types

let private eax = Intel.Register.toRegID Intel.Register.EAX
let private edx = Intel.Register.toRegID Intel.Register.EDX

/// Extract register id of given SSA varaible
let private tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

/// If given stmt is defining EAX or EDX, update corresponding state
let private transferStatement (state: RegisterState) (stmt: Stmt) =
  let update variable =
    match tryRegisterId variable with
    | Some registerId when registerId = eax ->
      { state with
          EAX = VersionState.define variable }
    | Some registerId when registerId = edx ->
      { state with
          EDX = VersionState.define variable }
    | _ -> state

  match stmt with
  | Def (variable, _)
  | Phi (variable, _) -> update variable
  | _ -> state

/// Transfer block by transferring statements in block
let private transferBlock
  (state: RegisterState)
  (block: IVertex<SSABasicBlock>)
  =
  block.VData.Internals.Statements
  |> Seq.map snd
  |> Seq.fold transferStatement state

/// Join output states of predecessors of given block
let private joinInputs
  (cfg: SSACFG)
  (blockIds: Set<VertexID>)
  (outputState: Map<VertexID, RegisterState>)
  (block: IVertex<SSABasicBlock>)
  =
  (* Filter valid predecessors of given block *)
  let selectedPreds =
    cfg.GetPreds block
    |> Array.filter (fun predecessor -> Set.contains predecessor.ID blockIds)

  if Array.isEmpty selectedPreds then
    (* No predecessors. No input state *)
    RegisterState.empty
  else
    (* If predecessors exist, join(Set union) all of their output states *)
    selectedPreds
    |> Array.map (fun predecessor ->
      (* Select output state of predecessor *)
      Map.tryFind predecessor.ID outputState
      |> Option.defaultValue RegisterState.empty)
    (* Join all of them *)
    |> Array.reduce RegisterState.join

/// Add blocks to analyze to WorkList(Back)
let private enqueueMany values (front, back) =
  front, List.fold (fun queued value -> value :: queued) back values

/// Dequeue WorkList Queue:(Front,Back), return first element and new WorkList
let private tryDequeue =
  function
  | head :: tail, back -> Some (head, (tail, back))
  | [], [] -> None
  | [], back ->
    (*
      Newly added block goes to the first of back, not goes to the last of front
      for efficiency. Therefore, if  analyzer analyzed all blocks in front,
      re-fill front as reversed back.
    *)
    match List.rev back with
    | head :: tail -> Some (head, (tail, []))
    | [] -> None

/// Extract live EAX and EDX register at return leaf node.
let analyze (cfg: SSACFG) (range: ReturnRange) =
  (* Convert Block Id into CFG Block *)
  let blocks = range.BlockIds |> Seq.map cfg.FindVertex |> Seq.toList

  (* Analyze the first block of WorkList and move to next *)
  let rec iterate worklist inputState outputState =
    match tryDequeue worklist with
    | None ->
      (* No more block to analyze *)
      outputState
    | Some (block: IVertex<SSABasicBlock>, worklist) ->
      (* Get input state used for analyzing current block *)
      let input = joinInputs cfg range.BlockIds outputState block

      if Map.tryFind block.ID inputState = Some input then
        (* Already up-to-date. Move to next block *)
        iterate worklist inputState outputState
      else
        (* Update inputState of current block as joined result *)
        let inputState = Map.add block.ID input inputState
        (* Analyze block and get outputState *)
        let output = transferBlock input block

        if Map.tryFind block.ID outputState = Some output then
          (* Already up-to-date. Move to next block *)
          iterate worklist inputState outputState
        else
          (* Update outputState of current block as joined result *)
          let outputState = Map.add block.ID output outputState

          (* Since outputState is changed, propagate to successors *)
          let successors =
            cfg.GetSuccs block
            (* Filter only target blocks *)
            |> Array.filter (fun successor ->
              Set.contains successor.ID range.BlockIds)
            |> Array.toList

          (* Update WorkList by adding successors *)
          let worklist = enqueueMany successors worklist
          iterate worklist inputState outputState

  let outputState = iterate (blocks, []) Map.empty Map.empty

  (* Extract return leaf node's output state *)
  Map.tryFind range.LeafId outputState
  |> Option.defaultValue RegisterState.empty
