module PointerAnalyzer.Frontend.SyscallDispatcherDetector

open B2R2
open B2R2.BinIR
open B2R2.BinIR.SSA
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open PointerAnalyzer.Frontend.FunctionDFA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Summary

let private tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

/// Extract incoming SSA varibles of givne FunctionDFA
let private incomingRegisters (dfa: FunctionDFA) =
  dfa.Edges.Uses.Keys
  (* Filter used before defined SSA variables *)
  |> Seq.filter (fun variable -> not (dfa.Edges.Defs.ContainsKey variable))
  (*
    Extract SSA variables used as incomming SSA variables by selectig minimum
    identifier
  *)
  |> Seq.choose (fun variable ->
    tryRegisterId variable
    |> Option.map (fun registerId -> registerId, variable))
  |> Seq.groupBy fst
  |> Seq.map (fun (registerId, variables) ->
    registerId,
    (variables
     |> Seq.map snd
     |> Seq.minBy (fun variable -> variable.Identifier)))
  |> Map.ofSeq

/// Extract the latest defined SSA variable of given register.
/// This is used to get the arguments used by syscall.
let private latestDefinition registerId statements beforeIndex =
  statements
  (* Filter until syscall *)
  |> Array.take beforeIndex
  (* From the last PP, extract latest defined SSA variables *)
  |> Array.rev
  |> Array.tryPick (fun (_, statement) ->
    match statement with
    | Def (variable, _)
    | Phi (variable, _) when tryRegisterId variable = Some registerId ->
      Some variable
    | _ -> None)

/// Get the latest defined SSA variables of given register at given syscall PP
let private reachingVariables
  (cfg: SSACFG)
  incoming
  blockId
  beforeIndex
  registerId
  =
  let vertices =
    cfg.Vertices |> Seq.map (fun block -> block.ID, block) |> Map.ofSeq

  let rec collect visited currentBlockId currentBeforeIndex =
    if Set.contains currentBlockId visited then
      None
    else
      let visited = Set.add currentBlockId visited
      let block = Map.find currentBlockId vertices
      let statements = block.VData.Internals.Statements

      match latestDefinition registerId statements currentBeforeIndex with
      | Some variable ->
        (*
          If target register is defined in current block, return latest defined
          SSA variable
        *)
        Some (Set.singleton variable)
      | None ->
        (*
          If not found, move bottom up to find latest defined SSA variable
        *)
        let predecessors = cfg.GetPreds block

        if Array.isEmpty predecessors then
          Map.tryFind registerId incoming |> Option.map Set.singleton
        else
          let reaching =
            predecessors
            |> Seq.map (fun predecessor ->
              collect
                visited
                predecessor.ID
                predecessor.VData.Internals.Statements.Length)
            |> Seq.toList

          if reaching |> List.forall Option.isSome then
            reaching |> List.choose id |> Set.unionMany |> Some
          else
            None

  collect Set.empty blockId beforeIndex

/// Check given variables all come from same function parameter
let private tryForwardedParameter platform (dfa: FunctionDFA) variables =
  (* Reverse trace given variable comes from function parameter *)
  let rec trace visited variable =
    if Set.contains variable visited then
      None
    else
      let visited = Set.add variable visited

      match dfa.Edges.Defs.TryGetValue variable with
      | false, _ ->
        (* Used before defined: Given from parameter *)
        platform.TryParameterIndex variable
      | true, Def (_, Var source) ->
        (* Recursivly trace defined variables *)
        trace visited source
      | true, Phi (destination, sourceIds) ->
        (* PHI node should resurcivly trace all source ids *)
        let parameters =
          sourceIds
          |> Array.map (fun identifier ->
            trace
              visited
              { destination with
                  Identifier = identifier })

        if Array.forall Option.isSome parameters then
          let uniqueParameters = parameters |> Array.choose id |> Set.ofArray

          if Set.count uniqueParameters = 1 then
            Some (Set.minElement uniqueParameters)
          else
            None
        else
          None
      | _ -> None

  let parameters = variables |> Seq.map (trace Set.empty) |> Seq.toList

  if not (List.isEmpty parameters) && List.forall Option.isSome parameters then
    let uniqueParameters = parameters |> List.choose id |> Set.ofList

    if Set.count uniqueParameters = 1 then
      Some (Set.minElement uniqueParameters)
    else
      None
  else
    None

let private trySiteDispatcher
  (platform: Platform)
  (abi: SyscallABI)
  (cfg: SSACFG)
  (dfa: FunctionDFA)
  (summary: SyscallSummary)
  (callSite: Addr)
  =
  dfa.Statements
  (* Filter syscall statement at callsite *)
  |> Array.tryFind (fun entry ->
    entry.ProgramPoint.Address = callSite
    && match entry.Statement with
       | SideEffect SysCall
       | SideEffect (Interrupt 0x80) -> true
       | _ -> false)
  |> Option.bind (fun entry ->
    (* Get block containing syscall(current statement) *)
    let block =
      cfg.Vertices |> Array.find (fun block -> block.ID = entry.BlockId)
    (* Get PP idx of syscall(current statement) *)
    let beforeIndex = entry.Index
    let incoming = incomingRegisters dfa

    (*
      Return parameter idx if given register defined before current statement
      comes from function paraemter
    *)
    let parameterFor registerId =
      reachingVariables cfg incoming block.ID beforeIndex registerId
      |> Option.bind (tryForwardedParameter platform dfa)

    (* Extract parameter Idx directly used as syscall number *)
    parameterFor abi.NumberRegister
    |> Option.bind (fun numberParameter ->
      (* Extract argument Idx directly used as syscall arguments *)
      let argumentParameters =
        abi.ArgumentRegisters
        |> Seq.choose (fun registerId ->
          parameterFor registerId
          |> Option.map (fun parameterIndex -> registerId, parameterIndex))
        |> Map.ofSeq

      if Map.isEmpty argumentParameters then
        (*
          ToDo!!!
            Current implementation assume all syscall needs parameters
        *)
        None
      else
        let forwardedParameters =
          argumentParameters
          |> Map.toSeq
          |> Seq.map snd
          |> Set.ofSeq
          |> Set.add numberParameter

        (* Construct SyscallDispatcherSummary to indicate syscall wrapper *)
        Some
          { NumberParameter = numberParameter
            ArgumentParameters = argumentParameters
            ForwardedParameters = forwardedParameters
            AbstractionOutputs = summary.AbstractionOutputs }))

/// Detect a syscall dispatcher from SSA data flow. Functions with a
/// constant syscall number are excluded and remain fixed wrappers.
let detect
  (platform: Platform)
  (cfg: SSACFG)
  (dfa: FunctionDFA)
  (syscallSummaries: Map<Addr, SyscallSummary>)
  =
  match platform.SyscallABI with
  | None -> None
  | Some abi ->
    let candidates =
      syscallSummaries
      |> Map.toSeq
      |> Seq.choose (fun (callSite, summary) ->
        trySiteDispatcher platform abi cfg dfa summary callSite)
      |> Seq.distinct
      |> Seq.toList

    match candidates with
    | [ candidate ] -> Some candidate
    | _ -> None
