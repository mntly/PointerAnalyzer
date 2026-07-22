module Checker.Return64Detection.Analysis.UseChainClassifier

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.BinGraph
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.DataFlow
open Checker.Return64Detection.Return64Types
open Checker.Return64Detection.Analysis.StatementIndex

/// FLAG registers.
let private flagRegisters =
  [ Intel.Register.DF
    Intel.Register.IF
    Intel.Register.TF
    Intel.Register.CF
    Intel.Register.PF
    Intel.Register.AF
    Intel.Register.ZF
    Intel.Register.SF
    Intel.Register.OF ]
  |> List.map Intel.Register.toRegID
  |> Set.ofList

/// Recursivly check given variable exists in given expr.
let rec private containsVariable variable expr =
  match expr with
  | Var value -> value = variable
  | UnOp (_, _, inner)
  | Cast (_, _, inner)
  | Extract (inner, _, _) -> containsVariable variable inner
  | BinOp (_, _, left, right)
  | RelOp (_, _, left, right) ->
    containsVariable variable left || containsVariable variable right
  | Ite (condition, _, trueExpr, falseExpr) ->
    containsVariable variable condition
    || containsVariable variable trueExpr
    || containsVariable variable falseExpr
  | Load (_, _, address) -> containsVariable variable address
  | Store (_, _, address, value) ->
    containsVariable variable address || containsVariable variable value
  | ExprList expressions -> List.exists (containsVariable variable) expressions
  | Num _
  | FuncName _
  | Undefined _ -> false

/// Recursivly check given variable used as address or stored to memory in
/// given expr.
let rec private hasMemoryRole variable expr =
  match expr with
  | Load (_, _, address) -> containsVariable variable address
  | Store (_, _, address, value) ->
    containsVariable variable address || containsVariable variable value
  | UnOp (_, _, inner)
  | Cast (_, _, inner)
  | Extract (inner, _, _) -> hasMemoryRole variable inner
  | BinOp (_, _, left, right)
  | RelOp (_, _, left, right) ->
    hasMemoryRole variable left || hasMemoryRole variable right
  | Ite (condition, _, trueExpr, falseExpr) ->
    hasMemoryRole variable condition
    || hasMemoryRole variable trueExpr
    || hasMemoryRole variable falseExpr
  | ExprList expressions -> List.exists (hasMemoryRole variable) expressions
  | Var _
  | Num _
  | FuncName _
  | Undefined _ -> false

/// Check given variable is FLAG register.
let private isFlag variable =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Set.contains registerId flagRegisters
  | _ -> false

/// Check given variable is temporal SSA variable
let private isTempVarOrFlag variable =
  match variable.Kind with
  | TempVar _ -> true
  | _ -> isFlag variable

/// Based on heuristic rules, check given variable is valid return register
type Classifier (cfg: SSACFG) =
  let edges = SSAEdges cfg
  (* Constract BlockId * StmtIdx -> Stmts mapping *)
  let statements = StatementIndex.build cfg

  /// Log the reason why corresponding variable is not return register by
  /// heuristic rule.
  let failure variable entry reason =
    { Variable = variable
      ProgramPoint = entry.ProgramPoint
      Reason = reason }

  /// Based on heuristic rules, classify given variable is valid return
  /// register or not.<br>
  /// ============================= Heuristic rule =============================
  ///   1. The register should become at return leaf nodes.
  ///   2. The register is not used or only used to calulate FLAG/Temp register
  ///<br>
  ///     1) The FLAG/Temp registers calculated from return register does not
  ///       used or only used to calculate FLAG/Temp register.<br>
  ///     2) ...
  /// <br>
  ///
  /// ====================================================================
  member _.Classify variable blockIds =
    let rec visit visited current =
      if Set.contains current visited then
        (* Current variable already seen(and analyzed) => Skip *)
        []
      else
        let visited = Set.add current visited

        (* Heuristic Rules for determining current variable is valid or not *)
        let heuristicRule entry =
          match entry.Statement with
          | Def (_, expression) when hasMemoryRole current expression ->
            (* Current variable is used for address or storing value *)
            [ failure current entry "used by memory load/store" ]
          | Def (destination, _) when isTempVarOrFlag destination ->
            (*
              If current variable is used for computing TempVar or Flag
              register, check TempVar fits the heuristic rules
            *)
            visit visited destination
          | Def _ ->
            (*
              Other definition: current variable is used to generate other
              computation => Invalid current variable as return register
            *)
            [ failure current entry "used to define an observable variable" ]
          | Phi (destination, _) -> visit visited destination
          | Jmp _ -> [ failure current entry "used by control flow" ]
          | ExternalCall _ ->
            [ failure current entry "used by a function call" ]
          | SideEffect _ -> [ failure current entry "used by a side effect" ]
          | LMark _ -> [ failure current entry "used by label" ]

        match edges.Uses.TryGetValue current with
        | false, _ ->
          (* Current variable is not used *)
          []
        | true, uses ->
          uses
          (* Extract stmt that current varialbe is used *)
          |> Seq.choose (fun location -> Map.tryFind location statements)
          (* Filter stmts only in target blocks *)
          |> Seq.filter (fun entry -> Set.contains entry.BlockId blockIds)
          (* Apply heuristic rules *)
          |> Seq.collect heuristicRule
          |> Seq.toList

    visit Set.empty variable |> List.distinct
