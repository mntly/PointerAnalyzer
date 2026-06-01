namespace PointerAnalyzer.Summary

open B2R2
open B2R2.BinIR.SSA
open PointerAnalyzer.AbsDom.AbsVal
open PointerAnalyzer.AbsDom.AnalysisState
open PointerAnalyzer.AbsDom.TypeMap

// Store the variable type per instruction

/// Type information for variables at one instruction.
type VarTypeMap = Map<Variable, TypePtr>

module VarTypeMap =
  let empty: VarTypeMap = Map.empty

  let find (var: Variable) varTypMap : TypePtr =
    match Map.tryFind var varTypMap with
    | Some typ -> typ
    | None -> TypePtr.bot

  let add var typ varTypMap : VarTypeMap = Map.add var typ varTypMap

/// ProgramPoint |-> Variable |-> TypePtr.
type TypePerInst = Map<ProgramPoint, VarTypeMap>

module TypePerInst =
  let empty: TypePerInst = Map.empty

  let findVarMap pp (typPerInst: TypePerInst) =
    match Map.tryFind pp typPerInst with
    | Some varTMap -> varTMap
    | None -> Map.empty

  let tryFind pp var (typPerInst: TypePerInst) =
    typPerInst |> Map.tryFind pp |> Option.bind (Map.tryFind var)

  let find pp var typPerInst =
    let varTMap = findVarMap pp typPerInst
    VarTypeMap.find var varTMap

  let add pp var typePtr typPerInst =
    let oldVarTMap = findVarMap pp typPerInst
    let newVarTMap = VarTypeMap.add var typePtr oldVarTMap
    Map.add pp newVarTMap typPerInst

  let addVal pp var (value: AbsVal) typPerInst =
    let _, _, typePtr = value
    add pp var typePtr typPerInst

  let addMany pp entries typPerInst =
    entries
    |> Seq.fold (fun acc (var, typePtr) -> add pp var typePtr acc) typPerInst

  // ToDo: Add register only shown current instruction
  let addRegMap pp regMap typPerInst =
    regMap
    |> Map.toSeq
    |> Seq.fold (fun acc (var, value) -> addVal pp var value acc) typPerInst

  let addState pp (state: AnalysisState) typPerInst =
    addRegMap pp state.Registers typPerInst

  let contains pp var typPerInst =
    let varTMap = findVarMap pp typPerInst

    match Map.tryFind var varTMap with
    | Some _ -> true
    | None -> false

  let toList (typPerInst: TypePerInst) =
    let extractVarMap (pp, varMap: VarTypeMap) =
      let varMaplst = Map.toList varMap
      List.map (fun (var, typePtr) -> pp, var, typePtr) varMaplst

    typPerInst |> Map.toList |> List.collect extractVarMap

  let toString typPerInst =
    let ext2str (pp: ProgramPoint, var, typePtr) =
      sprintf
        "%O: %s -> %s"
        pp
        (Variable.ToString var)
        (TypePtr.toString typePtr)

    typPerInst |> toList |> List.map ext2str |> String.concat "\n"
