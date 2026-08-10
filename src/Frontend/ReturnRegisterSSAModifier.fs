module PointerAnalyzer.Frontend.ReturnRegisterSSAModifier

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.MiddleEnd.ControlFlowGraph
open B2R2.MiddleEnd.SSA
open PointerAnalyzer.Platform.PlatformTypes

let private isRegisterDefinition registerId = function
  | Def ({ Kind = RegVar (_, definedId, _) }, _) -> definedId = registerId
  | _ -> false

let private insertBeforeTerminator statement statements =
  let insertIndex =
    statements
    |> Array.tryFindIndexBack (fun (_, stmt) ->
      match stmt with
      | Jmp _ -> true
      | _ -> false)
    |> Option.defaultValue statements.Length

  Array.concat
    [ Array.take insertIndex statements
      [| ProgramPoint.Fake, statement |]
      Array.skip insertIndex statements ]

let private addModifiedRegister
  (handle: BinHandle)
  registerId
  statements
  =
  let alreadyDefined =
    statements
    |> Array.exists (snd >> isRegisterDefinition registerId)

  if alreadyDefined then
    statements
  else
    let regType = handle.RegisterFactory.GetRegType registerId
    let regName = handle.RegisterFactory.GetRegisterName registerId

    let variable =
      { Kind = RegVar (regType, registerId, regName)
        Identifier = -1 }

    let statement = Def (variable, Undefined (regType, "modified-caller-saved"))
    insertBeforeTerminator statement statements

/// Create a callback that defines additional ABI return registers at calls to
/// functions detected as returning two machine-word slots.
let create
  (platform: Platform)
  (handle: BinHandle)
  (return64Functions: Set<Addr>)
  =
  let normalReturns = Set.ofList platform.ReturnRegisters

  let additionalReturns =
    platform.ReturnRegistersForSlotCount 2
    |> List.filter (fun registerId -> not (Set.contains registerId normalReturns))

  { new ISSAVertexCallback with
      member _.OnVertexCreation (_, _, vertex) =
        let block = vertex.VData.Internals

        if block.IsAbstract then
          let abstraction = block.AbstractContent

          if
            abstraction.EntryPoint <> 0UL
            && abstraction.ReturningStatus <> NoRet
            && Set.contains abstraction.EntryPoint return64Functions
          then
            let statements =
              additionalReturns
              |> List.fold
                (fun statements registerId ->
                  addModifiedRegister handle registerId statements)
                block.Statements

            block.UpdateStatements statements }
