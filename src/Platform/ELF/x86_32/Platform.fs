module PointerAnalyzer.Platform.ELF.X86_32.Platform

open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes

let private wordSize = 4

let private regId register =
  B2R2.FrontEnd.Intel.Register.toRegID register

let private tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

let private argumentRegisters = []

let private returnRegisters = [ regId B2R2.FrontEnd.Intel.Register.EAX ]

let private stackPointer = regId B2R2.FrontEnd.Intel.Register.ESP

let private tryRegisterArgumentIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> argumentRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

// Callee-side x86-32 stack arguments are represented by B2R2 as:
// arg0 = StackVar(-4), arg1 = StackVar(-8), ...
let private tryParameterStackIndex (variable: Variable) =
  match variable.Kind with
  | StackVar (_, offset) when offset <= -wordSize ->
    Some (argumentRegisters.Length + -offset / wordSize - 1)
  | _ -> None

let private tryCallStackIndex
  (context: CallSiteStackContext)
  (variable: Variable)
  =
  match context.ReturnAddressOffset, variable.Kind with
  | Some returnAddressOffset, StackVar (_, offset) when
    offset < returnAddressOffset
    ->
    let distance = returnAddressOffset - offset

    if distance % wordSize <> 0 then
      None
    else
      let index = argumentRegisters.Length + distance / wordSize - 1

      if index >= context.ParameterCount then None else Some index
  | _ -> None

let private tryReturnIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> returnRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

let create () =
  { Kind = ElfX86_32
    Name = "ELF x86-32"

    WordSize = wordSize
    IsAndMask = fun value -> value >= 0xFFFF0000UL

    StackPointer = stackPointer
    ArgumentRegisters = argumentRegisters
    ReturnRegisters = returnRegisters

    TryParameterIndex =
      fun variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryParameterStackIndex variable)
    TryCallArgumentIndex =
      fun context variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryCallStackIndex context variable)
    TryReturnIndex = tryReturnIndex }
