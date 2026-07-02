module PointerAnalyzer.Platform.ELF.X86_32.Platform

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes

let private wordSize = 4

let private regId register = Intel.Register.toRegID register

let private esp = regId Intel.Register.ESP
let private eax = regId Intel.Register.EAX
let private ecx = regId Intel.Register.ECX
let private edx = regId Intel.Register.EDX
let private ebx = regId Intel.Register.EBX
let private esi = regId Intel.Register.ESI
let private edi = regId Intel.Register.EDI

let private tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

let private argumentRegisters = []

let private returnRegisters = [ eax ]

let private stackPointer = esp

let private trivialAddressRegisters = Set.ofList [ stackPointer ]

let private trivialValueRegisters =
  Set.ofList
    [ regId Intel.Register.DF
      regId Intel.Register.IF
      regId Intel.Register.TF
      regId Intel.Register.CF
      regId Intel.Register.PF
      regId Intel.Register.AF
      regId Intel.Register.ZF
      regId Intel.Register.SF
      regId Intel.Register.OF ]

let private isTrivialAddress (variable: Variable) =
  match variable.Kind with
  | PCVar _ -> true
  | RegVar (_, registerId, _) -> Set.contains registerId trivialAddressRegisters
  | _ -> false

let private isTrivialValue (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Set.contains registerId trivialValueRegisters
  | _ -> false

let private tryRegisterArgumentIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> argumentRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

(*
  Callee-side x86-32 stack arguments are represented by B2R2 as:
  arg0 = StackVar(-4), arg1 = StackVar(-8), ...
*)
let private tryParameterStackIndex (variable: Variable) =
  match variable.Kind with
  | StackVar (_, offset) when offset <= -wordSize ->
    Some (argumentRegisters.Length + -offset / wordSize - 1)
  | _ -> None

/// Get index of arguments (passed through stack) of given variable
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

/// Get index of return register of given variabel
let private tryReturnIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> returnRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

/// Heuristic handling of get pcthunk instrinsic functions
let private tryPCThunk (handle: BinHandle) (address: Addr) =
  match handle.TryReadBytes (address, 4) with
  | Ok [| 0x8Buy; 0x04uy; 0x24uy; 0xC3uy |] -> Some eax
  | Ok [| 0x8Buy; 0x0Cuy; 0x24uy; 0xC3uy |] -> Some ecx
  | Ok [| 0x8Buy; 0x14uy; 0x24uy; 0xC3uy |] -> Some edx
  | Ok [| 0x8Buy; 0x1Cuy; 0x24uy; 0xC3uy |] -> Some ebx
  | Ok [| 0x8Buy; 0x34uy; 0x24uy; 0xC3uy |] -> Some esi
  | Ok [| 0x8Buy; 0x3Cuy; 0x24uy; 0xC3uy |] -> Some edi
  | _ -> None

let private CheckIntrinsic kind handle address =
  match kind with
  | PCThunk -> tryPCThunk handle address

let create () =
  { Kind = ElfX86_32
    Name = "ELF x86-32"

    WordSize = wordSize
    IsAndMask = fun value -> value >= 0xFFFF0000UL

    StackPointer = stackPointer
    ArgumentRegisters = argumentRegisters
    ReturnRegisters = returnRegisters

    TrivialAddressRegisters = trivialAddressRegisters
    TrivialValueRegisters = trivialValueRegisters
    IsTrivialAddress = isTrivialAddress
    IsTrivialValue = isTrivialValue

    CheckIntrinsic = CheckIntrinsic

    TryParameterIndex =
      fun variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryParameterStackIndex variable)
    TryCallArgumentIndex =
      fun context variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryCallStackIndex context variable)
    TryReturnIndex = tryReturnIndex }
