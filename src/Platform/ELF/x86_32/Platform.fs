module PointerAnalyzer.Platform.ELF.X86_32.Platform

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA
open PointerAnalyzer.Platform.PlatformTypes
open PointerAnalyzer.Platform.ELF.X86_32

let private wordSize = 4

let private regId register = Intel.Register.toRegID register

let private esp = regId Intel.Register.ESP
let private eax = regId Intel.Register.EAX
let private ecx = regId Intel.Register.ECX
let private edx = regId Intel.Register.EDX
let private ebx = regId Intel.Register.EBX
let private ebp = regId Intel.Register.EBP
let private esi = regId Intel.Register.ESI
let private edi = regId Intel.Register.EDI

let private tryRegisterId (variable: Variable) =
  match variable.Kind with
  | RegVar (_, registerId, _) -> Some registerId
  | _ -> None

let private argumentRegisters = []

let private returnRegisters = [ eax ]

/// Ordered machine-word registers that may form an integer return value.
let private returnSlotRegisters = [ eax; edx ]

let private callerSavedRegisters = Set.ofList [ eax; ecx; edx ]

let private calleeSavedRegisters = Set.ofList [ ebx; ebp; esi; edi ]

let private registerName registerId =
  Intel.Register.ofRegID registerId |> Intel.Register.toString

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

/// Returns the index of arguments passed by register
let private tryRegisterArgumentIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> argumentRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

/// Returns the index of arguments passed by register
let private tryCallRegisterArgumentIndex
  (_context: CallSiteStackContext)
  (registerId: RegisterID)
  =
  argumentRegisters |> List.tryFindIndex ((=) registerId)

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
  match context.StackPointer.TryDelta, variable.Kind with
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

/// Get index of arguments (passed through stack) of given offset
let private tryCallStackSlotArgumentIndex
  (context: CallSiteStackContext)
  (offset: int)
  =
  match context.StackPointer.TryDelta with
  | Some returnAddressOffset when offset < returnAddressOffset ->
    let distance = returnAddressOffset - offset

    if distance % wordSize <> 0 then
      None
    else
      let index = argumentRegisters.Length + distance / wordSize - 1

      if index >= context.ParameterCount then None else Some index
  | _ -> None

/// Return offset of Return Address is stored.
/// In B2R2, it transforms call instruction into push and jmp,
/// so offset of Return Address is offset of current SP
let private tryCallReturnAddressStackSlot (context: CallSiteStackContext) =
  context.StackPointer.TryDelta

/// Get index of return register of given variabel
let private tryReturnIndex (variable: Variable) =
  match tryRegisterId variable with
  | Some registerId -> returnRegisters |> List.tryFindIndex ((=) registerId)
  | None -> None

/// After call instruction, right next instruction of callsite is executed
let private tryCallReturnAddress (handle: BinHandle) (callSite: Addr) =
  let liftingUnit = handle.NewLiftingUnit ()

  match liftingUnit.TryParseInstruction callSite with
  | Ok instruction -> Some (callSite + uint64 instruction.Length)
  | Error _ -> None

let create () =
  { Kind = ElfX86_32
    Name = "ELF x86-32"

    WordSize = wordSize

    IsStack0Return = true

    StackPointer = stackPointer
    ArgumentRegisters = argumentRegisters
    ReturnRegisters = returnRegisters
    ReturnSlotRegisters = returnSlotRegisters
    CallerSavedRegisters = callerSavedRegisters
    CalleeSavedRegisters = calleeSavedRegisters
    RegisterName = registerName
    SyscallABI = Some (Syscall.create ())

    TrivialAddressRegisters = trivialAddressRegisters
    TrivialValueRegisters = trivialValueRegisters
    IsTrivialAddress = isTrivialAddress
    IsTrivialValue = isTrivialValue

    TryCallReturnAddress = tryCallReturnAddress

    TryParameterIndex =
      fun variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryParameterStackIndex variable)
    TryCallArgumentIndex =
      fun context variable ->
        tryRegisterArgumentIndex variable
        |> Option.orElseWith (fun () -> tryCallStackIndex context variable)
    TryCallRegisterArgumentIndex = tryCallRegisterArgumentIndex
    TryCallStackSlotArgumentIndex = tryCallStackSlotArgumentIndex
    TryCallReturnAddressStackSlot = tryCallReturnAddressStackSlot
    TryReturnIndex = tryReturnIndex }
