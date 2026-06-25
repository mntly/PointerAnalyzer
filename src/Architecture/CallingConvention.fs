module PointerAnalyzer.Platform.CallingConvention

open B2R2
open B2R2.BinIR.SSA
open B2R2.FrontEnd
open B2R2.FrontEnd.BinFile
open PointerAnalyzer

type CallingConventionKind =
  | X86Cdecl
  | X86Fastcall

type CallSiteStackContext =
  { ReturnAddressOffset: int option
    ParameterCount: int }

type CallingConvention =
  { Kind: CallingConventionKind
    Name: string
    WordSize: int
    ArgumentRegisters: RegisterID list
    ReturnRegisters: RegisterID list
    StackPointer: RegisterID
    TryParameterIndex: Variable -> int option
    TryCallArgumentIndex: CallSiteStackContext -> Variable -> int option
    TryReturnIndex: Variable -> int option }

module private X86 =
  let private regId register =
    B2R2.FrontEnd.Intel.Register.toRegID register

  let private tryRegisterId (variable: Variable) =
    match variable.Kind with
    | RegVar (_, registerId, _) -> Some registerId
    | _ -> None

  // Return Some (Stack Param Idx (Callee)): 0 ...
  let private tryParameterStackIndex
    registerArgCount
    wordSize
    (variable: Variable)
    =
    match variable.Kind with
    | StackVar (_, offset) when offset <= -wordSize ->
      Some (registerArgCount + -offset / wordSize - 1)
    | _ -> None

  let private tryCallStackIndex
    registerArgCount
    wordSize
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
        let index = registerArgCount + distance / wordSize - 1

        if index >= context.ParameterCount then None else Some index
    | _ -> None

  let create kind =
    let argumentRegisters =
      match kind with
      | X86Cdecl -> []
      | X86Fastcall ->
        [ regId B2R2.FrontEnd.Intel.Register.ECX
          regId B2R2.FrontEnd.Intel.Register.EDX ]

    let returnRegisters = [ regId B2R2.FrontEnd.Intel.Register.EAX ]

    let tryRegisterArgumentIndex (variable: Variable) =
      match tryRegisterId variable with
      | Some registerId ->
        argumentRegisters |> List.tryFindIndex ((=) registerId)
      | None -> None

    let tryParameterIndex (variable: Variable) =
      tryRegisterArgumentIndex variable
      |> Option.orElseWith (fun () ->
        tryParameterStackIndex argumentRegisters.Length 4 variable)

    let tryCallArgumentIndex context (variable: Variable) =
      tryRegisterArgumentIndex variable
      |> Option.orElseWith (fun () ->
        tryCallStackIndex argumentRegisters.Length 4 context variable)

    let tryReturnIndex (variable: Variable) =
      match tryRegisterId variable with
      | Some registerId -> returnRegisters |> List.tryFindIndex ((=) registerId)
      | None -> None

    { Kind = kind
      Name =
        match kind with
        | X86Cdecl -> "x86 cdecl"
        | X86Fastcall -> "x86 fastcall"
      WordSize = 4
      ArgumentRegisters = argumentRegisters
      ReturnRegisters = returnRegisters
      StackPointer = regId B2R2.FrontEnd.Intel.Register.ESP
      TryParameterIndex = tryParameterIndex
      TryCallArgumentIndex = tryCallArgumentIndex
      TryReturnIndex = tryReturnIndex }

module CallingConvention =
  let forBinary (architecture: Architecture) (handle: BinHandle) =
    match architecture, handle.File.Format with
    | ArchX86_32, FileFormat.PEBinary -> X86.create X86Fastcall
    | ArchX86_32, _ -> X86.create X86Cdecl
