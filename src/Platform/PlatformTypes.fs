module PointerAnalyzer.Platform.PlatformTypes

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA

/// <summary>
/// Tracks stack pointer values during main analysis.
/// </summary>
type StackPointerState =
  { Initial: Addr option
    Current: Addr option }

  /// Get offset between initial SP and current SP
  member this.TryDelta =
    let tryInt value =
      if
        value >= int64 System.Int32.MinValue
        && value <= int64 System.Int32.MaxValue
      then
        Some (int value)
      else
        None

    match this.Initial, this.Current with
    | Some initial, Some current -> int64 initial - int64 current |> tryInt
    | _ -> None

  /// Set current SP to given value
  member this.SetCurrent value = { this with Current = Some value }

  /// Set current SP to None
  member this.ForgetCurrent = { this with Current = None }

module StackPointerState =
  let empty = { Initial = None; Current = None }

  /// Initialize both initial SP and current SP to given value
  let initialize value =
    { Initial = Some value
      Current = Some value }

  let join left right =
    let joinOpt left right =
      match left, right with
      | Some left, Some right when left = right -> Some left
      | Some value, None
      | None, Some value -> Some value
      | _ -> None

    { Initial = joinOpt left.Initial right.Initial
      Current = joinOpt left.Current right.Current }

type PlatformKind = | ElfX86_32

type IntrinsicKind = | PCThunk

/// Expected pointer-analysis type of one syscall register.
type SyscallDataType =
  | SyscallAddress
  | SyscallValue
  | SyscallUnknown

/// Platform-specific signature selected by a syscall number.
type SyscallSignature =
  { Name: string
    Arguments: Map<RegisterID, SyscallDataType>
    Returns: Map<RegisterID, SyscallDataType>
    IsNoReturn: bool }

/// Platform-specific syscall calling convention and signature table.
type SyscallABI =
  { NumberRegister: RegisterID
    ArgumentRegisters: RegisterID list
    ClobberedRegisters: Set<RegisterID>
    TryFindSignature: uint64 -> SyscallSignature option }

type CallSiteStackContext =
  { StackPointer: StackPointerState
    ParameterCount: int }

type Platform =
  {
    /// Basic information of corresponding ABI
    Kind: PlatformKind
    Name: string

    /// Word size of correponding ABI
    WordSize: int

    /// Whether corresponding ABI stores return register before callee frame
    IsStack0Return: bool

    /// Register used as StackPointer
    StackPointer: RegisterID

    /// Registers used as parameter by regparams N
    RegParams: RegisterID list

    /// Registers used as function argument ordered by argument order
    ArgumentRegisters: RegisterID list

    /// Registers used as function return value
    ReturnRegisters: RegisterID list

    /// The all registers that may used as return value. For example, EDX for
    /// representing higher bit of 64 bit return value at x86-32
    ReturnSlotRegisters: RegisterID list

    /// CallerSaved Registers
    CallerSavedRegisters: Set<RegisterID>

    /// CalleeSaved Registers
    CalleeSavedRegisters: Set<RegisterID>

    /// The function transform the register name of given register Id
    RegisterName: RegisterID -> string

    /// Represent the syscall information
    SyscallABI: SyscallABI option

    /// The registers always Address type such as InstructionPointer
    TrivialAddressRegisters: Set<RegisterID>

    /// The registers always Value type such as Flag reigster
    TrivialValueRegisters: Set<RegisterID>

    /// Check given SSA variable is trivial address register
    IsTrivialAddress: Variable -> bool

    /// Check given SSA variable is trivial value register
    IsTrivialValue: Variable -> bool

    /// Get the return address of call
    TryCallReturnAddress: BinHandle -> Addr -> Addr option

    /// Get the parameter index of given SSA variable. If it is not used as
    /// parameter, return None.
    TryParameterIndex: Variable -> int option

    /// Get the argument index of given SSA variable. If it is not used as
    /// argument, return None.
    TryCallArgumentIndex: CallSiteStackContext -> Variable -> int option

    /// Get the argument index of given register. This only handle about
    /// argument passed by register.
    TryCallRegisterArgumentIndex:
      CallSiteStackContext -> RegisterID -> int option

    /// Get the argument index of stack offset. This only handle about
    /// argument passed by stack.
    TryCallStackSlotArgumentIndex: CallSiteStackContext -> int -> int option

    /// If given address is valid stack address, return offset from SP.
    /// This is used to handle storing to Stack B2R2 missinfer as MEM Store.
    TryActiveStackSlotOffset: StackPointerState -> Addr -> int -> int option

    /// Return offset of Return Address is stored
    TryCallReturnAddressStackSlot: CallSiteStackContext -> int option

    /// Transform the idxof return registers of given SSA variable
    TryReturnIndex: Variable -> int option
  }
