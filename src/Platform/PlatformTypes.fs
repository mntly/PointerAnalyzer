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

type CallSiteStackContext =
  { StackPointer: StackPointerState
    ParameterCount: int }

type Platform =
  { Kind: PlatformKind
    Name: string

    WordSize: int

    IsStack0Return: bool

    StackPointer: RegisterID
    ArgumentRegisters: RegisterID list
    ReturnRegisters: RegisterID list
    RegisterName: RegisterID -> string

    TrivialAddressRegisters: Set<RegisterID>
    TrivialValueRegisters: Set<RegisterID>
    IsTrivialAddress: Variable -> bool
    IsTrivialValue: Variable -> bool

    TryCallReturnAddress: BinHandle -> Addr -> Addr option

    TryParameterIndex: Variable -> int option
    TryCallArgumentIndex: CallSiteStackContext -> Variable -> int option
    TryCallRegisterArgumentIndex:
      CallSiteStackContext -> RegisterID -> int option
    TryCallStackSlotArgumentIndex: CallSiteStackContext -> int -> int option
    TryCallReturnAddressStackSlot: CallSiteStackContext -> int option
    TryReturnIndex: Variable -> int option }
