module PointerAnalyzer.Platform.PlatformTypes

open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.SSA

type PlatformKind = | ElfX86_32

type IntrinsicKind = | PCThunk

type CallSiteStackContext =
  { ReturnAddressOffset: int option
    ParameterCount: int }

type Platform =
  { Kind: PlatformKind
    Name: string

    WordSize: int
    IsAndMask: uint64 -> bool

    StackPointer: RegisterID
    ArgumentRegisters: RegisterID list
    ReturnRegisters: RegisterID list

    TrivialAddressRegisters: Set<RegisterID>
    TrivialValueRegisters: Set<RegisterID>
    IsTrivialAddress: Variable -> bool
    IsTrivialValue: Variable -> bool

    CheckIntrinsic: IntrinsicKind -> BinHandle -> Addr -> RegisterID option

    TryParameterIndex: Variable -> int option
    TryCallArgumentIndex: CallSiteStackContext -> Variable -> int option
    TryReturnIndex: Variable -> int option }
