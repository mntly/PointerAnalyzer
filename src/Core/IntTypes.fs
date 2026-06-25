namespace PointerAnalyzer

type Architecture = | ArchX86_32

module Architecture =
  let ofString arch =
    match arch with
    | "x86"
    | "x86_32"
    | "i386"
    | "i586" -> ArchX86_32
    | _ ->
      invalidArg (nameof arch) (sprintf "Unsupported architecture: %s" arch)

type NInt = int64
type NUInt = uint64

type NativeNInt =
  { Zero: NInt
    One: NInt
    Two: NInt
    WordSize: NInt
    ByteWidth: NInt
    WordWidth: NInt }

type NativeNUInt =
  { Zero: NUInt
    One: NUInt
    Two: NUInt
    WordSize: NUInt
    ByteWidth: NUInt
    WordWidth: NUInt
    ArchMaxInt: NUInt
    ArchMaxUInt: NUInt
    MaxLikelyInt: NUInt
    ShlLimit: NUInt
    ValidWidths: NUInt list
    OfInt: int -> NUInt
    ToInt: NUInt -> int
    OfNInt: NInt -> NUInt
    ToNInt: NUInt -> NInt
    OfUInt64: uint64 -> NUInt
    ToUInt64: NUInt -> uint64
    Add: NUInt -> NUInt -> NUInt
    Sub: NUInt -> NUInt -> NUInt
    PowOfTwo: NUInt -> NUInt
    IsAndMask: NUInt -> bool }

type NativeIntTypes =
  { Architecture: Architecture
    NInt: NativeNInt
    NUInt: NativeNUInt }
