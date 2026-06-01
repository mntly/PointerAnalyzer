module PointerAnalyzer.Convention.X86_32.IntTypes

open PointerAnalyzer

/// Native integer type. x86.
module NInt =
  let ZERO: NInt = 0L
  let ONE: NInt = 1L
  let TWO: NInt = 2L
  let WORD_SIZE: NInt = 4L // x86.
  let BYTE_WIDTH: NInt = 8L
  let WORD_WIDTH: NInt = BYTE_WIDTH * WORD_SIZE

module NUInt =
  let ZERO: NUInt = 0UL
  let ONE: NUInt = 1UL
  let TWO: NUInt = 2UL
  let WORD_SIZE: NUInt = 4UL // x86.
  let BYTE_WIDTH: NUInt = 8UL
  let WORD_WIDTH: NUInt = BYTE_WIDTH * WORD_SIZE
  let ARCH_MAX_INT: NUInt = 0x7FFFFFFFUL // x86.
  let ARCH_MAX_UINT: NUInt = 0xFFFFFFFFUL // x86.
  let MAX_LIKELY_INT: NUInt = 0x7FFF0000UL // x86
  let SHL_LIMIT: NUInt = 16UL
  let VALID_WIDTHS: NUInt list = [ 8UL; 16UL; 32UL; 64UL ]

  let ofInt (i: int) : NUInt = uint64 i

  let toInt (x: NUInt) : int = int x

  let ofNInt (i: NInt) : NUInt = uint64 i

  let toNInt (x: NUInt) : NInt = int64 x

  let ofUInt64 (ui64: uint64) : NUInt =
    if ui64 > uint64 ARCH_MAX_UINT then
      failwithf "Invalid range: %x" ui64

    ui64

  let toUInt64 (x: NUInt) : uint64 = uint64 x

  let add (x: NUInt) (y: NUInt) = x + y

  let sub (x: NUInt) (y: NUInt) = x - y

  let rec private powOfTwoAux e acc =
    if e = 0UL then acc else powOfTwoAux (e - 1UL) (2UL * acc)

  let powOfTwo exp = powOfTwoAux exp 1UL

  let isAndMask (ui: NUInt) = ui >= 0xffff0000UL // x86

let values =
  { Architecture = ArchX86_32
    NInt =
      { Zero = NInt.ZERO
        One = NInt.ONE
        Two = NInt.TWO
        WordSize = NInt.WORD_SIZE
        ByteWidth = NInt.BYTE_WIDTH
        WordWidth = NInt.WORD_WIDTH }
    NUInt =
      { Zero = NUInt.ZERO
        One = NUInt.ONE
        Two = NUInt.TWO
        WordSize = NUInt.WORD_SIZE
        ByteWidth = NUInt.BYTE_WIDTH
        WordWidth = NUInt.WORD_WIDTH
        ArchMaxInt = NUInt.ARCH_MAX_INT
        ArchMaxUInt = NUInt.ARCH_MAX_UINT
        MaxLikelyInt = NUInt.MAX_LIKELY_INT
        ShlLimit = NUInt.SHL_LIMIT
        ValidWidths = NUInt.VALID_WIDTHS
        OfInt = NUInt.ofInt
        ToInt = NUInt.toInt
        OfNInt = NUInt.ofNInt
        ToNInt = NUInt.toNInt
        OfUInt64 = NUInt.ofUInt64
        ToUInt64 = NUInt.toUInt64
        Add = NUInt.add
        Sub = NUInt.sub
        PowOfTwo = NUInt.powOfTwo
        IsAndMask = NUInt.isAndMask } }
