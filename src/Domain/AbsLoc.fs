module PointerAnalyzer.AbsDom.AbsLoc

open B2R2
open PointerAnalyzer
open PointerAnalyzer.Utils
open PointerAnalyzer.AbsDom.Signature
open PointerAnalyzer.AbsDom.Functor
open PointerAnalyzer.AbsDom.AbsInt

exception NotSymbolicException
exception NonComparableLocsException
exception UnknownBaseException

/// B2R2's function identity is the function entry address. Richer function
/// metadata lives in B2R2's Function/FunctionAbstraction types.
type Subroutine = Addr

module Subroutine =
  let toString (subrtn: Subroutine) = Addr.toFuncName subrtn

/// B2R2 has no general Offset wrapper. SSA stack variables use int offsets, so
/// this domain follows that convention.
type Offset = int

module Offset =
  let ZERO: Offset = 0

  let ofInt (i: int) : Offset = i
  let toInt (x: Offset) : int = x
  let ofAddr (addr: Addr) : Offset = int addr
  let toString (x: Offset) = sprintf "0x%x" x

  let add (x: Offset) (y: Offset) : Offset = x + y
  let sub (x: Offset) (y: Offset) : Offset = x - y

module private B2R2Addr =
  // TODO!!! Why add x -1 = x + 1?
  let addOffset (addr: Addr) (offset: Offset) : Addr =
    if offset >= 0 then
      addr + uint64 offset
    else
      addr - uint64 -offset

  let subOffset (addr: Addr) (offset: Offset) : Addr =
    if offset >= 0 then
      addr - uint64 offset
    else
      addr + uint64 -offset

type AbsLoc =
  | Stack of Subroutine * Offset
  | Global of Addr
  | Heap of Allocsite: Addr * Offset * Size: AbsInt
  | Code of Addr
  | Unknown of Addr
  | SLoc of SymbolLoc * Offset

type AbsLocModule (absInt: AbsIntModule) =
  inherit Elem<AbsLoc> ()

  let intTypes = IntTypesLoader.load absInt.architecture
  let convention = AbsLocLoader.load absInt.architecture

  let b2r2WordSize =
    match intTypes.NInt.WordWidth with
    | 32L -> WordSize.Bit32
    | 64L -> WordSize.Bit64
    | width -> failwithf "Unsupported B2R2 address word width: %d" width

  let addrToString (addr: Addr) = Addr.toString b2r2WordSize addr

  override __.toString loc =
    match loc with
    | Stack (sub, offset) when offset >= Offset.ZERO ->
      sprintf "Stack_%s+%s" (Subroutine.toString sub) (Offset.toString offset)
    | Stack (sub, offset) -> // when offset < 0
      let offset' = -offset
      sprintf "Stack_%s-%s" (Subroutine.toString sub) (Offset.toString offset')
    | Global addr -> addrToString addr |> sprintf "Global(%s)"
    | Heap (allocsite, offset, size) when offset >= Offset.ZERO ->
      let allocStr = addrToString allocsite
      let offsetStr = Offset.toString offset
      let sizeStr = absInt.toString size
      sprintf "Heap_%s+%s(size=%s)" allocStr offsetStr sizeStr
    | Heap (allocst, offset, _) -> // when offset < Offset.zero
      let offsetStr = Offset.toString -offset
      sprintf "Heap_%s-%s" (addrToString allocst) offsetStr
    | SLoc (sym, offset) when offset >= Offset.ZERO ->
      sprintf "%s+%s" (SymbolLoc.toString sym) (Offset.toString offset)
    | SLoc (sym, offset) -> // when offset < Offset.zero
      let offsetStr = Offset.toString -offset
      sprintf "%s-%s" (SymbolLoc.toString sym) offsetStr
    | Code addr -> addrToString addr |> sprintf "Code(%s)"
    | Unknown addr -> addrToString addr |> sprintf "UnknownLoc(%s)"


  /// Return a string * offset pair for sorting abstract locations.
  member __.getOrder loc =
    match loc with
    | Stack (sub, offset) -> "Stack_" + addrToString sub, offset
    | Global addr -> "Global_" + addrToString addr, Offset.ofAddr addr
    | Heap (allocst, offset, _) ->
      let allocStr = addrToString allocst
      "Heap_" + allocStr, offset
    | SLoc (sym, offset) -> sprintf "%s" (SymbolLoc.toString sym), offset
    | Code addr -> "Code_" + addrToString addr, Offset.ofAddr addr
    | Unknown addr -> "Unkonwn_" + addrToString addr, Offset.ofAddr addr

  // Create functions.

  member __.makeGlobalLoc addr = Global addr
  member __.makeStackLoc subrtn offset = Stack (subrtn, offset)
  member __.makeHeapLoc allocst offset size = Heap (allocst, offset, size)
  member __.makeSymLoc sym offset = SLoc (sym, offset)

  // Binary operation functions.

  member __.addOffset loc i =
    match loc with
    | Stack (sub, offset) -> Stack (sub, Offset.add offset i)
    | Global addr -> Global (B2R2Addr.addOffset addr i)
    | Heap (allocst, offset, size) -> Heap (allocst, Offset.add offset i, size)
    | SLoc (sym, offset) -> SLoc (sym, Offset.add offset i)
    | Code addr -> Code (B2R2Addr.addOffset addr i)
    | Unknown addr -> Unknown (B2R2Addr.addOffset addr i)

  member __.subOffset loc i =
    match loc with
    | Stack (sub, offset) -> Stack (sub, Offset.sub offset i)
    | Global addr -> Global (B2R2Addr.subOffset addr i)
    | Heap (allocst, offset, size) -> Heap (allocst, Offset.sub offset i, size)
    | SLoc (sym, offset) -> SLoc (sym, Offset.sub offset i)
    | Code addr -> Code (B2R2Addr.subOffset addr i)
    | Unknown addr -> Unknown (B2R2Addr.subOffset addr i)

  // ToDo!! Handle Base addr of Global, Code
  member __.isSameBase loc1 loc2 =
    match loc1, loc2 with
    | Stack (sub1, _), Stack (sub2, _) -> sub1 = sub2
    | Global addr1, Global addr2 -> addr1 = addr2
    | Heap (a1, _, _), Heap (a2, _, _) -> a1 = a2
    | SLoc (sym1, _), SLoc (sym2, _) -> sym1 = sym2
    | Unknown addr1, Unknown addr2 -> addr1 = addr2
    | Code _, Code _ -> true
    | _ -> false

  member __.findDistance loc1 loc2 =
    match loc1, loc2 with
    | Stack (sub1, offset1), Stack (sub2, offset2) when sub1 = sub2 ->
      offset1 - offset2
    | Global a1, Global a2 -> int (int64 a1 - int64 a2)
    | Heap (a1, offset1, _), Heap (a2, offset2, _) when a1 = a2 ->
      offset1 - offset2
    | SLoc (sym1, offset1), SLoc (sym2, offset2) when sym1 = sym2 ->
      offset1 - offset2
    | Unknown a1, Unknown a2 -> int (int64 a1 - int64 a2)
    | Code a1, Code a2 -> int (int64 a1 - int64 a2)
    | _ -> raise NonComparableLocsException

  // Checker functions.

  member __.isStack =
    function
    | Stack _ -> true
    | Global _ -> false
    | Heap _ -> false
    | Unknown _ -> false
    | Code _ -> false
    | SLoc _ -> false

  member __.isGlobal =
    function
    | Stack _ -> false
    | Global _ -> true
    | Heap _ -> false
    | Code _ -> false
    | Unknown _ -> false
    | SLoc _ -> false

  member __.isHeap =
    function
    | Stack _ -> false
    | Global _ -> false
    | Heap _ -> true
    | Unknown _ -> false
    | Code _ -> false
    | SLoc _ -> false

  member __.isSymbolic =
    function
    | Stack _ -> false
    | Global _ -> false
    | Heap _ -> false
    | Code _ -> false
    | Unknown _ -> false
    | SLoc _ -> true

  /// Check if a given location should be considered as an argument.
  member __.isArgument =
    function
    | Stack (_, offset) -> convention.IsStackArgument offset
    | Global _ -> false // For scalability, ignore side effect on globals.
    | Heap _ -> false // Caution: heap memory is not an argument.
    | Code _ -> false // Caution: Code is not an argument.
    | Unknown _ -> false // Caution: Unknown Location is not an argument.
    | SLoc _ -> convention.IsSymbolicArgument

  /// Check if a given location is a 'root' argument location, which is not
  /// derived from another argument location.
  member __.isRootArgument =
    function
    | Stack (_, offset) -> convention.IsStackRootArgument offset
    | Global _ -> false // For scalability, ignore side effect on globals.
    | Heap _ -> false
    | Unknown _ -> false
    | Code _ -> false
    | SLoc _ -> convention.IsSymbolicRootArgument

  /// Check if this location is for n-th argument. Note that symbolic locations
  /// with register symbol are not our interest here.
  member __.isNthArgument n =
    function
    | Stack (_, offset) -> convention.IsNthStackArgument n offset
    | _ -> false

  member __.isInvalid =
    function
    // Filter out locations that are likely to be spurious (FP), since access
    // to these locations indicate buffer underflow.
    | Heap (_, offset, _)
    | SLoc (_, offset) -> offset < Offset.ZERO
    | Stack _
    | Code _
    | Unknown _
    | Global _ -> false

  // Getter functions.

  member __.getSymbol =
    function
    | Stack _
    | Global _
    | Code _
    | Unknown _
    | Heap _ -> raise NotSymbolicException
    | SLoc (sym, _) -> sym

  member __.getBase =
    function
    | Stack (subrtn, _) -> Stack (subrtn, Offset.ZERO)
    | Global _ -> Global 0UL
    | Code _ -> Code 0UL
    | Unknown _ -> raise UnknownBaseException
    | Heap (allocsite, _, _) -> Heap (allocsite, Offset.ZERO, absInt.bot)
    | SLoc (sym, _) -> SLoc (sym, Offset.ZERO)

  member __.getOffset =
    function
    | Stack (_, offset) -> offset
    | Global addr -> Offset.ofAddr addr
    | Code addr -> Offset.ofAddr addr
    | Unknown addr -> raise UnhandledSyscallException
    | Heap (_, offset, _) -> offset
    | SLoc (_, offset) -> offset

  /// Get the argument index of a stack argument location. Note that symbolic
  /// locations with register symbol are not our interest here.
  member __.tryGetRootArgIdx =
    function
    | Stack (_, offset) -> convention.TryGetStackRootArgIdx offset
    | Global _ -> None
    | Code _ -> None
    | Unknown _ -> None
    | Heap _ -> None
    | SLoc _ -> None

  member __.setSize loc size =
    match loc with
    | Stack _ -> loc
    | Global _ -> loc
    | Code _ -> loc
    | Unknown _ -> loc
    | Heap (allocsite, offset, _) -> Heap (allocsite, offset, size)
    | SLoc _ -> loc

type AbsLocSet = Set<AbsLoc>

type AbsLocSetModule (absInt: AbsIntModule) =
  inherit SetDomain<AbsLoc> (AbsLocModule absInt)

  let absLoc = AbsLocModule absInt

  member __.addOffset locSet i =
    Set.map (fun loc -> absLoc.addOffset loc i) locSet

  member __.subOffset locSet i =
    Set.map (fun loc -> absLoc.subOffset loc i) locSet

  member __.resolveSymbol loc intSymMap locSymMap =
    match loc with
    | Stack _
    | Code _
    | Unknown _
    | Global _ -> Set.singleton loc
    | Heap (allocsite, offset, size) ->
      let newSize = absInt.substitute size intSymMap
      Set.singleton (Heap (allocsite, offset, newSize))
    | SLoc (sym, offset) ->
      if not (Map.containsKey sym locSymMap) then
        Set.empty
      else
        Set.map (fun l -> absLoc.addOffset l offset) (Map.find sym locSymMap)

  /// Symbol substitution function.
  member __.substitute locSet intSymMap locSymMap =
    Set.fold
      (fun accSet loc ->
        __.join (__.resolveSymbol loc intSymMap locSymMap) accSet)
      Set.empty
      locSet

  /// Collect symbols used in 'locSet'.
  member __.collectSymbols locSet =
    let folder accSet =
      function
      | Stack _
      | Code _
      | Unknown _
      | Global _ -> accSet
      | Heap (_, _, size) ->
        let symbols = absInt.collectSymbols size |> Set.map IntSymbol
        Set.union symbols accSet
      | SLoc (sym, _) -> Set.add (LocSymbol sym) accSet

    Set.fold folder Set.empty locSet

  /// Remove stack locations from the set.
  member __.nullifyStackLoc locSet =
    Set.filter (not << absLoc.isStack) locSet

module AbsLocDomain =
  let create architecture =
    let absInt = AbsIntDomain.create architecture
    AbsLocModule absInt

  let createSet architecture =
    let absInt = AbsIntDomain.create architecture
    AbsLocSetModule absInt

  let createFromString architecture =
    Architecture.ofString architecture |> create

  let createSetFromString architecture =
    Architecture.ofString architecture |> createSet
