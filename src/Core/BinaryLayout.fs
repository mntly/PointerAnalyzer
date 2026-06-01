namespace PointerAnalyzer

open B2R2

type BinaryRegionKind =
  | CodeRegion
  | GlobalRegion

type BinaryRegion =
  { Name: string
    Base: Addr
    Size: uint64
    Kind: BinaryRegionKind }

type BinaryLayout =
  { ImageBase: Addr
    Regions: BinaryRegion list }

module BinaryLayout =
  let empty =
    { ImageBase = 0UL
      Regions = [] }

  let private contains addr region =
    let last = region.Base + region.Size
    region.Size > 0UL && region.Base <= addr && addr < last

  let tryFindRegion kind addr layout =
    layout.Regions
    |> List.tryFind (fun region -> region.Kind = kind && contains addr region)

  let getRegionBase kind fallback addr layout =
    match tryFindRegion kind addr layout with
    | Some region -> region.Base
    | None -> fallback

  let getRegionOffset kind fallbackBase addr layout =
    let baseAddr = getRegionBase kind fallbackBase addr layout
    int (int64 addr - int64 baseAddr)
