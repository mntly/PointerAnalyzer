# Ground-Truth Converter

This directory converts source-level ground truth extracted from DWARF into
the ABI-level word slots used by PointerAnalyzer.

## Why conversion is separate

DWARF describes source types. PointerAnalyzer reports types for locations
selected by a concrete platform ABI. A source structure is one argument, but
it may occupy several word-sized stack locations. The converter preserves the
one-argument grouping while describing the individual locations that can be
compared.

The evaluator pipeline is:

```text
source GT JSON
  -> EvaluatorParseJson
  -> GroundTruthConverter
  -> converted GT JSON
  -> ElementEvaluator
```

## Source types

The raw type model is recursive:

```text
Address
Value
Unknown
Structure(fields)
```

An inline nested structure is recursively expanded. A pointer to a structure
is still `Address` and is not expanded.

Field offsets are relative to their containing structure. During conversion,
the converter adds the offsets of all containing structures to determine the
final ABI slot.

## Converted elements

Each converted element contains:

- `Size`: source-level byte size.
- `SourceType`: `Normal` or `Structure` after conversion.
- `OccupiedSlotCount`: number of ABI slots reserved by the complete element.
- `Slots`: indexed locations with an expected `Address`, `Value`, or `Unknown`
  type.

`OccupiedSlotCount` controls the starting location of the next argument.
`Slots` controls type comparison. Keeping them separate handles padding and
sparse structure fields.

Each slot also stores a field `Path`, such as `inner.buffer`, for diagnostics.

## x86-32 ELF

The implementation under `ELF/x86_32` uses the word size from
`analysisConfig.json`.

- Normal values occupy `ceil(Size / WordSize)` slots.
- Inline structure fields are recursively flattened into their containing
  structure's slots.
- A structure argument remains one converted argument, but its ABI slots are
  separate `Address`/`Value` evaluation elements.
- A structure return prepends a synthetic one-word `Address` element to
  converted `Args`. This represents the hidden return-buffer pointer visible
  in the binary-level signature.

When several small structure fields share one machine-word slot, conversion
stores one `Value` slot. The slot is evaluated once; its field paths are kept
together for diagnostics.

The synthetic return-buffer argument is evaluated and counted like a normal
argument. It becomes argument index zero, and source-level arguments shift by
one. A missing inferred hidden argument therefore produces one normal
argument failure.

The main converter dispatches by the `Platform` field in
`analysisConfig.json`.

## Partial structure coverage

For a structure argument, the evaluator first checks whether PointerAnalyzer
inferred at least one of its Word-Size slots:

- No observed structure slots: no confusion-metric element, but the function
  is logged as failed and as a parameter-count mismatch.
- At least one observed slot: evaluate every GT structure slot. An observed
  slot uses its inferred type, while a missing slot uses `Unknown` and is
  classified as failed.
- Every GT slot produces one result once the structure is considered detected,
  using that slot's concrete `Address` or `Value` ground-truth type.

The argument cursor always advances by `OccupiedSlotCount`, including missing
or padding slots. Missing slots of normal arguments remain failures.

Coverage is reported separately in `StructureSlotCoverage` and in the
evaluation log. `ObservedSlots` still counts only slots actually inferred by
PointerAnalyzer. `GTAll` counts comparable structure slots, while `All` counts
all GT slots of every detected structure and excludes completely unobserved
structures.

## Stored output

Evaluator mode stores the converted artifact as:

```text
SUFFIX_ConvertedGT.json
```

The file includes the selected platform, word size, converted functions,
occupied slot counts, slot indices, and field paths. The synthetic hidden
return-buffer argument appears directly as the first `Args` element. The file
is regenerated from the source GT whenever evaluation runs.
