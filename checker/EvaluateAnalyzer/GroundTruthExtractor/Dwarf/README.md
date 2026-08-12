# DWARF Ground-Truth Extraction

This directory uses GNU `readelf` to parse DWARF information.

- [`extract_dwarf_gt.py`](./extract_dwarf_gt.py) reads a **linked binary** and constructs the complete,
  address-keyed ground-truth JSON database.
- [`extractDwarfFunctionNames.py`](./extractDwarfFunctionNames.py) reads **relocatable object files** and writes a whitelist containing functions defined by those objects.
- [`readelf_dwarf_parser.py`](./readelf_dwarf_parser.py) executes `readelf`, parses its textual output, rebuilds compilation units and DIE trees, and resolves DIE references.

## Shortcuts

* [Requirements](#requirements)
* [Construct ground truth from a linked binary](#construct-ground-truth-from-a-linked-binary)
* [Construct a whitelist from object files](#construct-a-whitelist-from-object-files)

## Requirements

Python 3 and GNU Binutils `readelf` must be installed. By default the scripts
executes `readelf`; a cross-toolchain executable can be selected with either the
`READELF` environment variable or `--readelf`:

```sh
READELF=mips-linux-gnu-readelf python3 extract_dwarf_gt.py program-gt

python3 extract_dwarf_gt.py --readelf mips-linux-gnu-readelf program-gt
```

The parser parses the result of `readelf --wide --debug-dump=info INPUT`

## Construct ground truth from a linked binary

```sh
python3 extract_dwarf_gt.py program-gt \
  --log program_GTExtract.log \
  --whitelist functions.txt > program_GT.json
```

The processing stages are:

1. Parse all declaration `DW_TAG_subprogram` DIEs and index their signatures
   by source and linkage names.
2. Parse definition DIEs. A definition must not have `DW_AT_declaration` and
   must have a nonzero `DW_AT_low_pc`; that linked address becomes the JSON key.
3. Resolve parameters, return values, wrapper types, and recursive structures.
4. Incorporate compatible signatures reached through declarations,
   `DW_AT_specification`, and `DW_AT_abstract_origin`.
5. Merge compatible signatures that share an address. A concrete type or size
   fills an `Unknown` type or zero size; conflicting concrete information is
   rejected.
6. Apply the optional whitelist to the completed address-keyed database.

The output has this shape:

```json
{
  "0x00401120": {
    "Name": "update",
    "Args": [{ "Size": 8, "Type": "Address" }],
    "Return": [{ "Size": 4, "Type": "Value" }]
  }
}
```

The whitelist is an allow-list, not a list of required GT entries. A whitelist
name may be absent from the linked binary. Both a definition's source name and
linkage name are considered when filtering.

## Construct a whitelist from object files

Archive members should first be made available as individual object paths.
Then run:

```sh
python3 extractDwarfFunctionNames.py functions.txt member1.o member2.o
```

For each usable object, the script selects DIEs satisfying:

```text
tag == DW_TAG_subprogram
and DW_AT_declaration is not true
and a source or linkage name exists
```

The selected name is `DW_AT_linkage_name` when present, otherwise `DW_AT_name`. Declaration-only functions are excluded.

Names from all usable objects are deduplicated, sorted, and written one per
line. An object without usable DWARF produces a warning and contributes no
names; other objects are still processed. The command fails when the final
whitelist is empty.
