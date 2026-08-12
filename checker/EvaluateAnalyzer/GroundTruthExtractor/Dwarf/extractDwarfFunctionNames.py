#!/usr/bin/env python3
#
# extractDwarfFunctionNames.py
#
# Write GTExtractor whitelist names defined by DWARF object files
#

import argparse
import os
import sys
from pathlib import Path

from readelf_dwarf_parser import ReadelfError, run_readelf

def _attr_bool(die, name):
    attribute = die.attributes.get(name)
    return bool(attribute and attribute.value)

def _attr_string(die, name):
    attribute = die.attributes.get(name)
    if attribute is None or attribute.value is None:
        return None
    return str(attribute.value)

def function_name(die):
    return _attr_string(die, "DW_AT_linkage_name") or _attr_string(
        die, "DW_AT_name"
    )

# Extract function names of given objects
def extract_names(object_paths, readelf="readelf"):
    names = set()
    for object_path in object_paths:
        # Parse DWARF info
        try:
            dwarf = run_readelf(object_path, readelf)
        except ReadelfError as error:
            print(
                f"warning: ignore unusable DWARF in {object_path}: {error}",
                file=sys.stderr,
            )
            continue

        # Extract DEFINED function name: Ignore declare only functions
        for cu in dwarf.iter_CUs():
            for die in cu.iter_DIEs():
                if die.tag != "DW_TAG_subprogram":
                    continue
                if _attr_bool(die, "DW_AT_declaration"):
                    continue
                name = function_name(die)
                if name:
                    names.add(name)
    return sorted(names)

def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("output", type=Path)
    parser.add_argument("objects", nargs="+", type=Path)
    parser.add_argument(
        "--readelf", default=os.environ.get("READELF", "readelf")
    )
    args = parser.parse_args(argv)

    names = extract_names(args.objects, args.readelf)
    if not names:
        print(
            "archive members contain no named DWARF function definitions",
            file=sys.stderr,
        )
        return 1

    try:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("w", encoding="utf-8", newline="\n") as stream:
            for name in names:
                stream.write(f"{name}\n")
    except OSError as error:
        print(f"cannot write function whitelist: {error}", file=sys.stderr)
        return 1
    return 0

if __name__ == "__main__":
    sys.exit(main())
