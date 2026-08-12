#!/usr/bin/env python3
#
# readelf_dwarf_parser.py
#
# Parse `readelf --debug-dump=info` output into a DWARF DS
#

from __future__ import annotations

import os
import re
import subprocess
from dataclasses import dataclass, field

class ReadelfError(RuntimeError):
    """Raised when readelf cannot produce usable DWARF information."""

# Represent atrribute
@dataclass
class Attribute:
    value: object
    raw_value: str

# Represent the CompilationUnit
@dataclass
class CompilationUnit:
    # Offset in DWARF of this CU
    cu_offset: int
    # Default size of pointer of this CU
    address_size: int = 0
    # DIEs this CU contains
    dies: list["DIE"] = field(default_factory=list)

    def iter_DIEs(self):
        return iter(self.dies)

    def __getitem__(self, key):
        if key == "address_size":
            return self.address_size
        raise KeyError(key)

# Represent the DIE
@dataclass
class DIE:
    tag: str
    offset: int
    depth: int
    # CU that this DIE belongs to
    cu: CompilationUnit
    attributes: dict[str, Attribute] = field(default_factory=dict)
    children: list["DIE"] = field(default_factory=list)
    dwarf: "DwarfInfo | None" = field(default=None, repr=False)

    def iter_children(self):
        return iter(self.children)

    def get_DIE_from_attribute(self, name):
        attribute = self.attributes.get(name)
        if attribute is None or not isinstance(attribute.value, DIEReference):
            return None
        if self.dwarf is None:
            return None
        return self.dwarf.resolve_reference(self, attribute.value)

# Represent DIE needed to refer
@dataclass(frozen=True)
class DIEReference:
    offset: int

# Represent entire parsed DWARF information of given binary
@dataclass
class DwarfInfo:
    compilation_units: list[CompilationUnit]

    def __post_init__(self):
        self.dies_by_offset = {}
        for cu in self.compilation_units:
            for die in cu.dies:
                die.dwarf = self
                self.dies_by_offset[die.offset] = die

    def iter_CUs(self):
        return iter(self.compilation_units)

    def resolve_reference(self, source, reference):
        # readelf normalizes reference forms and prints the resolved
        # .debug_info offset in angle brackets.
        return self.dies_by_offset.get(reference.offset)

# Capture the offset from Compilation Unit definition
#   Compilation Unit @ offset 0:
# Captured: 0
CU_RE = re.compile(r"^\s*Compilation Unit @ offset\s+([^:]+):\s*$")
# Capture the pointer size of Compilation Unit
#   Pointer Size:  4
# Captured: 4
POINTER_SIZE_RE = re.compile(r"^\s*Pointer Size:\s*(\d+)\s*$")
# Capuater depth, offset, abbrev number,
# and (optional) DW TAG from DIE definition
# <0><c>: Abbrev Number: 1 (DW_TAG_compile_unit)
# Captured: 0, c, 1, DW_TAG_compile_unit
DIE_RE = re.compile(
    r"^\s*<(\d+)><([0-9a-fA-F]+)>:\s+Abbrev Number:\s+(\d+)"
    r"(?:\s+\((DW_TAG_[^)]+)\))?"
)
# Capture Tag name and corresponding value
#    <25>   DW_AT_name        : (strp) (offset: 0x1e4c3): _start
# Captured: DW_AT_name, (strp) (offset: 0x1e4c3): _start
ATTRIBUTE_RE = re.compile(
    r"^\s*<[0-9a-fA-F]+>\s+(DW_AT_[A-Za-z0-9_]+)\s*:\s*(.*)$"
)
# Extract only DIE offset from the value of attribute needed to refer
REFERENCE_RE = re.compile(r"<0x([0-9a-fA-F]+)>")

# Regex for remove prefixes of string(name)
# Remove "(...)" before target string
LEADING_FORM_RE = re.compile(r"^\s*\([^)]*\)\s*")
OFFSET_PREFIX_RE = re.compile(r"^\s*\(offset:\s*[^)]+\)\s*:\s*")

# Tags needed to refer correponding DIE
REFERENCE_ATTRIBUTES = {
    "DW_AT_type",
    "DW_AT_specification",
    "DW_AT_abstract_origin",
}
# Tags needed to handle as integer value
INTEGER_ATTRIBUTES = {
    "DW_AT_byte_size",
    "DW_AT_low_pc",
    "DW_AT_data_member_location",
}
# Tags needed to handle as boolean value
BOOLEAN_ATTRIBUTES = {"DW_AT_artificial", "DW_AT_declaration"}
# Tags needed to handle as string value
STRING_ATTRIBUTES = {"DW_AT_name", "DW_AT_linkage_name"}

# Parse integer value from forms such as "(data4) 0x123" and
# "(implicit_const) 4" end in the numeric value.
# Reject blocks/expressions instead of guessing.
def _parse_int(text):
    if "byte block:" in text or "DW_OP_" in text:
        return None
    # Extract all posible integers
    # (?<![A-Za-z0-9_]): Reject starts with "A-Za-z0-9_"
    values = re.findall(r"(?<![A-Za-z0-9_])-?(?:0x[0-9a-fA-F]+|\d+)", text)
    if not values:
        return None
    try:
        return int(values[-1], 0)
    except ValueError:
        return None

# Parse boolean attribute value
def _parse_bool(text):
    lowered = text.strip().lower()
    # The case that boolean is stored as string
    if lowered.endswith("true"):
        return True
    if lowered.endswith("false"):
        return False
    # The case that boolean is stored as int
    value = _parse_int(text)
    return bool(value) if value is not None else False

# Remove prefixes to get target string
def _parse_string(text):
    value = text.strip()
    while True:
        previous = value
        # Remove "(...)" before target string
        value = LEADING_FORM_RE.sub("", value, count=1)
        # Remove "(offset:...):" before target string
        value = OFFSET_PREFIX_RE.sub("", value, count=1)
        if value == previous:
            break
    return value.strip()

# Parse the raw value of each attribute
def _parse_attribute(name, raw_value):
    if name in REFERENCE_ATTRIBUTES:
        # Tags needed for refer corresponding DIE
        # Extract DIE offset from attribute value
        match = REFERENCE_RE.search(raw_value)
        value = DIEReference(int(match.group(1), 16)) if match else None
    elif name in BOOLEAN_ATTRIBUTES:
        # Tags needed to handle as boolean value
        value = _parse_bool(raw_value)
    elif name in INTEGER_ATTRIBUTES:
        # Tags needed to handle as integer value
        value = _parse_int(raw_value)
    elif name in STRING_ATTRIBUTES:
        # Tags needed to handle as string value
        value = _parse_string(raw_value)
    else:
        # Other cases, remain raw value
        value = raw_value.strip()
    return Attribute(value=value, raw_value=raw_value)

# Transform string integer into integer
def _parse_offset(text):
    value = text.strip()
    try:
        return int(value, 0)
    except ValueError:
        return int(value, 16)

# Parse readelf's decoded .debug_info text
def parse_debug_info(text):
    compilation_units = []
    current_cu = None
    current_die = None
    parents = []

    for line_number, line in enumerate(text.splitlines(), 1):
        cu_match = CU_RE.match(line)
        if cu_match:
            # Compilation Unit found
            current_cu = CompilationUnit(_parse_offset(cu_match.group(1)))
            compilation_units.append(current_cu)
            current_die = None
            parents = []
            continue

        if current_cu is None:
            # Out of Compilation Unit
            # CU and DIE must be in Compilation Unit
            continue

        pointer_match = POINTER_SIZE_RE.match(line)
        if pointer_match:
            # Pasrse pointer size information of CU
            current_cu.address_size = int(pointer_match.group(1))
            continue

        die_match = DIE_RE.match(line)
        if die_match:
            # DIE found
            # Parse depth, offset, and abbrev num of DIE
            depth = int(die_match.group(1))
            offset = int(die_match.group(2), 16)
            abbrev = int(die_match.group(3))
            tag = die_match.group(4)

            if abbrev == 0:
                # Abbrev Num 0 indicates that the DIE child list is finished
                current_die = None
                if depth < len(parents):
                    # Move to parent
                    parents = parents[:depth]
                continue
            if tag is None:
                # DIE Tag is None only when Abbrev Num 0
                raise ReadelfError(
                    f"line {line_number}: DIE has no DWARF tag: {line.strip()}"
                )
            if depth > len(parents):
                raise ReadelfError(
                    f"line {line_number}: unexpected DIE depth {depth}"
                )

            # Record current DIE definition
            die = DIE(tag, offset, depth, current_cu)
            if depth > 0:
                # Add current DIE as children of direct parrent
                if depth - 1 >= len(parents):
                    raise ReadelfError(
                        f"line {line_number}: DIE parent is missing"
                    )
                parents[depth - 1].children.append(die)
            
            # Update CU and parents based on the information of current DIE
            current_cu.dies.append(die)
            parents = parents[:depth]
            parents.append(die)
            current_die = die
            continue
        
        # Parsing attributes
        attribute_match = ATTRIBUTE_RE.match(line)
        if attribute_match and current_die is not None:
            name, raw_value = attribute_match.groups()
            current_die.attributes[name] = _parse_attribute(name, raw_value)

    if not compilation_units or not any(cu.dies for cu in compilation_units):
        raise ReadelfError("binary has no DWARF debug information")
    return DwarfInfo(compilation_units)

# Run readelf and return parsed DWARF information
def run_readelf(path, readelf="readelf"):
    environment = os.environ.copy()
    environment["LC_ALL"] = "C"
    try:
        result = subprocess.run(
            [readelf, "--wide", "--debug-dump=info", os.fspath(path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding="utf-8",
            errors="replace",
            env=environment,
            check=False,
        )
    except FileNotFoundError as error:
        raise ReadelfError(f"readelf executable was not found: {readelf}") from error
    except OSError as error:
        raise ReadelfError(f"cannot execute {readelf}: {error}") from error

    if result.returncode != 0:
        message = result.stderr.strip() or f"readelf exited with {result.returncode}"
        raise ReadelfError(message)
    return parse_debug_info(result.stdout)
