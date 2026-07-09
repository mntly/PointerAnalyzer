#!/usr/bin/env python3
import json
import os
import sys

# Import pyelftools to parse DWARF debug information
try:
    from elftools.elf.elffile import ELFFile
except ImportError:
    print(
        "pyelftools is not installed. Install it with: python3 -m pip install pyelftools",
        file=sys.stderr,
    )
    sys.exit(2)

# PointerAnalyzer's inferred type result
ADDRESS = "Address"
VALUE = "Value"
UNKNOWN = "Unknown"

# Get attribute of given DIE
def attr(die, name):
    return die.attributes.get(name)

# Transform boolean DWARF attribute from int to boolean
def attr_bool(die, name):
    value = attr(die, name)
    return bool(value and value.value)

# Transform string DWARF attribute to string
def attr_string(die, name):
    value = attr(die, name)
    if value is None:
        return None
    raw = value.value
    if isinstance(raw, bytes):
        return raw.decode("utf-8", errors="replace")
    return str(raw)

# In DWARF, DW_AT_name and DW_AT_linkage_name represent the function name
def function_name(die):
    return attr_string(die, "DW_AT_linkage_name") or attr_string(die, "DW_AT_name")

# Access DIE that represents the type of given DIE
def get_ref_die(die, attr_name):
    try:
        return die.get_DIE_from_attribute(attr_name)
    except Exception:
        return None

# Given type DIE, resolve its type
# seen prevents cycle when the type is defined using itself
def resolve_type(die, seen=None):
    # Type DIE not exist -> Mark as Unknown
    if die is None:
        return UNKNOWN

    # If resolve_type is first call, initialize seen
    if seen is None:
        seen = set()

    # If type is defined by the discovered type,
    # assume can not determine its type.
    # In this cycle case, Mark as Unknown
    die_key = (die.cu.cu_offset, die.offset)
    if die_key in seen:
        return UNKNOWN

    seen.add(die_key)

    # Classify Type
    tag = die.tag

    if tag in (
        "DW_TAG_pointer_type",
        "DW_TAG_array_type",
        "DW_TAG_subroutine_type",
        "DW_TAG_reference_type",
        "DW_TAG_rvalue_reference_type",
    ):
        return ADDRESS

    if tag in (
        "DW_TAG_base_type",
        "DW_TAG_enumeration_type",
        "DW_TAG_structure_type",
        "DW_TAG_union_type",
        "DW_TAG_class_type",
    ):
        return VALUE

    if tag in (
        "DW_TAG_typedef",
        "DW_TAG_const_type",
        "DW_TAG_volatile_type",
        "DW_TAG_restrict_type",
        "DW_TAG_atomic_type",
        "DW_TAG_packed_type",
    ):
        return resolve_type(get_ref_die(die, "DW_AT_type"), seen)

    return UNKNOWN

# Extract type of return value
def return_types(die):
    # Type of return value of function is represented as type of itself.
    type_die = get_ref_die(die, "DW_AT_type")
    if type_die is None:
        return []
    return [resolve_type(type_die)]

# Extract type of parameters of given function DIE
def parameter_types(die):
    result = []
    for child in die.iter_children():
        # Child DIE of Subprogram DIE with DW_TAG_formal_parameter
        # represents the parameters of corresponding function
        if child.tag != "DW_TAG_formal_parameter":
            continue
        
        # DW_AT_artificial represents the object or types
        # that are not actually declared in the source code
        if attr_bool(child, "DW_AT_artificial"):
            continue
        
        # Recursivly access DW_AT_type to extract concrete type
        result.append(resolve_type(get_ref_die(child, "DW_AT_type")))
    return result

# Normalize to Hex string
def normalize_addr(value):
    return f"0x{int(value):08x}"

# Check current DIE is function definition or not
def is_function_definition(die):
    if die.tag != "DW_TAG_subprogram":
        # DW_TAG_subprogram reoresents function name
        return False
    if attr_bool(die, "DW_AT_declaration"):
        # DW_AT_declaration represetns incomplete, non-defining, or seperate
        # entity declaration
        return False
    # DW_AT_low_pc represents code adress
    low_pc = attr(die, "DW_AT_low_pc")
    return low_pc is not None and int(low_pc.value) != 0


def unknown_count(signature):
    return signature["Args"].count(UNKNOWN) + signature["Return"].count(UNKNOWN)


def choose_duplicate(signatures):
    return sorted(
        signatures,
        key=lambda item: (unknown_count(item), -len(item["Args"]), item["Name"]),
    )[0]

def signature_to_text(signature):
    args = ", ".join(signature["Args"])
    returns = ", ".join(signature["Return"])
    return f'{signature["Name"]}: ({args}) -> ({returns})'

# Merge single type
def merge_type(left, right):
    if left == right:
        return left
    if left == UNKNOWN:
        return right
    if right == UNKNOWN:
        return left
    return None

# Merge type list left and right
def merge_type_list(left, right):
    if len(left) != len(right):
        return None

    merged = []
    for left_type, right_type in zip(left, right):
        merged_type = merge_type(left_type, right_type)
        if merged_type is None:
            return None
        merged.append(merged_type)
    return merged

# Merge siganture
# Only merge if Unknown type fits with concrete sigantures.
# Resect at least one conflict with Address and Value
def merge_signature(left, right):
    args = merge_type_list(left["Args"], right["Args"])
    returns = merge_type_list(left["Return"], right["Return"])

    if args is None or returns is None:
        return None

    names = sorted(set(left.get("Names", [left["Name"]]) + [right["Name"]]))
    return {
        "Name": names[0],
        "Names": names,
        "Args": args,
        "Return": returns,
    }


def merge_duplicate_signatures(address, signatures):
    # Not exist multiple functions in same address
    if len(signatures) == 1:
        return signatures[0], []

    # Loggind multiple function signatures with same address
    log_lines = [
        f"Address {address} has multiple DWARF function signatures:"
    ]

    for signature in signatures:
        log_lines.append(f"  - {signature_to_text(signature)}")

    # Incorporate multiple function signatures
    merged = {
        "Name": signatures[0]["Name"],
        "Names": [signatures[0]["Name"]],
        "Args": signatures[0]["Args"],
        "Return": signatures[0]["Return"],
    }

    for signature in signatures[1:]:
        # Merge if Unknown and concrete type fits with same location
        next_merged = merge_signature(merged, signature)
        if next_merged is None:
            log_lines.append("Decision: rejected as TypeMismatch")
            return None, log_lines
        merged = next_merged

    result = {
        "Name": merged["Name"],
        "Args": merged["Args"],
        "Return": merged["Return"],
    }

    log_lines.append(f"Merged: {signature_to_text(result)}")
    log_lines.append("Decision: accepted")
    return result, log_lines


# Extract GT information from given GT binary
def extract(binary_path):
    with open(binary_path, "rb") as stream:
        elf = ELFFile(stream)
        
        # Check given binary is compiled with debugging option
        if not elf.has_dwarf_info():
            raise RuntimeError("binary has no DWARF debug information")

        # Get DWARFInfo context object
        dwarf = elf.get_dwarf_info()

        by_addr = {}

        # Iterate all debug information of each Compile Unit
        for cu in dwarf.iter_CUs():
            # Iterate all Debuggin Information Entries in each Compile Unit
            for die in cu.iter_DIEs():
                # Only need the DIE of function definition
                if not is_function_definition(die):
                    continue

                # Extract function name
                name = function_name(die)
                if not name:
                    continue

                # Extract function address
                address = normalize_addr(attr(die, "DW_AT_low_pc").value)
                # Extract types of Function parameters and return value
                signature = {
                    "Name": name,
                    "Args": parameter_types(die),
                    "Return": return_types(die),
                }

                # Insert the GR information of current function
                by_addr.setdefault(address, []).append(signature)

        if not by_addr:
            # Can not detect any functions
            raise RuntimeError(
                "No DWARF function definitions found. Compile the ground-truth binary with debug information."
            )
        
        db = {}
        logs = []

        # Handle GT multiple function signature at same address
        for address, signatures in sorted(by_addr.items()):
            signature, log_lines = merge_duplicate_signatures(address, signatures)
            logs.extend(log_lines)
            # Update log
            if log_lines:
                logs.append("")
            # Update GT DB
            if signature is not None:
                db[address] = signature

        return db, logs


def main(argv):
    # Argument parsing
    if len(argv) != 2 and len(argv) != 4:
        print(
            "usage: extract_dwarf_gt.py <ground-truth-binary> [--log <log-path>]",
            file=sys.stderr,
        )
        return 1

    ## Extract ground truth binary for extract ground truth information
    binary_path = argv[1]
    log_path = None

    if len(argv) == 4:
        if argv[2] != "--log":
            print(
                "usage: extract_dwarf_gt.py <ground-truth-binary> [--log <log-path>]",
                file=sys.stderr,
            )
            return 1
        log_path = argv[3]

    # Check given file exists
    if not os.path.isfile(binary_path):
        print(f"ground-truth binary does not exist: {binary_path}", file=sys.stderr)
        return 1

    # Extract ground truth
    try:
        db, logs = extract(binary_path)
    except Exception as ex:
        print(str(ex), file=sys.stderr)
        return 1

    # Store mismatched/unknown type issue occurs as log
    if log_path is not None:
        os.makedirs(os.path.dirname(log_path), exist_ok=True)
        with open(log_path, "w", encoding="utf-8") as stream:
            stream.write("\n".join(logs))
            if logs:
                stream.write("\n")

    # Propagate GT result to F# handler
    print(json.dumps(db, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
