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

# Type tags of DWARF
ADDR_TAGS = [
    "DW_TAG_pointer_type",
    "DW_TAG_array_type",
    # "DW_TAG_subroutine_type",
    # "DW_TAG_reference_type",
    # "DW_TAG_rvalue_reference_type",
]
VALUE_TAGS = [
    "DW_TAG_base_type",
    "DW_TAG_enumeration_type",
    # "DW_TAG_structure_type",
    # "DW_TAG_union_type",
    # "DW_TAG_class_type",
]
RECURSIVE_TAGS = [
    "DW_TAG_typedef",
    # "DW_TAG_const_type",
    # "DW_TAG_volatile_type",
    "DW_TAG_restrict_type",
    # "DW_TAG_atomic_type",
    # "DW_TAG_packed_type",
]

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

def function_names(die):
    names = []
    for name in [attr_string(die, "DW_AT_linkage_name"), attr_string(die, "DW_AT_name")]:
        if name and name not in names:
            names.append(name)
    return names

# Access DIE that represents the type of given DIE
def get_ref_die(die, attr_name):
    try:
        return die.get_DIE_from_attribute(attr_name)
    except Exception:
        return None

# Transform DIE info to string
def die_location(die):
    if die is None:
        return "<none>"
    return f"cu=0x{die.cu.cu_offset:x}, die=0x{die.offset:x}"

# Transform TypeTag history into string
def type_path_to_text(path):
    if not path:
        return "<empty>"
    return " -> ".join(path)

"""
Construct log when type is classified as Unknown

Input
    contexnt stores the information of object corresponding current type
    reason explains why this type classified as Unknown
    tag indicates the TypeTag classified as Unknown
    die indicates the DIE classified its type as Unknown
    path tracks history for traversing recursive TypeTag

Output
    Log
"""
def unknown_type_log(context, reason, tag=None, die=None, path=None):
    # If no object information, do not add log
    if context is None:
        return []

    return [
        f"Function: {context['FunctionName']} @ {context['FunctionAddress']}",
        f"Target: {context['Target']}",
        f"Reason: {reason}",
        f"Tag: {tag if tag is not None else '<none>'}",
        f"Type path: {type_path_to_text(path)}",
        f"DIE: {die_location(die)}",
        "",
    ]

"""
Given type DIE, resolve its type
Input
   seen prevents cycle when the type is defined using itself
   context stores the object corresponding current type for logging
   path tracks history for traversing recursive TypeTag

Output
    Classified type
    Addition log indicating log when type is classified as Unknown
"""
def resolve_type(die, seen=None, context=None, path=None):
    # Type DIE not exist -> Mark as Unknown
    if die is None:
        return UNKNOWN, unknown_type_log(
            context,
            "Missing DW_AT_type reference",
            path=path,
        )

    # If resolve_type is first call, initialize seen and path
    if seen is None:
        seen = set()

    if path is None:
        path = []

    # If type is defined by the discovered type, assume can not determine its
    # type. In this cycle case, Mark as Unknown
    die_key = (die.cu.cu_offset, die.offset)
    tag = die.tag
    next_path = path + [tag]

    if die_key in seen:
        return UNKNOWN, unknown_type_log(
            context,
            "Recursive type resolution",
            tag=tag,
            die=die,
            path=next_path,
        )

    seen.add(die_key)

    # Classify Type
    if tag in ADDR_TAGS:
        return ADDRESS, []

    if tag in VALUE_TAGS:
        return VALUE, []

    if tag in RECURSIVE_TAGS:
        return resolve_type(
            get_ref_die(die, "DW_AT_type"),
            seen,
            context,
            next_path,
        )

    return UNKNOWN, unknown_type_log(
        context,
        "Unsupported DWARF type tag",
        tag=tag,
        die=die,
        path=next_path,
    )

# Extract type of return value
def return_types(die, function, address):
    # Type of return value of function is represented as type of itself.
    type_die = get_ref_die(die, "DW_AT_type")
    if type_die is None:
        return [], []

    # Context is used for logging when type is classified to Unknown
    context = {
        "FunctionName": function,
        "FunctionAddress": address,
        "Target": "Return Value",
    }

    resolved_type, log_lines = resolve_type(type_die, context=context)
    return [resolved_type], log_lines

# Extract type of parameters of given function DIE
def parameter_types(die, function, address):
    result = []
    logs = []
    index = 0
    for child in die.iter_children():
        # Child DIE of Subprogram DIE with DW_TAG_formal_parameter
        # represents the parameters of corresponding function
        if child.tag != "DW_TAG_formal_parameter":
            continue
        
        # DW_AT_artificial represents the object or types
        # that are not actually declared in the source code
        if attr_bool(child, "DW_AT_artificial"):
            continue

        # Extract parameter name for loggin
        param_name = attr_string(child, "DW_AT_name")
        if param_name:
            target = f"Argument {index} ({param_name})"
        else:
            target = f"Argument {index}"
        
        # Context is used for logging when type is classified to Unknown
        context = {
            "FunctionName": function,
            "FunctionAddress": address,
            "Target": target,
        }

        # Recursivly access DW_AT_type to extract concrete type
        resolved_type, log_lines = resolve_type(
            get_ref_die(child, "DW_AT_type"),
            context=context,
        )
        result.append(resolved_type)
        logs.extend(log_lines)
        index += 1
    return result, logs

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

# Check current DIE is function declaration or not
def is_function_declaration(die):
    return die.tag == "DW_TAG_subprogram" and attr_bool(die, "DW_AT_declaration")

# Extract function signature from DIE
# containing parameter and return value information
def extract_signature(die, name, address):
    args, arg_logs = parameter_types(die, name, address)
    returns, return_logs = return_types(die, name, address)

    return {
        "Name": name,
        "Args": args,
        "Return": returns,
    }, arg_logs + return_logs

def unknown_count(signature):
    return signature["Args"].count(UNKNOWN) + signature["Return"].count(UNKNOWN)

def choose_duplicate(signatures):
    return sorted(
        signatures,
        key=lambda item: (unknown_count(item), -len(item["Args"]), item["Name"]),
    )[0]

# Transform function signature to string
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

# Filter only unique signatures
def unique_signatures(signatures):
    seen = set()
    result = []

    for signature in signatures:
        key = (
            signature["Name"],
            tuple(signature["Args"]),
            tuple(signature["Return"]),
        )
        if key in seen:
            continue
        seen.add(key)
        result.append(signature)

    return result

def merge_return_types(definition, prototype):
    if not definition["Return"]:
        return prototype["Return"]
    if not prototype["Return"]:
        return definition["Return"]
    return merge_type_list(definition["Return"], prototype["Return"])

def merge_definition_with_prototype(definition, prototype):
    # Merge parameters
    if (not definition["Args"]) and prototype["Args"]:
        # Signature only exist from declaration
        args = prototype["Args"]
    elif len(definition["Args"]) == len(prototype["Args"]):
        # Merge only when same number of parameters
        args = merge_type_list(definition["Args"], prototype["Args"])
    else:
        return None

    # Merge return value
    returns = merge_return_types(definition, prototype)
    if args is None or returns is None:
        return None

    return {
        "Name": definition["Name"],
        "Args": args,
        "Return": returns,
    }

# Merge function signature from definition and declaration
def incorporate_prototypes(address, definition, prototypes):
    # Filter only unique signatures
    prototypes = unique_signatures(prototypes)
    
    # Signatures from only function defintion
    if not prototypes:
        return definition, []
    
    result = definition
    log_lines = [
        f"Function {definition['Name']} @ {address} has declaration signature candidates:",
        f"  Definition: {signature_to_text(definition)}",
    ]

    # Merge function signatures from function definition and declaration
    # Majority on function definition,
    # merging sigantures by iterating signatures from function declaration
    # Log only when at least one signature was rejected
    rejected = False
    for prototype in prototypes:
        log_lines.append(f"  Declaration: {signature_to_text(prototype)}")
        merged = merge_definition_with_prototype(result, prototype)
        if merged is None:
            rejected = True
            log_lines.append("  Decision: rejected declaration signature")
        else:
            result = merged
            log_lines.append("  Decision: incorporated declaration signature")

    log_lines.append(f"  Final: {signature_to_text(result)}")
    log_lines.append("")

    if rejected:
        return result, log_lines
    else:
        return result, log_lines

# Merge extracted signature among functions with same address
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

# Extract function signature from referenced function declaration
def parsing_refer_declaration(die, definition_name, address):
    signatures = []
    logs = []

    for attr_name in ["DW_AT_specification", "DW_AT_abstract_origin"]:
        ref_die = get_ref_die(die, attr_name)
        # Only parse referenced function DIE exists
        if ref_die is None or ref_die.tag != "DW_TAG_subprogram":
            continue
        
        # If exist, extract function signature
        names = function_names(ref_die)
        ref_name = names[0] if names else definition_name
        signature, signature_logs = extract_signature(ref_die, ref_name, address)
        signatures.append(signature)
        logs.extend(signature_logs)

    return signatures, logs

# Extract function signature from function declaration
def parsing_declaration(dwarf):
    prototypes = {}
    logs = []

    # Iterate all debug information of each Compile Unit
    for cu in dwarf.iter_CUs():
        for die in cu.iter_DIEs():
            # Only need the DIE of function declaration
            if not is_function_declaration(die):
                continue

            # Extract function name
            names = function_names(die)
            if not names:
                continue

            # Extract function signature from function declaration
            signature, signature_logs = extract_signature(
                die,
                names[0],
                "<declaration>",
            )
            logs.extend(signature_logs)

            # Merge all type signatures with same address
            for name in names:
                copied = signature.copy()
                copied["Name"] = name
                prototypes.setdefault(name, []).append(copied)

    return prototypes, logs

# Extract function signature from function definition
def parsing_definition(dwarf, prototypes, unknown_logs):
    by_addr = {}
    declaration_logs = []

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
            names = function_names(die)

            # Extract function signature
            signature, signature_logs = extract_signature(die, name, address)
            unknown_logs.extend(signature_logs)

            # Extract function signature from referenced function DIE
            ref_signatures, ref_logs = parsing_refer_declaration(
                die,
                name,
                address,
            )
            unknown_logs.extend(ref_logs)

            # Merge all signatures from
            # 1) function declaration and
            # 2) referenced function declaration
            prototype_signatures = ref_signatures[:]
            for candidate_name in names:
                prototype_signatures.extend(prototypes.get(candidate_name, []))

            # If there exist multiple functions with same address,
            # incorporates types
            incorporated_signature, incorporate_logs = incorporate_prototypes(
                address,
                signature,
                prototype_signatures,
            )
            declaration_logs.extend(incorporate_logs)

            # Insert the GR information of current function
            by_addr.setdefault(address, []).append(incorporated_signature)

    return by_addr, unknown_logs, declaration_logs

# Extract GT information from given GT binary
def extract(binary_path):
    with open(binary_path, "rb") as stream:
        elf = ELFFile(stream)
        
        # Check given binary is compiled with debugging option
        if not elf.has_dwarf_info():
            raise RuntimeError("binary has no DWARF debug information")

        # Get DWARFInfo context object
        dwarf = elf.get_dwarf_info()

        # 1. Extract type signature from function declaration
        prototypes, unknown_logs = parsing_declaration(dwarf)

        # 2. Extract type signature from function definition
        by_addr, unknown_logs, declaration_logs = parsing_definition(dwarf, prototypes, unknown_logs)

        if not by_addr:
            # Can not detect any functions
            raise RuntimeError(
                "No DWARF function definitions found. Compile the ground-truth binary with debug information."
            )
        
        # 3. Final merging to handle the multiple functions with same address
        # was found during parsing function definition
        db = {}
        duplicate_logs = []

        # Handle GT multiple function signature at same address
        for address, signatures in sorted(by_addr.items()):
            signature, log_lines = merge_duplicate_signatures(address, signatures)
            duplicate_logs.extend(log_lines)
            # Update log
            if log_lines:
                duplicate_logs.append("")
            # Update GT DB
            if signature is not None:
                db[address] = signature

        # Merge all logs
        logs = []
        if unknown_logs:
            logs.append("== Unknown Type Classification ==")
            logs.extend(unknown_logs)

        if declaration_logs:
            logs.append("== Declaration Signature Incorporation ==")
            logs.extend(declaration_logs)

        if duplicate_logs:
            logs.append("== Duplicate Address Signature Merge ==")
            logs.extend(duplicate_logs)

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
