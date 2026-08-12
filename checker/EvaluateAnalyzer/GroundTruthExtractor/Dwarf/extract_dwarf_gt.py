#!/usr/bin/env python3
import argparse
import json
import os
import sys

from readelf_dwarf_parser import run_readelf

# PointerAnalyzer's inferred type result
ADDRESS = "Address"
VALUE = "Value"
UNKNOWN = "Unknown"
STRUCTURE = "Structure"

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
]
STRUCTURE_TAGS = [
    "DW_TAG_structure_type",
    # "DW_TAG_union_type",
    # "DW_TAG_class_type",
]
RECURSIVE_TAGS = [
    "DW_TAG_typedef",
    "DW_TAG_const_type",
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

# Construct Ground Truth Type Element
def type_info(size, resolved_type, fields=None):
    result = {"Size": size, "Type": resolved_type}
    if resolved_type == STRUCTURE:
        result["Fields"] = fields if fields is not None else []
    return result

# Extract the offset of given fields in structure containing it
def member_offset(die):
    location = attr(die, "DW_AT_data_member_location")
    if location is None:
        return None

    try:
        return int(location.value)
    except (TypeError, ValueError):
        return None

# Construct the information of given field(context) for logging
def field_context(context, field_name):
    if context is None:
        return None

    return {
        "FunctionName": context["FunctionName"],
        "FunctionAddress": context["FunctionAddress"],
        "Target": f"{context['Target']} Field {field_name}",
    }

# Extract byte size of type of given DIE
def direct_byte_size(die):
    size_attr = attr(die, "DW_AT_byte_size")
    if size_attr is None:
        return 0
    try:
        return int(size_attr.value)
    except (TypeError, ValueError):
        return 0

# Extract byte size of address type of given DIE
# address_size is used only when DW_AT_byte_size fail
def address_byte_size(die):
    size = direct_byte_size(die)
    if size > 0:
        return size
    try:
        return int(die.cu["address_size"])
    except (KeyError, TypeError, ValueError):
        return 0

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
    Classified type and its byte size
    Addition log indicating log when type is classified as Unknown
"""
def resolve_type(die, seen=None, context=None, path=None):
    # Type DIE not exist -> Mark as Unknown
    if die is None:
        return type_info(0, UNKNOWN), unknown_type_log(
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
        return type_info(direct_byte_size(die), UNKNOWN), unknown_type_log(
            context,
            "Recursive type resolution",
            tag=tag,
            die=die,
            path=next_path,
        )

    seen.add(die_key)

    # Classify Type
    if tag in STRUCTURE_TAGS:
        fields = []
        logs = []

        for index, child in enumerate(die.iter_children()):
            if child.tag != "DW_TAG_member":
                # Field information is in DW_TAG_member
                continue

            name = attr_string(child, "DW_AT_name") or f"<field-{index}>"
            offset = member_offset(child)
            child_context = field_context(context, name)

            if offset is None:
                resolved = type_info(direct_byte_size(child), UNKNOWN)
                field_logs = unknown_type_log(
                    child_context,
                    "Missing or unsupported DW_AT_data_member_location",
                    tag=child.tag,
                    die=child,
                    path=next_path + [child.tag],
                )
                offset = -1
            else:
                resolved, field_logs = resolve_type(
                    get_ref_die(child, "DW_AT_type"),
                    seen.copy(),
                    child_context,
                    next_path,
                )

            field = {
                "Name": name,
                "Offset": offset,
                "Size": resolved["Size"],
                "Type": resolved["Type"],
            }
            # If the field of structure is structure, recursively merge
            if resolved["Type"] == STRUCTURE:
                field["Fields"] = resolved["Fields"]

            fields.append(field)
            logs.extend(field_logs)

        size = direct_byte_size(die)
        if size == 0:
            logs.extend(
                unknown_type_log(
                    context,
                    "Missing DW_AT_byte_size",
                    tag=tag,
                    die=die,
                    path=next_path,
                )
            )

        return type_info(size, STRUCTURE, fields), logs

    if tag in ADDR_TAGS:
        # Get byte size of type
        size = address_byte_size(die)
        logs = []
        if size == 0:
            logs = unknown_type_log(
                context,
                "Missing DW_AT_byte_size",
                tag=tag,
                die=die,
                path=next_path,
            )
        return type_info(size, ADDRESS), logs

    if tag in VALUE_TAGS:
        # Get byte size of type
        size = direct_byte_size(die)
        logs = []
        if size == 0:
            logs = unknown_type_log(
                context,
                "Missing DW_AT_byte_size",
                tag=tag,
                die=die,
                path=next_path,
            )
        return type_info(size, VALUE), logs

    if tag in RECURSIVE_TAGS:
        resolved, logs = resolve_type(
            get_ref_die(die, "DW_AT_type"),
            seen,
            context,
            next_path,
        )
        
        # If type of recursive tag is unknown,
        # Use size of current DIE as its type
        wrapper_size = direct_byte_size(die)
        if resolved["Size"] == 0 and wrapper_size > 0:
            resolved = type_info(wrapper_size, resolved["Type"])

        # Handling for Elf(Addr)
        type_name = attr_string(die, "DW_AT_name")
        if type_name is not None and "Addr" in type_name:
            return type_info(resolved["Size"], ADDRESS), logs

        return resolved, logs

    # Even fail to extract type, extract size of type
    size = direct_byte_size(die)
    if size == 0:
        referenced = get_ref_die(die, "DW_AT_type")
        if referenced is not None:
            referenced_type, _ = resolve_type(
                referenced,
                seen,
                context=None,
                path=next_path,
            )
            size = referenced_type["Size"]

    return type_info(size, UNKNOWN), unknown_type_log(
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
    def element_unknown_count(element):
        current = int(element["Type"] == UNKNOWN or element["Size"] == 0)
        return current + sum(
            element_unknown_count(field) for field in element.get("Fields", [])
        )

    return sum(
        element_unknown_count(element)
        for element in signature["Args"] + signature["Return"]
    )

def choose_duplicate(signatures):
    return sorted(
        signatures,
        key=lambda item: (unknown_count(item), -len(item["Args"]), item["Name"]),
    )[0]

# Transform function signature to string
def signature_to_text(signature):
    def element_to_text(element):
        if element["Type"] == STRUCTURE:
            fields = ", ".join(
                f'{field["Name"]}@{field["Offset"]}:{element_to_text(field)}'
                for field in element.get("Fields", [])
            )
            return f'Structure[{element["Size"]}B]{{{fields}}}'
        return f'{element["Type"]}[{element["Size"]}B]'

    args = ", ".join(element_to_text(element) for element in signature["Args"])
    returns = ", ".join(
        element_to_text(element) for element in signature["Return"]
    )
    return f'{signature["Name"]}: ({args}) -> ({returns})'

# Merge single type
def merge_type(left, right):
    left_type = left["Type"]
    right_type = right["Type"]

    if left_type == right_type:
        merged_type = left_type
    elif left_type == UNKNOWN:
        merged_type = right_type
    elif right_type == UNKNOWN:
        merged_type = left_type
    else:
        return None

    left_size = left["Size"]
    right_size = right["Size"]
    if left_size == right_size:
        merged_size = left_size
    elif left_size == 0:
        merged_size = right_size
    elif right_size == 0:
        merged_size = left_size
    else:
        return None

    if merged_type == STRUCTURE:
        if left_type == UNKNOWN:
            return type_info(merged_size, STRUCTURE, right.get("Fields", []))
        if right_type == UNKNOWN:
            return type_info(merged_size, STRUCTURE, left.get("Fields", []))

        left_fields = left.get("Fields", [])
        right_fields = right.get("Fields", [])
        if len(left_fields) != len(right_fields):
            return None

        merged_fields = []
        for left_field, right_field in zip(left_fields, right_fields):
            if (
                left_field["Name"] != right_field["Name"]
                or left_field["Offset"] != right_field["Offset"]
            ):
                return None

            merged_field = merge_type(left_field, right_field)
            if merged_field is None:
                return None

            merged_fields.append(
                {
                    "Name": left_field["Name"],
                    "Offset": left_field["Offset"],
                    **merged_field,
                }
            )

        return type_info(merged_size, STRUCTURE, merged_fields)

    return type_info(merged_size, merged_type)

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

    names = sorted(
        set(
            left.get("_Names", [left["Name"]])
            + right.get("_Names", [right["Name"]])
        )
    )
    return {
        "Name": names[0],
        "_Names": names,
        "Args": args,
        "Return": returns,
    }

# Filter only unique signatures
def unique_signatures(signatures):
    def element_key(element):
        return (
            element["Size"],
            element["Type"],
            tuple(
                (
                    field["Name"],
                    field["Offset"],
                    element_key(field),
                )
                for field in element.get("Fields", [])
            ),
        )

    seen = set()
    result = []

    for signature in signatures:
        key = (
            signature["Name"],
            tuple(element_key(item) for item in signature["Args"]),
            tuple(element_key(item) for item in signature["Return"]),
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
        return result, []

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
        "_Names": signatures[0].get("_Names", [signatures[0]["Name"]]),
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
        "_Names": merged["_Names"],
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
            incorporated_signature["_Names"] = sorted(
                set(names + [incorporated_signature["Name"]])
            )

            # Insert the GR information of current function
            by_addr.setdefault(address, []).append(incorporated_signature)

    return by_addr, unknown_logs, declaration_logs

# Use key as "Name", not "_Names"
def public_signature(signature):
    return {
        key: value
        for key, value in signature.items()
        if not key.startswith("_")
    }

# Filter the completed GT database using function names from the whitelist.
def apply_whitelist(db, whitelist):
    filtered = {}
    matched_names = set()

    for address, signature in db.items():
        names = set(signature.get("_Names", [signature["Name"]]))
        matches = names & whitelist
        if not matches:
            continue

        matched_names.update(matches)
        filtered[address] = signature

    logs = [
        "== Whitelist Filtering ==",
        f"Whitelist entries: {len(whitelist)}",
        f"Functions before filtering: {len(db)}",
        f"Functions after filtering: {len(filtered)}",
    ]

    unmatched_names = sorted(whitelist - matched_names)
    if unmatched_names:
        logs.append("Unmatched whitelist names:")
        logs.extend(f"  {name}" for name in unmatched_names)

    logs.append("")
    return filtered, logs

# Read one function name per line. Empty lines are ignored.
def load_whitelist(path):
    with open(path, "r", encoding="utf-8") as stream:
        return {line.strip() for line in stream if line.strip()}

# Extract GT information from given GT binary
def extract(binary_path, whitelist=None, readelf=None):
    # Parse the linked binary's DWARF information through GNU readelf.
    dwarf = run_readelf(
        binary_path,
        readelf or os.environ.get("READELF", "readelf"),
    )

    # 1. Extract type signature from function declaration
    prototypes, unknown_logs = parsing_declaration(dwarf)

    # 2. Extract type signature from function definition
    by_addr, unknown_logs, declaration_logs = parsing_definition(
        dwarf,
        prototypes,
        unknown_logs,
    )

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

    if whitelist is not None:
        db, whitelist_logs = apply_whitelist(db, whitelist)
        logs.extend(whitelist_logs)

    db = {
        # Filter only Functin Signature with key "Names" not "_Names"
        address: public_signature(signature)
        for address, signature in db.items()
    }
    return db, logs

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("binary_path", help="ground-truth binary with DWARF")
    parser.add_argument("--log", dest="log_path")
    parser.add_argument(
        "--whitelist",
        dest="whitelist_path",
        help="file containing one function name per line",
    )
    parser.add_argument(
        "--readelf",
        default=os.environ.get("READELF", "readelf"),
        help="readelf executable (default: READELF or readelf)",
    )
    args = parser.parse_args(argv[1:])

    # Check given file exists
    if not os.path.isfile(args.binary_path):
        print(
            f"ground-truth binary does not exist: {args.binary_path}",
            file=sys.stderr,
        )
        return 1

    if args.whitelist_path is not None and not os.path.isfile(
        args.whitelist_path
    ):
        print(
            f"function whitelist does not exist: {args.whitelist_path}",
            file=sys.stderr,
        )
        return 1

    # Extract ground truth
    try:
        whitelist = (
            load_whitelist(args.whitelist_path)
            if args.whitelist_path is not None
            else None
        )
        db, logs = extract(args.binary_path, whitelist, args.readelf)
    except Exception as ex:
        print(str(ex), file=sys.stderr)
        return 1

    # Store mismatched/unknown type issue occurs as log
    if args.log_path is not None:
        log_dir = os.path.dirname(args.log_path)
        if log_dir:
            os.makedirs(log_dir, exist_ok=True)
        with open(args.log_path, "w", encoding="utf-8") as stream:
            stream.write("\n".join(logs))
            if logs:
                stream.write("\n")

    # Propagate GT result to F# handler
    print(json.dumps(db, indent=2))
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
