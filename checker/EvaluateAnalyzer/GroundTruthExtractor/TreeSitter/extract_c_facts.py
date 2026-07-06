from __future__ import annotations

from dataclasses import dataclass, asdict
from pathlib import Path
import argparse
import json
import re
import sys

# Used for detecting alias
TRUE_ALIAS_MACROS = {"weak_alias", "strong_alias"}

# Represent the type of parameters
@dataclass(frozen=True)
class ParsedParameter:
    name: str
    ctype: str

# Represent the result of extracting round truth per function
@dataclass(frozen=True)
class ParsedSignature:
    name: str
    source: str
    prototype: str
    returnCType: str
    parameters: list[ParsedParameter]

# Represent the alias relationship
@dataclass(frozen=True)
class ParsedAlias:
    alias: str
    canonicalName: str

# Load tree-sitter C parser
def loadParser():
    # Check tree-sitter is installed
    try:
        from tree_sitter import Language, Parser
        import tree_sitter_c
    except ModuleNotFoundError as ex:
        missing = ex.name
        raise SystemExit(
            f"Missing Python package: {missing}\n"
            "Install dependencies with:\n"
            "  pip install tree-sitter tree-sitter-c"
        )

    # Set language as C
    rawLanguage = tree_sitter_c.language()

    try:
        language = Language(rawLanguage)
    except TypeError:
        language = rawLanguage

    parser = Parser()

    if hasattr(parser, "set_language"):
        parser.set_language(language)
    else:
        parser.language = language

    return parser

# Change continuos blanks to single blank
def normalizeSpace(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()


def node_text(source: bytes, node) -> str:
    return source[node.start_byte : node.end_byte].decode("utf-8", errors="replace")


def relativeSource(root: Path, sourcePath: Path) -> str:
    try:
        return sourcePath.resolve().relative_to(root.resolve()).as_posix()
    except ValueError:
        return sourcePath.as_posix()


def walk(node):
    yield node
    for child in node.children:
        yield from walk(child)


def direct_child(node, node_type: str):
    for child in node.children:
        if child.type == node_type:
            return child
    return None


def first_descendant(node, node_type: str):
    for candidate in walk(node):
        if candidate.type == node_type:
            return candidate
    return None


def identifier_text(source: bytes, node) -> str | None:
    if node is None:
        return None

    if node.type == "identifier":
        return node_text(source, node)

    for child in node.children:
        found = identifier_text(source, child)
        if found is not None:
            return found

    return None


def function_name(source: bytes, function_declarator) -> str | None:
    declarator = function_declarator.child_by_field_name("declarator")
    return identifier_text(source, declarator)


def remove_parameter_name(param_text: str, name: str) -> str:
    if not name:
        return normalizeSpace(param_text)

    escaped = re.escape(name)
    text = re.sub(rf"\b{escaped}\b\s*$", "", param_text).strip()
    text = re.sub(rf"\(\s*\*\s*{escaped}\s*\)", "(*)", text)

    return normalizeSpace(text)


def parse_parameter(source: bytes, param_node, index: int) -> ParsedParameter:
    text = normalizeSpace(node_text(source, param_node))
    declarator = param_node.child_by_field_name("declarator")
    name = identifier_text(source, declarator) or f"arg{index}"
    ctype = remove_parameter_name(text, name)

    return ParsedParameter(name=name, ctype=ctype)


def parse_parameters(source: bytes, function_declarator) -> list[ParsedParameter]:
    parameter_list = function_declarator.child_by_field_name("parameters")
    if parameter_list is None:
        parameter_list = direct_child(function_declarator, "parameter_list")

    if parameter_list is None:
        return []

    parameters = []

    for child in parameter_list.children:
        if child.type != "parameter_declaration":
            continue

        text = normalizeSpace(node_text(source, child))
        if text == "void":
            continue

        parameters.append(parse_parameter(source, child, len(parameters)))

    return parameters


def return_type_text(source: bytes, owner_node, function_declarator) -> str:
    raw = source[owner_node.start_byte : function_declarator.start_byte].decode(
        "utf-8", errors="replace"
    )

    return normalizeSpace(raw)


def is_nested_function_declarator(owner_node, function_declarator) -> bool:
    node = function_declarator.parent

    while node is not None and node != owner_node:
        if node.type in {"parameter_declaration", "field_declaration"}:
            return True

        if node.type == "function_declarator":
            return True

        node = node.parent

    return False


def parse_signature(
    source: bytes, relativePath: str, owner_node, function_declarator
) -> ParsedSignature | None:
    name = function_name(source, function_declarator)
    if name is None:
        return None

    return_type = return_type_text(source, owner_node, function_declarator)
    parameters = parse_parameters(source, function_declarator)

    prototype = "{} {}({})".format(
        return_type,
        name,
        ", ".join(
            f"{param.ctype} {param.name}" if param.name else param.ctype
            for param in parameters
        ),
    )

    return ParsedSignature(
        name=name,
        source=relativePath,
        prototype=prototype,
        returnCType=return_type,
        parameters=parameters,
    )


def extractSignatures(source: bytes, relativePath: str, rootNode) -> list[ParsedSignature]:
    signatures = []

    for node in walk(rootNode):
        if node.type == "function_definition":
            function_declarator = first_descendant(node, "function_declarator")

            if function_declarator is None:
                continue

            signature = parse_signature(source, relativePath, node, function_declarator)

            if signature is not None:
                signatures.append(signature)

        elif node.type == "declaration":
            function_declarators = [
                candidate
                for candidate in walk(node)
                if candidate.type == "function_declarator"
                and not is_nested_function_declarator(node, candidate)
            ]

            for function_declarator in function_declarators:
                signature = parse_signature(source, relativePath, node, function_declarator)

                if signature is not None:
                    signatures.append(signature)

    return signatures


def argument_identifiers(source: bytes, argument_list) -> list[str]:
    identifiers = []

    for child in argument_list.children:
        if child.type == "identifier":
            identifiers.append(node_text(source, child))

    return identifiers


def extract_aliases(source: bytes, rootNode) -> list[ParsedAlias]:
    aliases = []

    for node in walk(rootNode):
        if node.type != "call_expression":
            continue

        callee = node.child_by_field_name("function")
        callee_name = identifier_text(source, callee)

        if callee_name not in TRUE_ALIAS_MACROS:
            continue

        arguments = node.child_by_field_name("arguments")
        if arguments is None:
            arguments = direct_child(node, "argument_list")

        if arguments is None:
            continue

        names = argument_identifiers(source, arguments)
        if len(names) >= 2:
            aliases.append(ParsedAlias(alias=names[1], canonicalName=names[0]))

    return aliases


def parseSource(root: Path, sourcePath: Path):
    parser = loadParser()
    source = sourcePath.read_bytes()
    tree = parser.parse(source)
    relativePath = relativeSource(root, sourcePath)

    signatures = extractSignatures(source, relativePath, tree.root_node)
    aliases = extract_aliases(source, tree.root_node)

    return {
        "signatures": [
            {
                "name": signature.name,
                "source": signature.source,
                "prototype": signature.prototype,
                "returnCType": signature.returnCType,
                "parameters": [asdict(param) for param in signature.parameters],
            }
            for signature in signatures
        ],
        "aliases": [asdict(alias) for alias in aliases],
    }


def main() -> int:
    # Set arguments to indicate which file should be parsed
    parser = argparse.ArgumentParser(
        description="Extract textual C signatures and aliases with Tree-sitter."
    )

    parser.add_argument("--root", required=True, help="Library source root.")
    parser.add_argument("--source", required=True, help="C source file to parse.")

    args = parser.parse_args()

    root = Path(args.root)
    sourcePath = Path(args.source)

    if not root.exists():
        print(f"source root does not exist: {root}", file=sys.stderr)
        return 1

    if not sourcePath.exists():
        print(f"source file does not exist: {sourcePath}", file=sys.stderr)
        return 1

    # Parse given source code and extract GT type information
    facts = parseSource(root, sourcePath)
    # Print the result as JSON format to preprocessing in F#
    print(json.dumps(facts, indent=None, sort_keys=True))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
