#!/usr/bin/env python3
"""
Small Tree-sitter-C example for extracting textual function signatures.

This is only an experiment script.  It does not resolve typedefs, macros, or
preprocessor configuration.  It extracts the type text that appears in the C
source.

Usage:
  python3 checker/tree_sitter_signature_example.py
  python3 checker/tree_sitter_signature_example.py /path/to/source.c

Dependencies:
  pip install tree-sitter tree-sitter-c
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import re
import sys


DEFAULT_UCLIBC_ROOT = Path(
    "/mnt/c/MyProject/SoftSec/vSim/Datas/uClibc-ng-1.0.57/uClibc-ng-1.0.57"
)

DEFAULT_SOURCE = DEFAULT_UCLIBC_ROOT / "libc/stdlib/__uc_malloc.c"


@dataclass(frozen=True)
class ParameterSignature:
    name: str
    ctype: str


@dataclass(frozen=True)
class FunctionSignature:
    name: str
    return_type: str
    parameters: list[ParameterSignature]
    source: str


def load_c_language():
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

    raw_language = tree_sitter_c.language()

    try:
        language = Language(raw_language)
    except TypeError:
        language = raw_language

    parser = Parser()

    if hasattr(parser, "set_language"):
        parser.set_language(language)
    else:
        parser.language = language

    return parser


def node_text(source: bytes, node) -> str:
    return source[node.start_byte : node.end_byte].decode("utf-8", errors="replace")


def normalize_space(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()


def walk(node):
    yield node
    for child in node.children:
        yield from walk(child)


def first_descendant(node, node_type: str):
    for candidate in walk(node):
        if candidate.type == node_type:
            return candidate
    return None


def direct_child(node, node_type: str):
    for child in node.children:
        if child.type == node_type:
            return child
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
        return normalize_space(param_text)

    escaped = re.escape(name)

    # Simple case: "int argc" -> "int".
    text = re.sub(rf"\b{escaped}\b\s*$", "", param_text).strip()

    # Function pointer case: "int (*main)(int)" -> "int (*)(int)".
    text = re.sub(rf"\(\s*\*\s*{escaped}\s*\)", "(*)", text)

    return normalize_space(text)


def parse_parameter(source: bytes, param_node, index: int) -> ParameterSignature:
    text = normalize_space(node_text(source, param_node))
    declarator = param_node.child_by_field_name("declarator")
    name = identifier_text(source, declarator) or f"arg{index}"
    ctype = remove_parameter_name(text, name)

    return ParameterSignature(name=name, ctype=ctype)


def parse_parameters(source: bytes, function_declarator) -> list[ParameterSignature]:
    parameter_list = function_declarator.child_by_field_name("parameters")
    if parameter_list is None:
        parameter_list = direct_child(function_declarator, "parameter_list")

    if parameter_list is None:
        return []

    params = []

    for child in parameter_list.children:
        if child.type != "parameter_declaration":
            continue

        text = normalize_space(node_text(source, child))
        if text == "void":
            continue

        params.append(parse_parameter(source, child, len(params)))

    return params


def return_type_text(source: bytes, owner_node, function_declarator) -> str:
    raw = source[owner_node.start_byte : function_declarator.start_byte].decode(
        "utf-8", errors="replace"
    )

    raw = raw.strip()

    # A declaration may include a leading extern/static and newlines.
    return normalize_space(raw)


def parse_signature(source: bytes, source_path: Path, owner_node, function_declarator):
    name = function_name(source, function_declarator)
    if name is None:
        return None

    return FunctionSignature(
        name=name,
        return_type=return_type_text(source, owner_node, function_declarator),
        parameters=parse_parameters(source, function_declarator),
        source=str(source_path),
    )


def extract_signatures(source_path: Path) -> list[FunctionSignature]:
    parser = load_c_language()
    source = source_path.read_bytes()
    tree = parser.parse(source)

    signatures = []

    for node in walk(tree.root_node):
        if node.type == "function_definition":
            function_declarator = first_descendant(node, "function_declarator")
            if function_declarator is None:
                continue

            signature = parse_signature(
                source, source_path, node, function_declarator
            )

            if signature is not None:
                signatures.append(signature)

        elif node.type == "declaration":
            for function_declarator in [
                child for child in walk(node) if child.type == "function_declarator"
            ]:
                signature = parse_signature(
                    source, source_path, node, function_declarator
                )

                if signature is not None:
                    signatures.append(signature)

    return signatures


def format_signature(signature: FunctionSignature) -> str:
    args = ", ".join(param.ctype for param in signature.parameters)
    if not args:
        args = "void"

    return f"{signature.name}: ({args}) -> {signature.return_type}"


def main() -> int:
    source_path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_SOURCE

    if not source_path.exists():
        print(f"source file does not exist: {source_path}", file=sys.stderr)
        return 1

    signatures = extract_signatures(source_path)

    print(f"Source: {source_path}")
    print(f"Signatures: {len(signatures)}")
    print()

    for signature in signatures:
        print(format_signature(signature))
        for param in signature.parameters:
            print(f"  arg {param.name}: {param.ctype}")
        print(f"  return: {signature.return_type}")
        print()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
