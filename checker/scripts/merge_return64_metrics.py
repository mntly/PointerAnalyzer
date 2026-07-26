#!/usr/bin/env python3
#
# merge_return64_metrics.py
#
# This file gets output file path for storing merged result and
# the path for result json of each file.
# This file merges per binary result metrics.

import argparse
import json
from pathlib import Path

# Construct metrics to track
COUNT_FIELDS = (
    "GTAll",
    "Evaluated",
    "TP",
    "TN",
    "FP",
    "FN",
    "InvalidGT",
    "MissingGT",
)

def divide(numerator: int, denominator: int) -> float:
    return 0.0 if denominator == 0 else numerator / denominator

# Check the value of corresponding key in mapping is int or not
def require_int(mapping: dict, key: str, source: Path) -> int:
    value = mapping.get(key)
    if not isinstance(value, int) or isinstance(value, bool):
        raise ValueError(f"{source}: Count.{key} must be an integer")
    return value

# Merge all evalution results
def merge(inputs: list[Path]) -> dict:
    count = {field: 0 for field in COUNT_FIELDS}

    for source in inputs:
        # Load result json for each binary
        with source.open("r", encoding="utf-8") as stream:
            document = json.load(stream)

        # Extract metric per binary
        source_count = document.get("Count")
        if not isinstance(source_count, dict):
            raise ValueError(f"{source}: missing Count object")

        # Update metric
        for field in COUNT_FIELDS:
            count[field] += require_int(source_count, field, source)

    # Calculate confusion metrics
    tp = count["TP"]
    tn = count["TN"]
    fp = count["FP"]
    fn = count["FN"]
    evaluated = tp + tn + fp + fn
    precision = divide(tp, tp + fp)
    recall = divide(tp, tp + fn)
    f1 = (
        0.0
        if precision + recall == 0.0
        else 2.0 * precision * recall / (precision + recall)
    )

    return {
        "Count": count,
        "Metric": {
            "Accuracy": divide(tp + tn, evaluated),
            "Precision": precision,
            "Recall": recall,
            "F1": f1,
        },
    }

# Parse arguments and merge the results.
# The merged result is stored at given output file
def main() -> int:
    parser = argparse.ArgumentParser(
        description="Merge Return64 evaluator metrics by summing counts."
    )
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("inputs", nargs="+", type=Path)
    args = parser.parse_args()

    result = merge(args.inputs)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", encoding="utf-8") as stream:
        json.dump(result, stream, indent=2)
        stream.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
