#!/usr/bin/env python3
#
# merge_pointer_metrics.py
#
# This file gets output file path for storing merged result and
# the path for result json of each file.
# This file merges per binary result metrics.

import argparse
import json
from pathlib import Path

# Construct metrics to track
BUCKETS = ("Correct", "MisInferred", "Conflict", "Fail")
BUCKET_FIELDS = ("Total", "GTAddress", "GTValue")
CONFUSION_FIELDS = ("TP", "TN", "FP", "FN")

def divide(numerator: int, denominator: int) -> float:
    return 0.0 if denominator == 0 else numerator / denominator

def empty_count() -> dict:
    return {
        "GTAll": 0,
        "All": 0,
        **{
            bucket: {field: 0 for field in BUCKET_FIELDS}
            for bucket in BUCKETS
        },
    }

# Check the value of corresponding key in mapping is int or not
def require_int(mapping: dict, key: str, source: Path) -> int:
    value = mapping.get(key)
    if not isinstance(value, int) or isinstance(value, bool):
        raise ValueError(f"{source}: {key} must be an integer")
    return value

# Merge all evalution results
def merge(inputs: list[Path]) -> dict:
    count = empty_count()
    confusion = {field: 0 for field in CONFUSION_FIELDS}

    for source in inputs:
        # Load result json for each binary
        with source.open("r", encoding="utf-8") as stream:
            document = json.load(stream)

        # Extract specific detail
        source_count = document.get("Count")
        # Extract final confusion metrics
        source_final = document.get("FinalResult")
        if not isinstance(source_count, dict) or not isinstance(source_final, dict):
            raise ValueError(f"{source}: missing Count or FinalResult object")
        
        # Update counts
        count["GTAll"] += require_int(source_count, "GTAll", source)
        count["All"] += require_int(source_count, "All", source)
        
        # Update specific metrics
        for bucket in BUCKETS:
            source_bucket = source_count.get(bucket)
            if not isinstance(source_bucket, dict):
                raise ValueError(f"{source}: missing Count.{bucket} object")
            for field in BUCKET_FIELDS:
                count[bucket][field] += require_int(source_bucket, field, source)

        # Update confusion metrics
        source_confusion = source_final.get("Confusion")
        if not isinstance(source_confusion, dict):
            raise ValueError(f"{source}: missing FinalResult.Confusion object")
        for field in CONFUSION_FIELDS:
            confusion[field] += require_int(source_confusion, field, source)

    # Calculate ratio for specific metrics
    ratio = {}
    for bucket in BUCKETS:
        values = count[bucket]
        ratio[bucket] = {
            "Total": divide(values["Total"], count["All"]),
            "GTAddress": divide(values["GTAddress"], values["Total"]),
            "GTValue": divide(values["GTValue"], values["Total"]),
        }

    # Calculate ratio for per GT metrics
    address_total = sum(count[bucket]["GTAddress"] for bucket in BUCKETS)
    value_total = sum(count[bucket]["GTValue"] for bucket in BUCKETS)

    gt_type_ratio = {
        "GTAddress": {
            bucket: divide(count[bucket]["GTAddress"], address_total)
            for bucket in BUCKETS
        },
        "GTValue": {
            bucket: divide(count[bucket]["GTValue"], value_total)
            for bucket in BUCKETS
        },
    }

    # Calculate confusion metrics
    tp = confusion["TP"]
    tn = confusion["TN"]
    fp = confusion["FP"]
    fn = confusion["FN"]
    final_total = tp + tn + fp + fn

    return {
        "Count": count,
        "Ratio": ratio,
        "GTTypeRatio": gt_type_ratio,
        "FinalResult": {
            "Confusion": confusion,
            "Acc": divide(tp + tn, final_total),
            "Recall": divide(tp, tp + fn),
            "Precision": divide(tp, tp + fp),
        },
    }

# Parse arguments and merge the results.
# The merged result is stored at given output file
def main() -> int:
    parser = argparse.ArgumentParser(
        description="Merge PointerAnalyzer evaluator metrics by summing counts."
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
