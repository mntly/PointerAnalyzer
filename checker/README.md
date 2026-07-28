# Code for evaluating PointerAnalyzer

## [FindCall0](./FindCall0)

This directory contains the codes to find out which instructions are lifted to [B2R2 SSA](https://b2r2.org/B2R2/reference/b2r2-binir-ssa.html) as `jmp 0`.

### [FindCall0.fs](./FindCall0/FindCall0.fs)

This file extracts the functions in given binary using `readelf` and in each function, check it contains `jmp 0` form SSA statement.

```sh
dotnet run --project Checker.fsproj \
  -m 0 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0
```

If option `-o` is indicated with directory path, it stores the result into given directory.

```sh
dotnet run --project Checker.fsproj \
  -m 0 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0 \
  -o output
```

### [FindCall0Invalid.fs](./FindCall0/FindCall0Invalid.fs)

Almost same as [FindCall0.fs](./FindCall0/FindCall0.fs). The main difference is this file checks the function resolved from B2R2. It extract function not only valid but aslo invalid from B2R2 function recovery logic.

```sh
dotnet run --project Checker.fsproj \
  -m 1 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0
```

If option `-o` is indicated with directory path, it stores the result into given directory.

```sh
dotnet run --project Checker.fsproj \
  -m 1 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0 \
  -o output
```

## [EvaluateAnalyzer](./EvaluateAnalyzer/)

This directory contains the codes related to evaluate [PointerAnalyzer](../README.md).

### [GroundTruthExtractor](./EvaluateAnalyzer/GroundTruthExtractor)

This directory contains the codes related to extract type of parameters and return value of each function in given library code.

The ground truth type signature is only appeared in binary compiled with debug option that includes DWARF information. If given binary does not have DWARF information, the [GroundTruthExtractor](./EvaluateAnalyzer/GroundTruthExtractor) may occur error.

```sh
dotnet run --project Checker.fsproj \
  -m 2 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0-gt \
  -o output
```

With `-on` option, you can assign the suffix name of output file. As default, it will be basename of given binary. The extracted ground truth is stroed with `SUFFIX_GT.json`. During extraction, if there happens some strange behavior, the extractor will logs as `SUFFIX_GTExtract.log`

Each ground-truth argument and return value contains its byte `Size` and
classified `Type`. `Type` is `Address`, `Value`, `Unknown`, or `Structure`.
Structures recursively contain their fields, byte offsets, sizes, and types.
A size of `0` means that DWARF did not provide enough size information and
requires manual correction before evaluation.

```sh
dotnet run --project Checker.fsproj \
  -m 2 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0-gt \
  -o output \
  -on testSimple
```

### [Evaluator](./EvaluateAnalyzer/Evaluator/)

This directory contains the codes related to evaluate the performance of PointerAnalyzer using given ground truth type(`-gt`) and inferred type(`-i`).
The evaluator reads `analysisConfig.json` from the directory containing the
given `inferredTypes.json`. PointerAnalyzer generates both files together.
Before evaluation, source-level GT is converted according to the platform ABI.
The converted artifact is stored as `SUFFIX_ConvertedGT.json`. See
[GroundTruthConverter](./EvaluateAnalyzer/Evaluator/GroundTruthConverter/) for
the conversion and partial-structure evaluation rules.

```sh
dotnet run --project Checker.fsproj \
  -m 3 \
  -gt output/testSimple_GT.json \
  -i ../output/helloword-x86_32-i586-uclibc-O0/inferredTypes.json \
  -o output
```

With `-on` option, you can assign the suffix name of output file. As default, it will be basename of given binary. The extracted ground truth is stroed with `SUFFIX_GT.json`. During extraction, if there happens some strange behavior, the extractor will logs as `SUFFIX_GTExtract.log`

```sh
dotnet run --project Checker.fsproj \
  -m 3 \
  -gt output/testSimple_GT.json \
  -i ../output/helloword-x86_32-i586-uclibc-O0/inferredTypes.json \
  -o output \
  -on testSimple
```

## [Return64Detection](./Return64Detection/)

This directory contains the x86-32 detector for functions that return a
64 bit value through `EDX:EAX`.

Use `-rr 0` to analyze each return leaf and its direct predecessors, or
`-rr 1` to analyze the entire function. Use `-rh 0` for the basic def-use
heuristic, or `-rh 1` to additionally require a caller that consumes `EDX`
before overwriting it.

```sh
dotnet run --project Checker.fsproj -- \
  -m 4 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0 \
  -rr 0 \
  -rh 1
```

Passing `-o output` stores the report as `Return64Result`; otherwise it is
printed to standard output.

### [Evaluator](./Return64Detection/Evaluator/)

This directory contains the evaluator of x86-32 detector for functions that
return a 64 bit value through `EDX:EAX`. To evaluate, GT Json file generate from
[GroundTruthExtractor](./EvaluateAnalyzer/GroundTruthExtractor) is needed.
The evaluator only evaluates the function with returns valid vlaue. If the
function does not exist in GT Json or its GT return value size is more than 8,
then corresponding function marked as Invalid.

```sh
dotnet run --project Checker.fsproj -- \
  -m 5 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0 \
  -gt output/testSimple_GT.json \
  -rr 0 \
  -rh 1 \
  -o output \
  -on testSimple
```

With above commmand, this evaluator stores `testSimple_Return64Result`: the 
result of Return64Detector, `testSimple_Return64EvalResult.json`: the evaluation
metric, and `testSimple_Return64EvalResult.log`: the detailed log of evaluation.
