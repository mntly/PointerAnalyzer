# Code for evaluating PointerAnalyzer

## [FindCall0](./FindCall0)

This directory contains the codes to find out which instructions are lifted to [B2R2 SSA](https://b2r2.org/B2R2/reference/b2r2-binir-ssa.html) as `jmp 0`.

### [FindCall0.fs](./FindCall0/FindCall0.fs)

This file extracts the functions in given binary using `readelf` and in each function, check it contains `jmp 0` form SSA statement.

```
dotnet run --project Checker.fsproj \
  -m 0 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0
```

If option `-o` is indicated with directory path, it stores the result into given directory.

```
dotnet run --project Checker.fsproj \
  -m 0 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0 \
  -o output
```

### [FindCall0Invalid.fs](./FindCall0/FindCall0Invalid.fs)

Almost same as [FindCall0.fs](./FindCall0/FindCall0.fs). The main difference is this file checks the function resolved from B2R2. It extract function not only valid but aslo invalid from B2R2 function recovery logic.

```
dotnet run --project Checker.fsproj \
  -m 1 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0
```

If option `-o` is indicated with directory path, it stores the result into given directory.

```
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

```
dotnet run --project Checker.fsproj \
  -m 2 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0-gt \
  -o output
```

With `-on` option, you can assign the suffix name of output file. As default, it will be basename of given binary. The extracted ground truth is stroed with `SUFFIX_GT.json`. During extraction, if there happens some strange behavior, the extractor will logs as `SUFFIX_GTExtract.log`

```
dotnet run --project Checker.fsproj \
  -m 2 \
  -b ../datas/binaries/helloword-x86_32-i586-uclibc-O0-gt \
  -o output \
  -on testSimple
```