# PointerAnalyzer

## ToDo: Description of PointerAnalyzer

## Usage

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b <binary> \
    -o <outputdir> \
    [OPTIONS]
```

### Required Arguments

| Option | Description |
|--------|-------------|
| `-b`, `--binary <file>` | Binary file to analyze. |
| `-o`, `--output <dir>` | Directory to store analysis results. |

### Optional Arguments

| Option | Description |
|--------|-------------|
| `-d`, `--dumpssa` | Print/Store recovered B2R2 SSA. |
| `-dc`, `--dumpconstraints` | Print/Store the human-readable type constraints and type IDs. |
| `-lf`, `--listfunctions` | Print/Store recovered functions and exit before analysis. |
| `-s`, `--store <int>` | If `1`, store optional outputs such as SSA/function list/constraints in the output directory. If `0`, print optional outputs to stdout. The main `inferredTypes.json` result is always stored. |
| `-t`, `--tracktime` | Print the processing time of each analysis step. |
| `--function <name\|address>` | After analyzing the binary, print the result of only the selected function. |
| `--help` | Display help information. |

## Examples

### Analyze all functions

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output
```

This stores the main analysis result at:

```text
output/pointer_argument_return/inferredTypes.json
output/pointer_argument_return/analysisConfig.json
```

The default JSON result stores only inferred type names such as `Address`,
`Value`, `Conflict`, and `Unknown`; it does not include TypeIds.
`analysisConfig.json` stores the analyzed platform's word size for the
evaluator.

### Print recovered SSA

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -d
```

### Dump the recovered SSA

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -d \
    -s 1
```

### List recovered functions

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -lf
```

### Save the recovered funtions

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -lf \
    -s 1
```

### Track analysis time

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -t
```

### Dump type constraints

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/pointer_argument_return \
    -o output \
    -dc
```

### Print out one the result of specific function

* Below scripts both print out the inferred type result of `_dl_aux_init` function at 0x0804a80f.

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/helloword-x86_32-i586-uclibc-O0 \
    -o output \
    --function 0x0804a80f
```

```bash
dotnet run --project src/PointerAnalyzer.fsproj \
    -b datas/binaries/helloword-x86_32-i586-uclibc-O0 \
    -o output \
    --function _dl_aux_init
```
