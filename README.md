# PointerAnalyzer
Build:
```bash
dotnet build
```

Run it with:

```bash
dotnet run --project src/PointerAnalyzer.fsproj -- /PATH/TO/BINARY
```

Example:
```bash
dotnet run --project src/PointerAnalyzer.fsproj -- datas/binaries/pointer_argument_return
```

The analysis result containing type constraints per each functions will be printed out.

---

To also store the recovered B2R2 SSA:

```bash
dotnet run --project src/PointerAnalyzer.fsproj -- /PATH/TO/BINARY --dump-ssa
```

Example:
```bash
dotnet run --project src/PointerAnalyzer.fsproj -- datas/binaries/pointer_argument_return --dump-ssa
```

The SSA of each function is stored at `output/<BINARY_NAME>_ssa`.

List recovered functions without running analysis:

```bash
dotnet run --project src/PointerAnalyzer.fsproj -- datas/binaries/helloword-x86_32-i586-uclibc-O0 --list-functions
```

Example:
```bash
dotnet run --project src/PointerAnalyzer.fsproj -- datas/binaries/helloword-x86_32-i586-uclibc-O0 --list-functions
```

Analyze all functions, but print only one selected function:

```bash
dotnet run --project src/PointerAnalyzer.fsproj -- /PATH/TO/BINARY --function 0x8049000
dotnet run --project src/PointerAnalyzer.fsproj -- /PATH/TO/BINARY --function function_name
```

Dump SSA only for one selected function:

```bash
dotnet run --project src/PointerAnalyzer.fsproj -- /PATH/TO/BINARY --dump-ssa --function function_name
```
