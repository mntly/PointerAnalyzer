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

The SSA of each function is stored at `output/<BINARY_NAME>_ssa`