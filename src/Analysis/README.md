# PointerAnalyzer Analysis

This directory evaluates B2R2 SSA statements and produces type constraints.
`Main.fs` prepares B2R2 data. The files in this directory consume that data.

## Pipeline

```text
Assembly or binary
    |
    | Main.fs: create BinHandle and recover CFG
    v
B2R2 SSA CFG
    |
    | Main.fs: run B2R2 constant propagation and prepare StmtEvalConfig
    v
SSA CFG + configuration
    |
    | AnalyzerDomain.analyze
    v
StmtEvalModule.Eval
    |
    | for expressions
    v
ExprEvalModule.Eval
    |
    | updates
    v
AnalysisState
    |
    | TypeState.solve
    v
TypeConstraints + TypeConflicts
```

## Recommended Reading Order

For a strict function-by-function reading path, use this order:

1. `Main/Main.fs` - `main`
2. `Main/Main.fs` - `assemble`
3. `Main/Main.fs` - `recoverSSA`
4. `Main/Main.fs` - `constantValueFrom`
5. `Main/Main.fs` - `pointerUseFrom`
6. `Main/Main.fs` - `pointerVariablesInExpr`
7. `Main/Main.fs` - `variablesInExpr`
8. `Main/Main.fs` - `classifyConstant`
9. `Analysis/StmtEval.fs` - `StmtEvalConfig`
10. `Analysis/Analyzer.fs` - `AnalyzerDomain.analyze`
11. `Analysis/Analyzer.fs` - `AnalyzerDomain.createWithConfig`
12. `Analysis/Analyzer.fs` - `AnalyzerModule`
13. `Analysis/Analyzer.fs` - `AnalyzerModule.EvalTransferOnce`
14. `Analysis/StmtEval.fs` - `StmtEvalModule.Eval`
15. `Analysis/StmtEval.fs` - `evalDefinition`
16. `Analysis/StmtEval.fs` - `write`
17. `Analysis/StmtEval.fs` - `evalPhi`
18. `Analysis/StmtEval.fs` - `phiSource`
19. `Analysis/StmtEval.fs` - `evalMemoryDefinition`
20. `Analysis/ExprEval.fs` - `ExprEvalModule.Eval`
21. `Analysis/ExprEval.fs` - `evalStore`
22. `Domain/AnalysisState.fs` - `getOrFreshTypeId` and `freshTypeId`
23. `Domain/AnalysisState.fs` - `setRegister`, `load`, and `store`
24. `Domain/AnalysisState.fs` - constraint helpers such as `addSame`
25. `Domain/TypeState.fs` - `getOrFresh`, `fresh`, and `addConstraint`
26. `Domain/AbsVal.fs` - `binOp`, `relOp`, `join`, and `cast`
27. `Domain/RegMap.fs` - `find`, `add`, and `join`
28. `Domain/AbsMem.fs` - `location`, `updateVersion`, `add`, and `join`
29. `Analysis/Analyzer.fs` - return to `AnalyzerDomain.analyze`
30. `Domain/TypeState.fs` - `solve`
31. `TypeInference/TypeConstraintSolver.fs` - `solve`
32. `Main/Main.fs` - return to `main` and inspect result printing

Do not read each file completely during the first pass. Follow the calls above
and return to the caller after understanding the invoked function.

### 1. `Main/Main.fs`: prepare the analysis input

Start at `main`.

Read its calls in this order:

1. `assemble`
   - Converts `sampleAssembly` into x86 bytes with `B2R2.Assembly.Assembler`.
   - This is only test-input preparation. A future binary-file frontend can
     replace it without changing the analyzer.

2. `recoverSSA`
   - Creates a B2R2 `BinHandle`.
   - Recovers the CFG with `BinaryBrew`.
   - Converts the LowUIR CFG into an SSA CFG with `SSALifterFactory`.
   - Extracts `(ProgramPoint * Stmt)` pairs from SSA basic blocks.

3. `constantValueFrom`
   - Runs B2R2 `SSAConstantPropagation`.
   - Returns a callback of type `Variable -> BitVector option`.
   - `StmtEval` uses this callback when a defined SSA variable has a known
     constant value.

4. `pointerUseFrom`
   - Searches SSA expressions for variables used as memory or jump addresses.
   - Returns a callback of type `Variable -> bool`.
   - `StmtEval` uses this as an address-type hint for newly defined variables.

5. `classifyConstant`
   - Classifies a numeric constant as an address, value, or unknown constant.
   - Constants inside the current code range are treated as addresses.

6. `AnalyzerDomain.analyze`
   - Passes the prepared SSA statements and callbacks into the analyzer.
   - Returns solved type constraints and conflicts.

The B2R2-specific construction belongs in `Main.fs` because a later frontend
may obtain the same SSA CFG and data-flow results from a binary file.

### 2. `Analysis/Analyzer.fs`: coordinate statement evaluation

Read these definitions:

1. `AnalysisResult`
   - `FinalState`: complete register, memory, and type state.
   - `TypeConstraints`: original and Z3-derived constraints.
   - `TypeConflicts`: type IDs inferred as both address and value.

2. `AnalyzerModule`
   - Creates the initial `AnalysisState`.
   - Owns a configured `StmtEvalModule`.

3. `EvalStmt`
   - Evaluates one SSA statement.

4. `EvalTransferOnce`
   - Starts with the SSA CFG root blocks.
   - Keeps a set of visited basic-block IDs.
   - Evaluates statements until their transfer result changes control flow.
   - Resolves `Next`, `LabelTarget`, and `InterTarget` to CFG successors.
   - Evaluates every reachable block at most once.
   - Keeps register and memory state branch-local.
   - Threads the latest `TypeState` through branches so fresh IDs remain
     globally unique.
   - Joins branch register and memory states after both branches finish.

5. `AnalyzerDomain.analyze`
   - Creates the analyzer.
   - Evaluates the supplied SSA CFG with a worklist.
   - Calls `TypeState.solve`.
   - Builds the public `AnalysisResult`.

`EvalTransferOnce` follows `TransferResult.Target`, matching the statement
transfer judgment in `docs/DomainDefinition.tex`. Unreachable blocks are not
visited, and back edges do not cause loop headers to be evaluated again. Type
propagation reaches its fixed point later in `TypeState.solve`; abstract values
and memory are intentionally calculated only once.

### 3. `Analysis/StmtEval.fs`: evaluate SSA statements

First read the configuration types:

```fsharp
type StmtEvalConfig =
  { PointerUse: Variable -> bool
    ConstValue: Variable -> BitVector option
    ClassifyConstant: BitVector -> ConstantType }
```

Then read `StmtEvalModule.Eval`, which dispatches on the B2R2 statement:

- `Def(variable, expr)`
  - Calls `evalDefinition`.
  - Evaluates the right-hand expression or uses B2R2's known constant.
  - Writes the resulting `AbsVal` and `TypeId` to the state.

- `Def(memory, Store(...))`
  - Calls `evalMemoryDefinition`, then `ExprEvalModule.EvalStore`.

- `Phi(variable, sourceIds)`
  - Calls `evalPhi`.
  - Joins source abstract values.
  - Adds `Same(destinationTypeId, sourceTypeIds...)`.

- `Jmp`
  - Evaluates conditions and targets.
  - Marks conditions as values and indirect targets as addresses.
  - Returns a `TransferResult` describing the target.

- `ExternalCall`
  - Currently only marks the callee expression as an address.

Useful private helpers:

- `write`: stores the value and type ID of a defined SSA variable.
- `applyPointerHint`: adds `Address(typeId)` when `PointerUse` is true.
- `phiSource`: recovers one PHI input value and type ID.

### 4. `Analysis/ExprEval.fs`: recursively evaluate expressions

The central function is:

```fsharp
Eval:
  AnalysisState ->
  Expr ->
  AbsVal * TypeId option * AnalysisState
```

Each result contains:

- the computed abstract value;
- the type ID representing the expression, when one exists;
- the updated analysis state.

Read the expression cases in this order:

1. `Num`
   - Allocates a fresh type ID.
   - Calls `ClassifyConstant`.
   - Adds `Address(typeId)` or `Value(typeId)`.

2. `Var`
   - Gets the stable type ID assigned to the SSA variable.
   - Reads its abstract value from `RegMap`.

3. `BinOp`
   - Recursively evaluates both operands.
   - Computes the abstract value.
   - For `ADD`, adds `AddResult(result, left, right)`.
   - For `SUB`, adds `SubResult(result, left, right)`.

4. `Load`
   - Evaluates and marks the address expression as an address.
   - Reads the value and type ID from abstract memory.

5. `Store` / `EvalStore`
   - Evaluates the address and stored value.
   - Marks the address expression as an address.
   - Updates the destination memory version.

6. `Cast`, `Extract`, `RelOp`, and `UnOp`
   - Compute abstract values.
   - Currently classify their results as values.

7. `Ite` and `ExprList`
   - Evaluate multiple expressions.
   - Join abstract values.
   - Relate their type IDs with `Same`.

## Example Call Flow

For this SSA statement:

```text
EAX_3 := EAX_2 + 1
```

the call flow is:

```text
AnalyzerModule.EvalTransferOnce
  -> StmtEvalModule.Eval
  -> evalDefinition EAX_3 (EAX_2 + 1)
  -> ExprEvalModule.Eval BinOp(ADD, EAX_2, 1)
     -> Eval Var(EAX_2)
     -> Eval Num(1)
     -> AbsVal.binOp ADD
     -> fresh result TypeId
     -> add AddResult(resultId, eax2Id, oneId)
  -> write EAX_3 resultValue resultId
  -> update RegMap and TypeIndicatorMap
```

After every statement has been processed:

```text
AnalyzerDomain.analyze
  -> TypeState.solve
  -> Z3 Fixedpoint propagation
  -> AnalysisResult.TypeConstraints
  -> AnalysisResult.TypeConflicts
```

## Related Domain Files

After understanding the analysis call flow, read these files:

1. `Domain/AnalysisState.fs`: operations exposed to expression and statement evaluation.
2. `Domain/AbsVal.fs`: abstract constant domain and arithmetic.
3. `Domain/RegMap.fs`: SSA variable to abstract value.
4. `Domain/AbsMem.fs`: abstract memory indexed by memory version and address.
5. `Domain/TypeState.fs`: stable variable IDs, fresh expression IDs, and constraints.
6. `Domain/TypeConstraint.fs`: constraint definitions.
7. `TypeInference/TypeConstraintSolver.fs`: Z3 Fixedpoint rules and queries.

## Short Mental Model

Use this division while reading or extending the analyzer:

```text
Main.fs       = obtain and prepare B2R2 analysis inputs
Analyzer.fs   = run the analysis and return its result
StmtEval.fs   = define SSA statement semantics
ExprEval.fs   = define recursive SSA expression semantics
Domain/*      = store and manipulate abstract state
TypeInference = solve collected type constraints
```
