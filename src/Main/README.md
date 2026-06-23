# PointerAnalyzer Main

`Main.fs` is the current B2R2 frontend and executable entry point.

It:

1. Creates test bytes from `sampleAssembly`.
2. Recovers a B2R2 CFG.
3. Lifts the CFG to B2R2 SSA.
4. Runs B2R2 constant propagation.
5. Creates `StmtEvalConfig`.
6. Calls `AnalyzerDomain.analyze`.
7. Prints SSA, type constraints, and conflicts.

The analyzer itself does not assemble code or recover B2R2 structures. A
future binary-file frontend should replace the input-preparation portion of
`Main.fs` and pass the resulting SSA CFG and callbacks to the same
`AnalyzerDomain.analyze` function.

See [`../Analysis/README.md`](../Analysis/README.md) for the complete reading
order and call-flow explanation.

## Exact Starting Order

Start with this shorter path before reading every expression and statement
case:

1. `Main.fs` - `main`
2. `Main.fs` - `assemble`
3. `Main.fs` - `recoverSSA`
4. `Main.fs` - `constantValueFrom`
5. `Main.fs` - `pointerUseFrom`
6. `Main.fs` - `classifyConstant`
7. `Analysis/StmtEval.fs` - `StmtEvalConfig`
8. `Analysis/Analyzer.fs` - `AnalyzerDomain.analyze`
9. `Analysis/Analyzer.fs` - `AnalyzerModule.EvalTransferOnce`
10. `Analysis/StmtEval.fs` - `StmtEvalModule.Eval`
11. `Analysis/ExprEval.fs` - `ExprEvalModule.Eval`
12. `Domain/AnalysisState.fs` - state update helpers
13. `Domain/TypeState.fs` - `solve`
14. `TypeInference/TypeConstraintSolver.fs` - `solve`
15. `Main.fs` - result printing in `main`
