# PointerAnalyzer.Domain

The implementation follows `docs/DomainDefinition.tex`.

- `AbsVal.fs`: `Const | Bot | Top`
- `RegMap.fs`: SSA variable to abstract value
- `AbsMem.fs`: `(memory version, address)` to `(value, type ID)`
- `TypeMap.fs`: SSA variable to type ID
- `TypeState.fs`: fresh type IDs and type constraints
- `TypeConstraintSolver.fs`: Z3 Fixedpoint relations, rules, and queries
- `AnalysisState.fs`: register, memory, and type state
