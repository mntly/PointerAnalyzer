# Type Inference

- [`ResolvedType.fs`](./ResolvedType.fs) is the last step of PointerAnalysis. It gets the result of Constraint Solving process, and produce Varaible Type map that represent the inferred type of each varaible.
- [`TypeConstraintSolver.fs`](./TypeConstraintSolver.fs) translates the analyzer's finite type constraints into Z3 fixed-point relations and returns inferred constraints and conflicts according to [Constraint Solving Rule](../../docs/DomainDefinition.pdf).