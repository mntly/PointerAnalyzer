# Type Inference

[`TypeConstraintSolver.fs`](./TypeConstraintSolver.fs) translates the analyzer's finite type constraints into Z3 fixed-point relations and returns inferred constraints and conflicts according to [Constraint Solving Rule](../../docs/DomainDefinition.pdf).