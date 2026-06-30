# Domain

This directory contains the implementation related to Abstract Interpretation, Type Constraint, and Analysis State.

1. Abstract Interpretation
    - [`AbsMem.fs`](./AbsMem.fs): Mapping from memory-version/address to abstract value. This mapping only track the address with concrete integer.
    - [`AbsVal.fs`](./AbsVal.fs): Abstract values and its primitive operations.
    - [`RegMap.fs`](./RegMap.fs): Mapping from SSA variable to correspond abstract value.

2. Type Constraint
    - [`TypeConstraint.fs`](./TypeConstraint.fs): Define type constraints among Address/Value/Equality/Arithmetic(addition, subtraction)
    - [`TypeIdMap.fs`](./TypeIdMap.fs): Mapping from SSA variable to type IDs. (Type ID increases monotonly during overall analysis.)
    - [`TypeState.fs`](./TypeState.fs): Manage the type constraint and provide interface of Type Constraint Solving.

3. Analysis State
    - [`AnalysisState.fs`](./AnalysisState.fs): The state updated during evaluating the stmts.

- [`Signature.fs`](./Signature.fs): Common abstract-domain interfaces.
