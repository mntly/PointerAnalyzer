# Summary

This directory represents and applies modular analysis results of each functions.

- [`FunctionSummary.fs`](./FunctionSummary.fs) stores type IDs of parameters and return value, generated type constraints, and the next unused type ID.
- [`FunctionSummaryBuilder.fs`](./FunctionSummaryBuilder.fs) extracts architecture-specific parameters and return register from an `AnalysisResult`.
- [`SummaryApplicator.fs`](./SummaryApplicator.fs) connects type IDs of caller variables to callee parameter and return type IDs with `Same` constraints.
- [`TypePerInst.fs`](./TypePerInst.fs) stores types of each register per instruction. In current implementation, it is not used.