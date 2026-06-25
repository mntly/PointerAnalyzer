# Analysis

This directory contains the core implementation of modular analysis such as evaluating exprs and stmts.

- [`Analyzer.fs`](./Analyzer.fs) analysis one function by traversing its SSA CFG.
- [`StmtEval.fs`](./StmtEval.fs) evaluates SSA statements according to [Statment Transfer Semantics](../docs/DomainDefinition.pdf)
- [`ExprEval.fs`](./ExprEval.fs) recursively evaluates expressions and creates type constraints according to [Expr Evaluation semantics](../docs/DomainDefinition.pdf)