# Frontend

This directory converts a binary file into B2R2 SSA CFG with DFA result for the analyzer.

- [`BinaryLoader.fs`](./BinaryLoader.fs) opens the file with B2R2, validates the supported ISA, and selects a calling convention.
- [`ConstantClassifier.fs`](./ConstantClassifier.fs) classifies constants into Value or Address using B2R2's mapped-address information. Unmapped constants remain Unknown.
- [`FunctionDFA.fs`](./FunctionDFA.fs) runs B2R2 SSA constant propagation and uses B2R2 SSA def-use edges to find variables used as pointer expressions.
- [`ProgramDFA.fs`](./ProgramDFA.fs) recovers functions and CFGs, lifts each function to SSA, and records call-site-to-callee relationships.
