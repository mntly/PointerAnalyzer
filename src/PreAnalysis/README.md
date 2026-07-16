# PreAnalysis

This directory contains codes for PreAnalyzer. PreAnalyzer checks the leaf nodes
of given function CFG and Def-Use chain. It extracts live SSA Variables.

The main analyzer of PointerAnalyzer only create type constraints using the type
id of live SSA Variables and their computations.

- [`LivenessPropagator`](./LivenessPropagator.fs)

- [`PreAnalysisTypes`](./PreAnalysisTypes.fs): Defines the data structure to
track the live SSA Variables. It has the function for checking given variable is
Live or Dead.

- [`PreAnalyzer`](./PreAnalyzer.fs): The core implementation of PreAnalyzer.
It extracts sink live SSA Variables, and propagate the liveness by iterating
Bottom-Up.

- [`VariableCollector`](./VariableCollector.fs): Helper functions for extracting SSA Variables.

## [`Sink`](./Sink/)

This directory contains codes for extracting initial live registers.

- [`CallSiteSinkCollector`](./Sink/CallSiteSinkCollector.fs): This file
extracts live registers used as function arguments at each function call site.
The registers used as arguments are selected soundly, in other words, all
registers possible to used as arguments are selected.

- [`DefaultLiveCollector`](./Sink/DefaultLiveCollector.fs): This file extracts
live SSA variables handled as a default. Current, PreAnalyzer handles below SSA
variables as a default live SSA variable.
1. StackVar
2. SSA variables with trivial type, such as PC or FLAG registers, etc.

- [`LeafSinkCollector`](./Sink/LeafSinkCollector.fs): Thif file extracts live
registers of leaf node of given CFG.

- [`SinkCollector`](./SinkCollector.fs): The core implementation of extracting
initial live registers
