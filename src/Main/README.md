# Main

```text
binary path
-> BinaryLoader.load
-> ProgramRecovery.recover
-> optionally print recovered functions and exit
-> ModularAnalyzer.analyze
-> print JSON inferred types for all functions, or only the function selected by --function
-> optionally print human-readable constraints when --dumpconstraints is set
```
