namespace PointerAnalyzer.Summary

open B2R2

/// Per-site syscall summary recovered from B2R2's syscall abstraction. The
/// platform syscall table supplies parameter and return types at apply time.
type SyscallSummary =
  { IsExit: bool
    AbstractionOutputs: Set<RegisterID> }

module SyscallSummary =
  let create isExit abstractionOutputs =
    { IsExit = isExit
      AbstractionOutputs = abstractionOutputs }
