# x86-32

In this directory, x86-32 specific calling convention information will be stored.

In current implementation, the active x86-32 cdecl/fastcall policy currently lives in
[`CallingConvention.fs`](../CallingConvention.fs). (In the future, the x86-32 specific logic will be seperated to here.) With [`IntTypes.fs`](../../Core/IntTypes.fs), this tracks architecture/calling convention specific informations.