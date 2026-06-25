# Architecture

This directory contains the architecture/calling convention specific information such as parameter registers.

- [`x86_32`](./x86_32/) handles x86_32 specific calling conventions

---

- [`CallingConvention.fs`](./CallingConvention.fs) provides common API to
    1) Identify parameters/arguments
    2) Return Registers.

    In current implementation, it has full implementation only for x86-32