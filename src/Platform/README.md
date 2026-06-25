# Platform

This directory contains binary-platform specific information. A platform combines
CPU-level facts and heuristics with ABI/calling-convention rules.

Examples of platform facts:

- word size
- architecture heuristics such as `IsAndMask`
- stack pointer and return registers
- parameter, argument, and return-value mapping

---

- [`ELF/x86_32`](./ELF/x86_32/)
