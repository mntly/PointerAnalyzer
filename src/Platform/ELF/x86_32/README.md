# ELF x86-32 Platform

This platform profile models ELF x86-32 binaries with a cdecl-style ABI.

- word size: 4 bytes
- AND mask heuristic: `value >= 0xffff0000` (ToDo! Define AND mask, apply it to evaluation)
- stack pointer: `ESP`
- return register: `EAX`
- register arguments: none
- stack arguments: `StackVar(-4)`, `StackVar(-8)`, ...
