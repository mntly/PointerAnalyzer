```bash
as --32 datas/asms/pointer_argument_return.s -o /tmp/pointer_argument_return.o
```

```bash
ld -m elf_i386 -e _start /tmp/pointer_argument_return.o -o datas/binaries/pointer_argument_return
```
