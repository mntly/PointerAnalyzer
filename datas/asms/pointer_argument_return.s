.section .data
.align 4
operPtr:
  .long 40

result:
  .long 0

.section .text
.intel_syntax noprefix
.global _start

# int addOffset(int *ptr, int offset)
.type addOffset, @function
addOffset:
  push ebp
  mov ebp, esp

  mov eax, [ebp+8]
  add eax, [ebp+12]

  pop ebp
  ret
.size addOffset, .-addOffset

# int compute(void)
.type compute, @function
compute:
  push ebp
  mov ebp, esp

  push 2
  push OFFSET FLAT:operPtr
  call addOffset
  add esp, 8

  add eax, 1
  mov DWORD PTR [result], eax

  leave
  ret
.size compute, .-compute

.type _start, @function
_start:
  call compute

  mov ebx, eax
  mov eax, 1
  int 0x80
.size _start, .-_start
