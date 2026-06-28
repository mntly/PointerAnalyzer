.section .data
.align 4
source_value:
  .long 40

result_value:
  .long 0

.section .text
.intel_syntax noprefix
.global _start

# int load_and_add(int *ptr, int delta)
.type load_and_add, @function
load_and_add:
  push ebp
  mov ebp, esp

  mov eax, [ebp+8]
  mov eax, [eax]
  add eax, [ebp+12]

  pop ebp
  ret
.size load_and_add, .-load_and_add

# int compute_result(void)
.type compute_result, @function
compute_result:
  push ebp
  mov ebp, esp

  # cdecl arguments are pushed from right to left.
  push 2
  push OFFSET FLAT:source_value
  call load_and_add
  add esp, 8

  # Use the callee return value, then store the result.
  add eax, 1
  mov DWORD PTR [result_value], eax

  leave
  ret
.size compute_result, .-compute_result

.type _start, @function
_start:
  call compute_result

  mov ebx, eax
  mov eax, 1
  int 0x80
.size _start, .-_start
