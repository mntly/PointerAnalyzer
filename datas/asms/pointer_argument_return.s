.section .data
.align 4
source_value:
  .long 40

result_value:
  .long 0

.section .text
.global _start

# int load_and_add(int *ptr, int delta)
.type load_and_add, @function
load_and_add:
  mov 4(%esp), %eax
  add 8(%esp), %eax
  ret
.size load_and_add, .-load_and_add

# int compute_result(void)
.type compute_result, @function
compute_result:
  push %ebp
  mov %esp, %ebp

  # cdecl arguments are pushed from right to left.
  push $2
  push $source_value
  call load_and_add
  add $8, %esp

  # Use the callee return value, then store the result.
  add $1, %eax
  mov %eax, result_value

  leave
  ret
.size compute_result, .-compute_result

.type _start, @function
_start:
  call compute_result

  mov %eax, %ebx
  mov $1, %eax
  int $0x80
.size _start, .-_start
