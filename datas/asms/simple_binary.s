.section .data
.align 4
value:
  .long 42

.section .text
.global _start
.type load_value, @function
load_value:
  mov 4(%esp), %eax
  mov (%eax), %eax
  ret
.size load_value, .-load_value

.type _start, @function
_start:
  push $value
  call load_value
  add $4, %esp

  mov %eax, %ebx
  mov $1, %eax
  int $0x80
.size _start, .-_start
