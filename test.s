
.globl main
.text
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw s0, 8(sp)
  addi s0, sp, 16
  li a0, 42
  j .L_ret_main
.L_ret_main:
  lw ra, 12(sp)
  lw s0, 8(sp)
  addi sp, sp, 16
  ret