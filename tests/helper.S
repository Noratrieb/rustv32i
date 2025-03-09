.macro START_TEST
    .section .text
    .globl _start
    _start:
.endm

.macro ASSERT_EQ actual expected
    li t6, \expected
    bne \actual, t6, fail
.endm

.macro PASS
    li a7, -1
    li a0, 1
    ecall
.endm

fail:
    li a7, -1
    li a0, 0
    ecall
