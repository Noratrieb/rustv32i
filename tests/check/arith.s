.section .text
.globl _start
_start:
    li t0, 10
    li t1, 20
    add t2, t0, t1
    li t3, 30
    bne t2, t3, fail

    li t0, 10
    li t1, -2
    add t2, t0, t1
    li t3, 8
    bne t2, t3, fail

    li a7, -1
    li a0, 1
    ecall

fail:
    li a7, -1
    li a0, 0
    ecall
