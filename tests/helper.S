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

.macro FAIL
    j fail
.endm

.macro WITH_SINGLE_TEST_NUMBERS macro
    \macro a, 0
    \macro c, 1
    \macro d, 2
    \macro u, 3
    \macro e, 4
    \macro v, 5
    \macro f, 8
    \macro t, 10
    \macro g, 16
    \macro h, 32
    \macro i, 64
    \macro s, 100
    \macro j, 128
    \macro k, 256
    \macro l, 512
    \macro w, 1000
    \macro m, 1024
    \macro n, 2047
    \macro b, -1
    \macro o, -2
    \macro p, -16
    \macro q, -1024
    \macro r, -1000

.endm

.macro WITH_TWO_TEST_NUMBERS macro
    .macro \macro\()_TMP namea:req a:req
        .macro \macro\()_TMP_\namea nameb:req b:req
            \macro \a, \b
        .endm
        
        WITH_SINGLE_TEST_NUMBERS \macro\()_TMP_\namea
    .endm

    WITH_SINGLE_TEST_NUMBERS \macro\()_TMP
.endm


fail:
    li a7, -1
    li a0, 0
    ecall
