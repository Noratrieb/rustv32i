# Integer computational register-register instruction.

#include "../helper.S"

.macro CASER inst:req a:req b:req expected:req
    li t0, \a
    li t1, \b
    \inst t2, t0, t1
    ASSERT_EQ t2, \expected
.endm

.macro CASE_IMM inst:req a:req b:req expected:req
    li t0, \a
    \inst t2, t0, \b
    ASSERT_EQ t2, \expected
.endm

.macro CASE_BOTH inst:req insti:req a:req b:req expected:req
    CASER \inst, \a, \b, \expected
    CASE_IMM \insti, \a, \b, \expected
.endm

.macro CASE inst:req a:req b:req expected:req
    CASE_BOTH \inst, \inst\()i, \a, \b, \expected
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

START_TEST
    # Base instructions

    .macro CASE_ADD a:req, b:req
        CASE add, \a, \b, \a + \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_ADD

    CASE slt 10 20 1
    CASE slt 20 10 0
    CASE slt, -1 0 1
    CASE slt 0, -1 0
    CASE slt, -1, -1, 0
    CASE slt, -100, -1, 1

    CASE_BOTH sltu sltiu 10 20 1
    CASE_BOTH sltu sltiu 20 10 0
    CASE_BOTH sltu sltiu, -1, 0, 0
    CASE_BOTH sltu sltiu, -100, -1, 1
    CASE_BOTH sltu sltiu, 100, -1, 1

    CASE and 0b11, 0b11, 0b11
    CASE and, -1, -1, -1
    CASE and, -1, 0, 0
    CASE and, -1, 40, 40
    CASE and, 0b101, 0b100, 0b100

    .macro CASE_AND a:req, b:req
        CASE and, \a, \b, \a & \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_AND

    CASE or, -1, 0, -1
    CASE or, -1, 40, -1
    CASE or, 0, 0, 0
    CASE or, 0b101, 0b110, 0b111

    .macro CASE_OR a:req, b:req
        CASE or, \a, \b, \a | \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_OR

    CASE xor, -1, 0, -1
    CASE xor, -1, -1, 0
    CASE xor 0b101, 0b100, 0b001

    .macro CASE_XOR a:req, b:req
        CASE xor, \a, \b, \a ^ \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_XOR

    CASE sll, 2, 1, 4
    CASE sll, 2, 20, 2097152
    CASE sll, 2, 30, 2147483648
    CASE sll, 2, 31, 0
    CASER sll, 2, 32, 2 # error for immediate
    CASE sll, 0, 10, 0
    CASE sll, 10, 0, 10
    CASE sll, -1, 31, -2147483648
    CASER sll, -1, 32, -1 # error for immediate

    CASE srl, 4, 1, 2
    CASE srl, 0, 10, 0
    CASE srl, 10, 0, 10
    CASE srl, -1, 1, 2147483647
    CASE srl, 0b111, 2, 0b001
    CASER srl, -1, 32, -1 # error for immediate

    CASER sub, 10, 5, 5
    CASER sub, -1, 1, -2
    CASER sub, 1, 2, -1
    CASER sub, -1, -2, 1
    CASER sub, 0, 4294967295, 1

    .macro CASE_SUB a:req, b:req
        CASER sub, \a, \b, \a - \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_SUB

    CASE sra, 4, 1, 2
    CASE sra, 0, 10, 0
    CASE sra, 10, 0, 10
    CASE sra, -1, 1, -1
    CASE sra, -1, 31, -1
    CASE sra, 0b111, 2, 0b001
    CASER sra, 10, 32, 10 # error for immediate

    # M extension

    CASER mul, 4, 4, 16
    CASER mul, 10, 0, 0
    CASER mul, 10, 1, 10
    CASER mul, -1, -1, 1
    CASER mul, 25252566, 5225225, 353909638

    .macro CASE_MUL a:req, b:req
        CASER mul, \a, \b, \a * \b
    .endm

    WITH_TWO_TEST_NUMBERS CASE_MUL

    CASER mulh 4, 4, 0
    CASER mulh, -1, -1, 0
    CASER mulh, 25252566, 5225225, 30722

    CASER mulhu 4, 4, 0
    CASER mulhu, -1, -1, 4294967294
    CASER mulhu, 25252566, 5225225, 30722

    # mulhsu hasn't been implemented yet.

    CASER div, 4, 2, 2
    CASER div, -1, 1, -1
    CASER div, 1, 1, 1
    CASER div, 1, 0, -1
    CASER div, -10, 2, -5
    CASER div, 5, 2, 2
    CASER div, 5, -1, -5
    CASER div, -2147483648, -1, -1

    CASER divu, 4, 2, 2
    CASER divu, -1, 1, -1
    CASER divu, 1, 1, 1
    CASER divu, 1, 0, -1
    CASER divu, -10, 2, 2147483643
    CASER divu, 5, 2, 2

    CASER rem, 4, 2, 0
    CASER rem, 5, 2, 1
    CASER rem, 5, 0, 5
    CASER rem, -10, 3, -1
    CASER rem, 5, -1, 0
    CASER rem, -2147483648, -1, 0

    CASER remu, 4, 2, 0
    CASER remu, 5, 2, 1
    CASER remu, 5, 0, 5
    CASER remu, -10, 3, 0

    PASS
