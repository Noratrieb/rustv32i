# Atomic Memory Operations

#include "../helper.S"

.macro CASE_BASE inst reg mem expected_mem
    li t0, 0
    li t1, \mem
    sw t1, (t0)
    li t3, \reg
    \inst t2, t3, (t0)
    ASSERT_EQ t2, \mem
    lw t3, (t0)
    ASSERT_EQ t3, \expected_mem
.endm

.macro CASE inst reg mem expected_mem
    CASE_BASE \inst, \reg, \mem, \expected_mem
    CASE_BASE \inst\().aq, \reg, \mem, \expected_mem
    CASE_BASE \inst\().rl, \reg, \mem, \expected_mem
    CASE_BASE \inst\().aqrl, \reg, \mem, \expected_mem
.endm

START_TEST
    CASE amoswap.w, 1, 0, 1
    CASE amoswap.w, 10, -1, 10
    CASE amoswap.w 0, 0, 0

    CASE amoadd.w, 1, 1, 2
    CASE amoadd.w, -1, 1, 0
    CASE amoadd.w, 10, -2, 8

    CASE amoand.w, 0b11, 0b11, 0b11
    CASE amoand.w, -1, -1, -1
    CASE amoand.w, -1, 0, 0
    CASE amoand.w, -1, 40, 40
    CASE amoand.w, 0b101, 0b100, 0b100

    CASE amoor.w, -1, 0, -1
    CASE amoor.w, -1, 40, -1
    CASE amoor.w, 0, 0, 0
    CASE amoor.w, 0b101, 0b110, 0b111

    CASE amoxor.w, -1, 0, -1
    CASE amoxor.w, -1, -1, 0
    CASE amoxor.w, 0b101, 0b100, 0b001

    CASE amomax.w, 0, 0, 0
    CASE amomax.w, 0, 1, 1
    CASE amomax.w, -1, 0, 0
    CASE amomax.w 100, -100, 100

    CASE amomaxu.w, 0, 0, 0
    CASE amomaxu.w, 0, 1, 1
    CASE amomaxu.w, -1, 0, -1
    CASE amomaxu.w 100, -100, -100

    CASE amomin.w, 0, 0, 0
    CASE amomin.w, 0, 1, 0
    CASE amomin.w, -1, 0, -1
    CASE amomin.w 100, -100, -100

    CASE amominu.w, 0, 0, 0
    CASE amominu.w, 0, 1, 0
    CASE amominu.w, -1, 0, 0
    CASE amominu.w 100, -100, 100

    PASS
