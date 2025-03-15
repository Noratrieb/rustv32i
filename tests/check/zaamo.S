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
    .macro CASE_AMOSWAP a:req b:req
        CASE amoswap.w, \a, \b, \a
    .endm
    WITH_TWO_TEST_NUMBERS CASE_AMOSWAP

    .macro CASE_AMOADD a:req b:req
        CASE amoadd.w, \a, \b, \a + \b
    .endm
    WITH_TWO_TEST_NUMBERS CASE_AMOADD

    .macro CASE_AMOAND a:req b:req
        CASE amoand.w, \a, \b, \a & \b
    .endm
    WITH_TWO_TEST_NUMBERS CASE_AMOAND

    .macro CASE_AMOOR a:req b:req
        CASE amoor.w, \a, \b, \a | \b
    .endm
    WITH_TWO_TEST_NUMBERS CASE_AMOOR

    .macro CASE_AMOXOR a:req b:req
        CASE amoxor.w, \a, \b, \a ^ \b
    .endm
    WITH_TWO_TEST_NUMBERS CASE_AMOXOR

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
