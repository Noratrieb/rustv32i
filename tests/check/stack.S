# Stack Pointer Addition (special-cased by compressed instructions)
# I've had two bugs in this area before...

#include "../helper.S"

.macro CASE_FOR_REGISTER reg:req number:req
    li \reg, 0
    addi \reg, \reg, \number
    ASSERT_EQ \reg, \number
.endm

# The goal is sp addition, but why not test some other registers as well while we're at it :)

.macro CASE number
    CASE_FOR_REGISTER sp, \number
    CASE_FOR_REGISTER ra, \number
    CASE_FOR_REGISTER a0, \number
    CASE_FOR_REGISTER s0, \number
    CASE_FOR_REGISTER t0, \number
.endm

START_TEST
    CASE 0
    CASE 1
    CASE 2
    CASE 4
    CASE 8
    CASE 10
    CASE 16
    CASE 32
    CASE 64
    CASE 100
    CASE 128
    CASE 200
    CASE 256
    CASE 512
    CASE 1024
    CASE 2047

    CASE -1
    CASE -2
    CASE -4
    CASE -8
    CASE -10
    CASE -16
    CASE -32
    CASE -64
    CASE -100
    CASE -128
    CASE -200
    CASE -256
    CASE -512
    CASE -1024
    CASE -2047
    CASE -2048

    PASS
