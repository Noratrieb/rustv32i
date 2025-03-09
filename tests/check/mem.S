# Load and Store Instructions

#include "../helper.S"


.macro CASE_NO_OFFSET_DIFF_RESULT s l value result
    li t1, \value
    \s t1, 0(t0)
    \l t2, 0(t0)
    ASSERT_EQ t2, \result
.endm

.macro CASE_NO_OFFSET s l value
    CASE_NO_OFFSET_DIFF_RESULT \s, \l, \value, \value
.endm

.macro CASE_SAME_OFFSET s l value offset
    li t1, \value
    \s t1, \offset(t0)
    \l t2, \offset(t0)
    ASSERT_EQ t2, \value
.endm

.macro CASE_ADDED_OFFSET s l value offset
    addi t3, t0, \offset
    li t1, \value
    \s t1, \offset(t0)
    \l t2, 0(t3)
    ASSERT_EQ t2, \value
.endm

START_TEST
    li t0, 0

    ##### word

    CASE_NO_OFFSET    sw, lw, -4
    CASE_SAME_OFFSET  sw, lw, -5, 4
    CASE_SAME_OFFSET  sw, lw, -6, 1000
    CASE_ADDED_OFFSET sw, lw, -7, 4
    CASE_ADDED_OFFSET sw, lw, -8, 1000

    ##### half

    CASE_NO_OFFSET_DIFF_RESULT sh, lh, 65535, -1
    CASE_NO_OFFSET sh, lhu, 65535

    CASE_NO_OFFSET    sh, lh, 21450
    CASE_SAME_OFFSET  sh, lh, 21451, 4
    CASE_SAME_OFFSET  sh, lh, 21452, 1000
    CASE_ADDED_OFFSET sh, lh, 21453, 4
    CASE_ADDED_OFFSET sh, lh, 21454, 1000

    CASE_NO_OFFSET    sh, lhu, 20420
    CASE_SAME_OFFSET  sh, lhu, 20421, 4
    CASE_SAME_OFFSET  sh, lhu, 20422, 1000
    CASE_ADDED_OFFSET sh, lhu, 20423, 4
    CASE_ADDED_OFFSET sh, lhu, 20424, 1000

    ##### byte

    CASE_NO_OFFSET_DIFF_RESULT sb, lb, 255, -1
    CASE_NO_OFFSET sb, lbu, 255

    CASE_NO_OFFSET    sb, lb, 90
    CASE_SAME_OFFSET  sb, lb, 90, 4
    CASE_SAME_OFFSET  sb, lb, 91, 1000
    CASE_ADDED_OFFSET sb, lb, 92, 4
    CASE_ADDED_OFFSET sb, lb, 93, 1000

    CASE_NO_OFFSET    sb, lbu, 110
    CASE_SAME_OFFSET  sb, lbu, 110, 4
    CASE_SAME_OFFSET  sb, lbu, 111, 1000
    CASE_ADDED_OFFSET sb, lbu, 112, 4
    CASE_ADDED_OFFSET sb, lbu, 113, 1000

    PASS
