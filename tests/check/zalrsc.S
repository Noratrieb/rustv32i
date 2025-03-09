# Load-Reserved/Store-Conditional Instructions

#include "../helper.S"

.macro RESET_MEM
    li t0, 0
    sc.w zero, t0, (t0) # reset reservation set
    li t1, -1
    sw t1, 0(t0)
    li t1, -2
    sw t1, 4(t0)
.endm

START_TEST
    RESET_MEM

    lr.w t1, (t0)
    ASSERT_EQ t1, -1
    lr.w.aq t1, (t0)
    ASSERT_EQ t1, -1
    lr.w.rl t1, (t0)
    ASSERT_EQ t1, -1
    lr.w.aqrl t1, (t0)
    ASSERT_EQ t1, -1

    RESET_MEM

    # invalid SC
    li t2, 10
    sc.w t1, t2, (t0)
    ASSERT_EQ t1, 1
    li t2, 10
    sc.w.aq t1, t2, (t0)
    ASSERT_EQ t1, 1
    li t2, 10
    sc.w.rl t1, t2, (t0)
    ASSERT_EQ t1, 1
    li t2, 10
    sc.w.aqrl t1, t2, (t0)
    ASSERT_EQ t1, 1

    RESET_MEM

    li t1, 10
    lr.w zero, (t0)
    sc.w t1, t1, (t0)
    ASSERT_EQ t1, 0

    li t1, 10
    lr.w.aq zero, (t0)
    sc.w.rl t1, t1, (t0)
    ASSERT_EQ t1, 0

    li t1, 10
    lr.w.aqrl zero, (t0)
    sc.w.aqrl t1, t1, (t0)
    ASSERT_EQ t1, 0

    PASS
