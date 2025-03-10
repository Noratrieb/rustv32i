# Control transfer instructions

#include "../helper.S"

START_TEST
    j unconditional
    FAIL
unconditional:

# Test branching instructions
    li t0, 10
    li t1, 10
    beq t0, t1, branch2
    FAIL
branch2:
    bge t0, t1, branch3
    FAIL
branch3:
    bne t0, t1, fail
    blt t0, t1, fail
    bltu t0, t1, fail

    li t0, -1
    li t1, 1
    blt t1, t0, fail
    bltu t0, t1, fail
    bge t0, t1, fail
    bgeu t1, t0, fail

    blt t0, t1, branch4
    FAIL
branch4:
    bltu t1, t0, branch5
    FAIL
branch5:
    bge t1, t0, branch6
    FAIL
branch6:
    bgeu t0, t1, branch7
    FAIL
branch7:

# Backwards jal
    li t0, 0
backwards:
    li t1, 0
    bne t0, t1, end_backwards
    li t0, 1
    jal backwards
end_backwards:

# Test link registers being set correctly:
    auipc t1, 0
    jal t0, link2
    jal t5, fail # force uncompressed 
link2:
    addi t1, t1, 8 # the instruction following the jump
    bne t1, t0, fail

    auipc t1, 0
    jalr t0, 12(t1) # 12 is the three instructions, so to the addi
    jal t5, fail # force uncompressed 

    addi t1, t1, 8 # the instruction following the jump
    bne t0, t1, fail

# Test a loop (t0=counter, t1=expected)
    li t0, 0
    li t1, 100
loop1:
    addi t0, t0, 1
    blt t0, t1, loop1

    ASSERT_EQ t0, 100

# End
    PASS
