# Integer computational register-register instruction.

#include "../helper.S"

.macro CASE_REG inst a b expected
    li t0, \a
    li t1, \b
    \inst t2, t0, t1
    ASSERT_EQ t2, \expected
.endm

.macro CASE_IMM inst a b expected
    li t0, \a
    \inst t2, t0, \b
    ASSERT_EQ t2, \expected
.endm

.macro CASE_BOTH inst insti a b expected
    CASE_REG \inst, \a, \b, \expected

    CASE_IMM \insti, \a, \b, \expected
.endm


.macro CASE inst a b expected
    CASE_BOTH \inst, \inst\()i, \a, \b, \expected
.endm

START_TEST
    CASE add 10, 20, 30
    CASE add 10, -2, 8
    CASE add 10, 0, 10
    CASE add 0, 0, 0

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

    CASE or, -1, 0, -1
    CASE or, -1, 40, -1
    CASE or, 0, 0, 0
    CASE or, 0b101, 0b110, 0b111

    CASE xor, -1, 0, -1
    CASE xor, -1, -1, 0
    CASE xor 0b101, 0b100, 0b001

    CASE sll, 2, 1, 4
    CASE sll, 0, 10, 0
    CASE sll, 10, 0, 10
    CASE sll, -1, 31, -2147483648
    CASE_REG sll, -1, 32, -1 # error for immediate

    CASE srl, 4, 1, 2
    CASE srl, 0, 10, 0
    CASE srl, 10, 0, 10
    CASE srl, -1, 1, 2147483647
    CASE srl, 0b111, 2, 0b001
    CASE_REG srl, -1, 32, -1 # error for immediate

    CASE_REG sub, 10, 5, 5
    CASE_REG sub, -1, 1, -2
    CASE_REG sub, 1, 2, -1
    CASE_REG sub, -1, -2, 1
    CASE_REG sub, 0, 4294967295, 1

    CASE sra, 4, 1, 2
    CASE sra, 0, 10, 0
    CASE sra, 10, 0, 10
    CASE sra, -1, 1, -1
    CASE sra, -1, 31, -1
    CASE sra, 0b111, 2, 0b001
    CASE_REG sra, 10, 32, 10 # error for immediate

    PASS
