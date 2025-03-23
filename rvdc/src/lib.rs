#![doc = include_str!("../README.md")]
#![no_std]
#![deny(missing_docs)]

use core::fmt::{self, Debug, Display};
use core::ops::RangeInclusive;

/// A decoded RISC-V integer register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub u8);

impl Reg {
    /// The zero register `zero` (`x0`)
    pub const ZERO: Reg = Reg(0);

    /// The return address register `ra` (`x1`)
    pub const RA: Reg = Reg(1);
    /// The stack pointer register `sp` (`x2`)
    pub const SP: Reg = Reg(2);
    /// The global pointer register `gp` (`x3`)
    pub const GP: Reg = Reg(3);
    /// The thread pointer register `tp` (`x4`)
    pub const TP: Reg = Reg(4);

    /// Saved register `s0` (`x8`)
    pub const S0: Reg = Reg(8);
    /// Saved register frame pointer `fp` (`s0`, `x8`)
    pub const FP: Reg = Reg(8);
    /// Saved register `s1` (`x9`)
    pub const S1: Reg = Reg(9);
    /// Saved register `s2` (`x18`)
    pub const S2: Reg = Reg(18);
    /// Saved register `s3` (`x19`)
    pub const S3: Reg = Reg(19);
    /// Saved register `s4` (`x20`)
    pub const S4: Reg = Reg(20);
    /// Saved register `s5` (`x21`)
    pub const S5: Reg = Reg(21);
    /// Saved register `s6` (`x22`)
    pub const S6: Reg = Reg(22);
    /// Saved register `s7` (`x23`)
    pub const S7: Reg = Reg(23);
    /// Saved register `s8` (`x24`)
    pub const S8: Reg = Reg(24);
    /// Saved register `s9` (`x25`)
    pub const S9: Reg = Reg(25);
    /// Saved register `s10` (`x26`)
    pub const S10: Reg = Reg(26);
    /// Saved register `s11` (`x27`)
    pub const S11: Reg = Reg(27);

    /// Argument/return value register `a0` (`x10`)
    pub const A0: Reg = Reg(10);
    /// Argument/return value register `a1` (`x11`)
    pub const A1: Reg = Reg(11);
    /// Argument register `a2` (`x12`)
    pub const A2: Reg = Reg(12);
    /// Argument register `a3` (`x13`)
    pub const A3: Reg = Reg(13);
    /// Argument register `a4` (`x14`)
    pub const A4: Reg = Reg(14);
    /// Argument register `a5` (`x15`)
    pub const A5: Reg = Reg(15);
    /// Argument register `a6` (`x16`)
    pub const A6: Reg = Reg(16);
    /// Argument register `a7` (`x17`)
    pub const A7: Reg = Reg(17);

    /// Temporary register `t0` (`x5`)
    pub const T0: Reg = Reg(5);
    /// Temporary register `t1` (`x6`)
    pub const T1: Reg = Reg(6);
    /// Temporary register `t2` (`x7`)
    pub const T2: Reg = Reg(7);
    /// Temporary register `t3` (`x28`)
    pub const T3: Reg = Reg(28);
    /// Temporary register `t4` (`x29`)
    pub const T4: Reg = Reg(29);
    /// Temporary register `t5` (`x30`)
    pub const T5: Reg = Reg(30);
    /// Temporary register `t6` (`x31`)
    pub const T6: Reg = Reg(31);
}

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = self.0;
        match n {
            0 => write!(f, "zero"),
            1 => write!(f, "ra"),
            2 => write!(f, "sp"),
            3 => write!(f, "gp"),
            4 => write!(f, "tp"),
            5..=7 => write!(f, "t{}", n - 5),
            8 => write!(f, "s0"),
            9 => write!(f, "s1"),
            10..=17 => write!(f, "a{}", n - 10),
            18..=27 => write!(f, "s{}", n - 18 + 2),
            28..=31 => write!(f, "t{}", n - 28 + 3),
            _ => unreachable!("invalid register"),
        }
    }
}

/// A RISC-V instruction.
/// 
/// Every variant is a different instruction, with immediates as `u32`.
/// For instructions that sign-extend immediates, the immediates will have been
/// sign-extended already, so the value can be used as-is.
/// For instructions that have immediates in the upper bits (`lui`, `auipc`),
/// the shift will have been done already, so the value can also be used as-is.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
#[expect(missing_docs)] // enum variant fields
pub enum Inst {
    /// Load Upper Immediate
    Lui { uimm: u32, dest: Reg },
    /// Add Upper Immediate to PC
    Auipc { uimm: u32, dest: Reg },

    /// Jump And Link
    Jal { offset: u32, dest: Reg },
    /// Jump And Link Register (indirect)
    Jalr { offset: u32, base: Reg, dest: Reg },

    /// Branch Equal
    Beq { offset: u32, src1: Reg, src2: Reg },
    /// Branch Not Equal
    Bne { offset: u32, src1: Reg, src2: Reg },
    /// Branch Less Than (signed)
    Blt { offset: u32, src1: Reg, src2: Reg },
    /// Branch Greater or Equal (signed)
    Bge { offset: u32, src1: Reg, src2: Reg },
    /// Branch Less Than Unsigned
    Bltu { offset: u32, src1: Reg, src2: Reg },
    /// Branch Greater or Equal Unsigned
    Bgeu { offset: u32, src1: Reg, src2: Reg },

    /// Load Byte (sign-ext)
    Lb { offset: u32, dest: Reg, base: Reg },
    /// Load Unsigned Byte (zero-ext)
    Lbu { offset: u32, dest: Reg, base: Reg },
    /// Load Half (sign-ext)
    Lh { offset: u32, dest: Reg, base: Reg },
    /// Load Unsigned Half (zero-ext)
    Lhu { offset: u32, dest: Reg, base: Reg },
    /// Load Word
    Lw { offset: u32, dest: Reg, base: Reg },

    /// Store Byte
    Sb { offset: u32, src: Reg, base: Reg },
    /// Store Half
    Sh { offset: u32, src: Reg, base: Reg },
    /// Store Word
    Sw { offset: u32, src: Reg, base: Reg },

    /// Add Immediate
    Addi { imm: u32, dest: Reg, src1: Reg },
    /// Set Less Than Immediate (signed)
    Slti { imm: u32, dest: Reg, src1: Reg },
    /// Set Less Than Immediate Unsigned
    Sltiu { imm: u32, dest: Reg, src1: Reg },
    /// XOR Immediate
    Xori { imm: u32, dest: Reg, src1: Reg },
    /// OR Immediate
    Ori { imm: u32, dest: Reg, src1: Reg },
    /// AND Immediate
    Andi { imm: u32, dest: Reg, src1: Reg },
    /// Shift Left Logical Immediate
    Slli { imm: u32, dest: Reg, src1: Reg },
    /// Shift Right Logical Immediate (unsigned)
    Srli { imm: u32, dest: Reg, src1: Reg },
    /// Shift Right Arithmetic Immediate (signed)
    Srai { imm: u32, dest: Reg, src1: Reg },

    /// Add
    Add { dest: Reg, src1: Reg, src2: Reg },
    /// Subtract
    Sub { dest: Reg, src1: Reg, src2: Reg },
    /// Shift Left Logical
    Sll { dest: Reg, src1: Reg, src2: Reg },
    /// Set Less Than (signed)
    Slt { dest: Reg, src1: Reg, src2: Reg },
    /// Set Less Than Unsigned
    Sltu { dest: Reg, src1: Reg, src2: Reg },
    /// XOR
    Xor { dest: Reg, src1: Reg, src2: Reg },
    /// Shift Right Logical (unsigned)
    Srl { dest: Reg, src1: Reg, src2: Reg },
    /// Shift Right Arithmetic (unsigned)
    Sra { dest: Reg, src1: Reg, src2: Reg },
    /// OR
    Or { dest: Reg, src1: Reg, src2: Reg },
    /// AND
    And { dest: Reg, src1: Reg, src2: Reg },
    /// Memory Fence
    Fence { fence: Fence },

    /// ECALL, call into environment
    Ecall,
    /// EBREAK, break into debugger
    Ebreak,

    // ------------- M extension -------------
    /// Multiply
    Mul { dest: Reg, src1: Reg, src2: Reg },
    /// Mul Upper Half Signed-Signed
    Mulh { dest: Reg, src1: Reg, src2: Reg },
    /// Mul Upper Half Signed-Unsigned
    Mulhsu { dest: Reg, src1: Reg, src2: Reg },
    /// Mul Upper Half Unsigned-Unsigned
    Mulhu { dest: Reg, src1: Reg, src2: Reg },
    /// Divide (signed)
    Div { dest: Reg, src1: Reg, src2: Reg },
    /// Divide Unsigned
    Divu { dest: Reg, src1: Reg, src2: Reg },
    /// Remainder (signed)
    Rem { dest: Reg, src1: Reg, src2: Reg },
    /// Remainder Unsigned
    Remu { dest: Reg, src1: Reg, src2: Reg },

    // ------------- A extension -------------
    /// Load-Reserved Word
    LrW {
        order: AmoOrdering,
        dest: Reg,
        addr: Reg,  
    },
    /// Store-Conditional Word
    ScW {
        order: AmoOrdering,
        dest: Reg,
        addr: Reg,
        src: Reg,
    },
    /// Atomic Memory Operation
    AmoW {
        order: AmoOrdering,
        op: AmoOp,
        dest: Reg,
        addr: Reg,
        src: Reg,
    },
}

/// The details of a RISC-V `fence` instruction.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fence {
    /// The `fm` field of the instruction.
    /// - `0b0000` is a normal fence
    /// - `0b1000` with `rw,rw` implies a `fence.tso`
    pub fm: u8,
    /// The predecessor set.
    pub pred: FenceSet,
    /// The sucessor set.
    pub succ: FenceSet,
    /// The `rd` field of the instruction. Currently always zero.
    pub dest: Reg,
    /// The `rs1` field of the instruction. Currently always zero.
    pub src: Reg,
}

/// The affected parts of a fence.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[expect(missing_docs)]
pub struct FenceSet {
    pub device_input: bool,
    pub device_output: bool,
    pub memory_read: bool,
    pub memory_write: bool,
}

/// An atomic memory ordering for instructions from the A extension.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AmoOrdering {
    /// No bits.
    Relaxed,
    /// `aq`
    Acquire,
    /// `rl`
    Release,
    /// `aq`, `rl`
    SeqCst,
}

/// An atomic memory operations from the Zaamo extension.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AmoOp {
    /// Swap
    Swap,
    /// ADD
    Add,
    /// XOR
    Xor,
    /// AND
    And,
    /// OR
    Or,
    /// Signed minimum
    Min,
    /// Signed maximum
    Max,
    /// Unsigned minimum
    Minu,
    /// Unsigned maximum
    Maxu,
}

/// The error used for invalid instructions containing information about the instruction and error.
///
/// Note that this is also returned for the defined illegal instruction of all zero.
pub struct DecodeError {
    /// The instruction bytes that failed to decode.
    pub instruction: u32,
    /// Which field of the instruction contained unexpected bits.
    pub unexpected_field: &'static str,
}

impl Fence {
    /// Whether this is a `fence.tso`.
    /// `fm=0b1000` and `RW,RW`
    pub fn is_tso(&self) -> bool {
        matches!(
            self,
            Self {
                fm: 0b1000,
                pred: FenceSet {
                    device_input: _,
                    device_output: _,
                    memory_read: true,
                    memory_write: true
                },
                succ: FenceSet {
                    device_input: _,
                    device_output: _,
                    memory_read: true,
                    memory_write: true
                },
                ..
            }
        )
    }
    /// Whether this fence indicates a `pause` assembler pseudoinstruction.
    pub fn is_pause(&self) -> bool {
        self.pred
            == FenceSet {
                device_input: false,
                device_output: false,
                memory_read: false,
                memory_write: true,
            }
            && self.succ
                == FenceSet {
                    device_input: false,
                    device_output: false,
                    memory_read: false,
                    memory_write: false,
                }
            && self.dest == Reg::ZERO
            && self.src == Reg::ZERO
    }
}

impl AmoOrdering {
    /// Create a new [`AmoOrdering`] from the two ordering bits.
    pub fn from_aq_rl(aq: bool, rl: bool) -> Self {
        match (aq, rl) {
            (false, false) => Self::Relaxed,
            (true, false) => Self::Acquire,
            (false, true) => Self::Release,
            (true, true) => Self::SeqCst,
        }
    }
}

impl Debug for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self, f)
    }
}

/// Prints the instruction in disassembled form.
///
/// Note that the precise output here is not considered stable.
impl Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Inst::Lui { uimm, dest } => write!(f, "lui {dest}, {}", uimm >> 12),
            Inst::Auipc { uimm, dest } => write!(f, "auipc {dest}, {}", uimm >> 12),
            Inst::Jal { offset, dest } => {
                if dest.0 == 0 {
                    write!(f, "j {}", offset as i32)
                } else {
                    write!(f, "jal {dest}, {}", offset as i32)
                }
            }
            Inst::Jalr { offset, base, dest } => {
                if dest == Reg::ZERO && offset == 0 && base == Reg::RA {
                    write!(f, "ret")
                } else {
                    write!(f, "jalr {dest}, {}({base})", offset as i32)
                }
            }
            Inst::Beq { offset, src1, src2 } => write!(f, "beq {src1}, {src2}, {}", offset as i32),
            Inst::Bne { offset, src1, src2 } => write!(f, "bne {src1}, {src2}, {}", offset as i32),
            Inst::Blt { offset, src1, src2 } => write!(f, "blt {src1}, {src2}, {}", offset as i32),
            Inst::Bge { offset, src1, src2 } => write!(f, "bge {src1}, {src2}, {}", offset as i32),
            Inst::Bltu { offset, src1, src2 } => {
                write!(f, "bltu {src1}, {src2}, {}", offset as i32)
            }
            Inst::Bgeu { offset, src1, src2 } => {
                write!(f, "bgeu {src1}, {src2}, {}", offset as i32)
            }
            Inst::Lb { offset, dest, base } => write!(f, "lb {dest}, {}({base})", offset as i32),
            Inst::Lbu { offset, dest, base } => write!(f, "lbu {dest}, {}({base})", offset as i32),
            Inst::Lh { offset, dest, base } => write!(f, "lh {dest}, {}({base})", offset as i32),
            Inst::Lhu { offset, dest, base } => write!(f, "lhu {dest}, {}({base})", offset as i32),
            Inst::Lw { offset, dest, base } => write!(f, "lw {dest}, {}({base})", offset as i32),
            Inst::Sb { offset, src, base } => write!(f, "sb {src}, {}({base})", offset as i32),
            Inst::Sh { offset, src, base } => write!(f, "sh {src}, {}({base})", offset as i32),
            Inst::Sw { offset, src, base } => write!(f, "sw {src}, {}({base})", offset as i32),
            Inst::Addi { imm, dest, src1 } => {
                if dest.0 == 0 && src1.0 == 0 && imm == 0 {
                    write!(f, "nop")
                } else if src1.0 == 0 {
                    write!(f, "li {dest}, {}", imm as i32)
                } else if imm == 0 {
                    write!(f, "mv {dest}, {src1}")
                } else {
                    write!(f, "addi {dest}, {src1}, {}", imm as i32)
                }
            }
            Inst::Slti {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "slti {dest}, {rs1}, {}", imm as i32),
            Inst::Sltiu {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "sltiu {dest}, {rs1}, {}", imm as i32),
            Inst::Andi {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "andi {dest}, {rs1}, {}", imm as i32),
            Inst::Ori {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "ori {dest}, {rs1}, {}", imm as i32),
            Inst::Xori {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "xori {dest}, {rs1}, {}", imm as i32),
            Inst::Slli {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "slli {dest}, {rs1}, {}", imm as i32),
            Inst::Srli {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "srli {dest}, {rs1}, {}", imm as i32),
            Inst::Srai {
                imm,
                dest,
                src1: rs1,
            } => write!(f, "srai {dest}, {rs1}, {}", imm as i32),
            Inst::Add { dest, src1, src2 } => {
                write!(f, "add {dest}, {src1}, {src2}")
            }
            Inst::Sub { dest, src1, src2 } => write!(f, "sub {dest}, {src1}, {src2}"),
            Inst::Sll { dest, src1, src2 } => write!(f, "sll {dest}, {src1}, {src2}"),
            Inst::Slt { dest, src1, src2 } => write!(f, "slt {dest}, {src1}, {src2}"),
            Inst::Sltu { dest, src1, src2 } => write!(f, "sltu {dest}, {src1}, {src2}"),
            Inst::Xor { dest, src1, src2 } => write!(f, "xor {dest}, {src1}, {src2}"),
            Inst::Srl { dest, src1, src2 } => write!(f, "srl {dest}, {src1}, {src2}"),
            Inst::Sra { dest, src1, src2 } => write!(f, "sra {dest}, {src1}, {src2}"),
            Inst::Or { dest, src1, src2 } => write!(f, "or {dest}, {src1}, {src2}"),
            Inst::And { dest, src1, src2 } => write!(f, "and {dest}, {src1}, {src2}"),
            Inst::Fence { fence } => match fence.fm {
                _ if fence.is_tso() => write!(f, "fence.tso"),
                0b0000 if fence.is_pause() => {
                    write!(f, "pause")
                }
                _ => write!(f, "fence {},{}", fence.pred, fence.succ),
            },
            Inst::Ecall => write!(f, "ecall"),
            Inst::Ebreak => write!(f, "ebreak"),
            Inst::Mul { dest, src1, src2 } => write!(f, "mul {dest}, {src1}, {src2}"),
            Inst::Mulh { dest, src1, src2 } => write!(f, "mulh {dest}, {src1}, {src2}"),
            Inst::Mulhsu { dest, src1, src2 } => write!(f, "mulhsu {dest}, {src1}, {src2}"),
            Inst::Mulhu { dest, src1, src2 } => write!(f, "mulhu {dest}, {src1}, {src2}"),
            Inst::Div { dest, src1, src2 } => write!(f, "div {dest}, {src1}, {src2}"),
            Inst::Divu { dest, src1, src2 } => write!(f, "divu {dest}, {src1}, {src2}"),
            Inst::Rem { dest, src1, src2 } => write!(f, "rem {dest}, {src1}, {src2}"),
            Inst::Remu { dest, src1, src2 } => write!(f, "remu {dest}, {src1}, {src2}"),
            Inst::LrW { order, dest, addr } => write!(f, "lr.w{order} {dest}, ({addr})",),
            Inst::ScW {
                order,
                dest,
                addr,
                src,
            } => write!(f, "sc.w{order} {dest}, {src}, ({addr})"),
            Inst::AmoW {
                order,
                op,
                dest,
                addr,
                src,
            } => write!(f, "amo{op}.w{order} {dest}, {src}, ({addr})",),
        }
    }
}

impl Display for FenceSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut has = false;
        if self.device_input {
            has = true;
            write!(f, "i")?;
        }
        if self.device_output {
            has = true;
            write!(f, "o")?;
        }
        if self.memory_read {
            has = true;
            write!(f, "r")?;
        }
        if self.memory_write {
            has = true;
            write!(f, "w")?;
        }
        if !has {
            write!(f, "0")?;
        }
        Ok(())
    }
}

impl Display for AmoOrdering {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AmoOrdering::Relaxed => write!(f, ""),
            AmoOrdering::Acquire => write!(f, ".aq"),
            AmoOrdering::Release => write!(f, ".rl"),
            AmoOrdering::SeqCst => write!(f, ".aqrl"),
        }
    }
}

impl Display for AmoOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AmoOp::Swap => write!(f, "swap"),
            AmoOp::Add => write!(f, "add"),
            AmoOp::Xor => write!(f, "xor"),
            AmoOp::And => write!(f, "and"),
            AmoOp::Or => write!(f, "or"),
            AmoOp::Min => write!(f, "min"),
            AmoOp::Max => write!(f, "max"),
            AmoOp::Minu => write!(f, "minu"),
            AmoOp::Maxu => write!(f, "maxu"),
        }
    }
}

impl Debug for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DecodeError")
            .field("instruction", &format_args!("{:0>32b}", self.instruction))
            .field("unexpected_field", &self.unexpected_field)
            .finish()
    }
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to decode instruction '{:0>32b}' because of field '{}'",
            self.instruction, self.unexpected_field
        )
    }
}

impl core::error::Error for DecodeError {}

fn sign_extend(value: u32, size: u32) -> u32 {
    let right = u32::BITS - size;
    (((value << right) as i32) >> right) as u32
}

#[derive(Clone, Copy)]
struct InstCode(u32);

impl InstCode {
    fn extract(self, range: RangeInclusive<u32>) -> u32 {
        let end_span = 32 - (range.end() + 1);
        (self.0 << (end_span)) >> (end_span + range.start())
    }
    fn immediate_u(self, mappings: &[(RangeInclusive<u32>, u32)]) -> u32 {
        let mut imm = 0;
        for (from, to) in mappings {
            let value = self.extract(from.clone());
            imm |= value << to;
        }
        imm
    }
    fn immediate_s(self, mappings: &[(RangeInclusive<u32>, u32)]) -> u32 {
        let mut imm = 0;
        let mut size = 0;
        for (from, to) in mappings {
            let value = self.extract(from.clone());
            imm |= value << to;
            let this_size = from.end() - from.start() + 1;
            size = size.max(*to + this_size);
        }
        sign_extend(imm, size)
    }

    fn opcode(self) -> u32 {
        self.0 & 0b1111111
    }
    fn funct3(self) -> u32 {
        self.extract(12..=14)
    }
    fn funct7(self) -> u32 {
        self.extract(25..=31)
    }
    fn rs1(self) -> Reg {
        Reg(self.extract(15..=19) as u8)
    }
    fn rs2(self) -> Reg {
        Reg(self.extract(20..=24) as u8)
    }
    fn rs2_imm(self) -> u32 {
        self.extract(20..=24)
    }
    fn rd(self) -> Reg {
        Reg(self.extract(7..=11) as u8)
    }
    fn imm_i(self) -> u32 {
        self.immediate_s(&[(20..=31, 0)])
    }
    fn imm_s(self) -> u32 {
        self.immediate_s(&[(25..=31, 5), (7..=11, 0)])
    }
    fn imm_b(self) -> u32 {
        self.immediate_s(&[(31..=31, 12), (7..=7, 11), (25..=30, 5), (8..=11, 1)])
    }
    fn imm_u(self) -> u32 {
        self.immediate_u(&[(12..=31, 12)])
    }
    fn imm_j(self) -> u32 {
        self.immediate_s(&[(31..=31, 20), (21..=30, 1), (20..=20, 11), (12..=19, 12)])
    }
}

#[derive(Clone, Copy)]
struct InstCodeC(u16);

impl InstCodeC {
    fn extract(self, range: RangeInclusive<u32>) -> u32 {
        let end_span = u16::BITS - (range.end() + 1);
        ((self.0 << (end_span)) >> (end_span + range.start())) as u32
    }
    fn immediate_u(self, mappings: &[(RangeInclusive<u32>, u32)]) -> u32 {
        let mut imm = 0;
        for (from, to) in mappings {
            let value = self.extract(from.clone());
            imm |= value << to;
        }
        imm
    }
    fn immediate_s(self, mappings: &[(RangeInclusive<u32>, u32)]) -> u32 {
        let mut imm = 0;
        let mut size = 0;
        for (from, to) in mappings {
            assert!(from.start() <= from.end());
            let value = self.extract(from.clone());
            imm |= value << to;
            let this_size = from.end() - from.start() + 1;
            size = size.max(*to + this_size);
        }
        sign_extend(imm, size)
    }
    fn quadrant(self) -> u16 {
        self.0 & 0b11
    }
    fn funct3(self) -> u32 {
        self.extract(13..=15)
    }
    fn funct2(self) -> u32 {
        self.extract(10..=11)
    }
    /// rd/rs1 (7..=11)
    fn rd(self) -> Reg {
        Reg(self.extract(7..=11) as u8)
    }
    /// rs2 (2..=6)
    fn rs2(self) -> Reg {
        Reg(self.extract(2..=6) as u8)
    }
    /// rs1' (7..=9)
    fn rs1_short(self) -> Reg {
        let smol_reg = self.extract(7..=9);
        // map to x8..=x15
        Reg((smol_reg + 8) as u8)
    }
    /// rs2' (2..=4)
    fn rs2_short(self) -> Reg {
        let smol_reg = self.extract(2..=4);
        // map to x8..=x15
        Reg((smol_reg + 8) as u8)
    }
}

impl From<InstCodeC> for InstCode {
    fn from(value: InstCodeC) -> Self {
        Self(value.0 as u32)
    }
}

/// Whether the decoded instruction was a compressed instruction or not.
/// If it was compressed, only the first two bytes were used.
/// If it was not compressed, all four bytes are consumed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IsCompressed {
    /// Normal 4-byte instruction
    No,
    /// Compressed 2-byte instruction
    Yes,
}

fn decode_error(instruction: impl Into<InstCode>, unexpected_field: &'static str) -> DecodeError {
    DecodeError {
        instruction: instruction.into().0,
        unexpected_field,
    }
}

impl Inst {
    /// Whether the first byte of an instruction indicates a compressed or uncompressed instruction.
    pub fn first_byte_is_compressed(byte: u8) -> bool {
        (byte & 0b11) != 0b11
    }

    /// Decode an instruction from four bytes.
    ///
    /// The instruction may be compressed, in which case only two bytes are consumed.
    /// Even in these cases, the full next four bytes must be passed.
    ///
    /// If the caller wants to avoid reading more bytes than necessary, [`Self::first_byte_is_compressed`]
    /// can be used to check, read the required bytes, and then call [`Self::decode_compressed`] or
    /// [`Self::decode_normal`] directly.
    pub fn decode(code: u32) -> Result<(Inst, IsCompressed), DecodeError> {
        let is_compressed = (code & 0b11) != 0b11;
        if is_compressed {
            Ok((Self::decode_compressed(code as u16)?, IsCompressed::Yes))
        } else {
            Ok((Self::decode_normal(code)?, IsCompressed::No))
        }
    }

    /// Decode a known compressed instruction from its two bytes.
    ///
    /// # Example
    /// ```rust
    /// // Compressed addi sp, sp, -0x20
    /// let x = 0x1101_u16;
    /// let expected = rvdc::Inst::Addi { imm: (-0x20_i32) as u32, dest: rvdc::Reg::SP, src1: rvdc::Reg::SP };
    ///
    /// let inst = rvdc::Inst::decode_compressed(x).unwrap();
    /// assert_eq!(inst, expected);
    /// ```
    pub fn decode_compressed(code: u16) -> Result<Inst, DecodeError> {
        let code = InstCodeC(code);
        if code.0 == 0 {
            return Err(decode_error(code, "null instruction"));
        }
        let inst = match code.quadrant() {
            // C0
            0b00 => match code.funct3() {
                // C.ADDI4SPN -> addi \rd', sp, \imm
                0b000 => Inst::Addi {
                    imm: code.immediate_u(&[(5..=5, 3), (6..=6, 2), (7..=10, 6), (11..=12, 4)]),
                    dest: code.rs2_short(),
                    src1: Reg::SP,
                },
                // C.LW -> lw \dest \offset(\base)
                0b010 => Inst::Lw {
                    offset: code.immediate_u(&[(10..=12, 3), (5..=5, 6), (6..=6, 2)]),
                    dest: code.rs2_short(),
                    base: code.rs1_short(),
                },
                // C.SW -> sw \src, \offset(\base)
                0b110 => Inst::Sw {
                    offset: code.immediate_u(&[(10..=12, 3), (5..=5, 6), (6..=6, 2)]),
                    src: code.rs2_short(),
                    base: code.rs1_short(),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // C1
            0b01 => match code.funct3() {
                // C.ADDI -> addi \rd, \rd, \imm
                0b000 => Inst::Addi {
                    imm: code.immediate_s(&[(2..=6, 0), (12..=12, 5)]),
                    dest: code.rd(),
                    src1: code.rd(),
                },
                // C.JAL -> jal ra, \offset
                0b001 => Inst::Jal {
                    offset: code.immediate_s(&[
                        (2..=2, 5),
                        (3..=5, 1),
                        (6..=6, 7),
                        (7..=7, 6),
                        (8..=8, 10),
                        (9..=10, 8),
                        (11..=11, 4),
                        (12..=12, 11),
                    ]),
                    dest: Reg::RA,
                },
                // C.LI -> addi \rd, zero, \imm
                0b010 => Inst::Addi {
                    imm: code.immediate_s(&[(2..=6, 0), (12..=12, 5)]),
                    dest: code.rd(),
                    src1: Reg::ZERO,
                },
                // Arithmetic instructions
                0b100 => {
                    let bit12 = code.extract(12..=12);
                    match code.funct2() {
                        // C.SRLI -> srli \rd', \rd', \imm
                        0b00 => {
                            if bit12 != 0 {
                                return Err(decode_error(code, "imm"));
                            }

                            Inst::Srli {
                                imm: code.immediate_u(&[(2..=6, 0), (12..=12, 5)]),
                                dest: code.rs1_short(),
                                src1: code.rs1_short(),
                            }
                        }
                        // C.SRAI -> srai \rd', \rd', \imm
                        0b01 => {
                            if bit12 != 0 {
                                return Err(decode_error(code, "imm"));
                            }

                            Inst::Srai {
                                imm: code.immediate_u(&[(2..=6, 0), (12..=12, 5)]),
                                dest: code.rs1_short(),
                                src1: code.rs1_short(),
                            }
                        }
                        // C.ANDI -> andi \rd', \rd', \imm
                        0b10 => Inst::Andi {
                            imm: code.immediate_u(&[(2..=6, 0), (12..=12, 5)]),
                            dest: code.rs1_short(),
                            src1: code.rs1_short(),
                        },
                        0b11 => {
                            if bit12 != 0 {
                                return Err(decode_error(code, "bit 12"));
                            }
                            let funct2 = code.extract(5..=6);
                            match funct2 {
                                // C.SUB -> sub \rd', \rd', \rs2'
                                0b00 => Inst::Sub {
                                    dest: code.rs1_short(),
                                    src1: code.rs1_short(),
                                    src2: code.rs2_short(),
                                },
                                // C.XOR -> xor \rd', \rd', \rs2'
                                0b01 => Inst::Xor {
                                    dest: code.rs1_short(),
                                    src1: code.rs1_short(),
                                    src2: code.rs2_short(),
                                },
                                // C.OR -> or \rd', \rd', \rs2'
                                0b10 => Inst::Or {
                                    dest: code.rs1_short(),
                                    src1: code.rs1_short(),
                                    src2: code.rs2_short(),
                                },
                                // C.AND -> and \rd', \rd', \rs2'
                                0b11 => Inst::And {
                                    dest: code.rs1_short(),
                                    src1: code.rs1_short(),
                                    src2: code.rs2_short(),
                                },
                                _ => unreachable!("only two bits"),
                            }
                        }
                        _ => unreachable!("only two bits"),
                    }
                }
                // C.J -> jal zero, \offset
                0b101 => Inst::Jal {
                    offset: code.immediate_s(&[
                        (2..=2, 5),
                        (3..=5, 1),
                        (6..=6, 7),
                        (7..=7, 6),
                        (8..=8, 10),
                        (9..=10, 8),
                        (11..=11, 4),
                        (12..=12, 11),
                    ]),
                    dest: Reg::ZERO,
                },
                0b011 => {
                    match code.rd().0 {
                        // C.ADDI16SP -> addi sp, sp, \imm
                        2 => Inst::Addi {
                            imm: code.immediate_s(&[
                                (2..=2, 5),
                                (3..=4, 7),
                                (5..=5, 6),
                                (6..=6, 4),
                                (12..=12, 9),
                            ]),
                            dest: Reg::SP,
                            src1: Reg::SP,
                        },
                        // C.LUI -> lui \rd, \imm
                        _ => {
                            let uimm = code.immediate_s(&[(2..=6, 12), (12..=12, 17)]);
                            if uimm == 0 {
                                return Err(decode_error(code, "imm"));
                            }
                            Inst::Lui {
                                uimm,
                                dest: code.rd(),
                            }
                        }
                    }
                }
                // C.BEQZ -> beq \rs1', zero, \offset
                0b110 => Inst::Beq {
                    offset: code.immediate_s(&[
                        (2..=2, 5),
                        (3..=4, 1),
                        (5..=6, 6),
                        (10..=11, 3),
                        (12..=12, 8),
                    ]),
                    src1: code.rs1_short(),
                    src2: Reg::ZERO,
                },
                // C.BEQZ -> bne \rs1', zero, \offset
                0b111 => Inst::Bne {
                    offset: code.immediate_s(&[
                        (2..=2, 5),
                        (3..=4, 1),
                        (5..=6, 6),
                        (10..=11, 3),
                        (12..=12, 8),
                    ]),
                    src1: code.rs1_short(),
                    src2: Reg::ZERO,
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // C2
            0b10 => match code.funct3() {
                // C.SLLI -> slli \rd, \rd, \imm
                0b000 => {
                    if code.extract(12..=12) != 0 {
                        return Err(decode_error(code, "imm"));
                    }
                    Inst::Slli {
                        imm: code.immediate_u(&[(2..=6, 0), (12..=12, 5)]),
                        dest: code.rd(),
                        src1: code.rd(),
                    }
                }
                // C.LWSP -> lw \reg \offset(sp)
                0b010 => {
                    let dest = code.rd();
                    if dest.0 == 0 {
                        return Err(decode_error(code, "rd"));
                    }

                    Inst::Lw {
                        offset: code.immediate_u(&[(12..=12, 5), (4..=6, 2), (2..=3, 6)]),
                        dest,
                        base: Reg::SP,
                    }
                }
                0b100 => {
                    let bit = code.extract(12..=12);
                    let rs2 = code.rs2();
                    let rd_rs1 = code.rd();
                    match (bit, rd_rs1.0, rs2.0) {
                        // C.JR -> jalr zero, 0(\rs1)
                        (0, _, 0) => {
                            if rd_rs1.0 == 0 {
                                return Err(decode_error(code, "rs1"));
                            }
                            Inst::Jalr {
                                offset: 0,
                                base: rd_rs1,
                                dest: Reg::ZERO,
                            }
                        }
                        // C.MV -> add \rd, x0, \rs2
                        (0, _, _) => Inst::Add {
                            dest: code.rd(),
                            src1: Reg::ZERO,
                            src2: code.rs2(),
                        },
                        // C.EBREAK -> ebreak
                        (1, 0, 0) => Inst::Ebreak,
                        // C.JALR -> jalr ra, 0(\rs1)
                        (1, _, 0) if rd_rs1.0 != 0 => Inst::Jalr {
                            offset: 0,
                            base: rd_rs1,
                            dest: Reg::RA,
                        },
                        // C.ADD -> add \rd, \rd, \rs2
                        (1, _, _) => Inst::Add {
                            dest: rd_rs1,
                            src1: rd_rs1,
                            src2: rs2,
                        },
                        _ => return Err(decode_error(code, "inst")),
                    }
                }
                // C.SWSP -> sw \reg \offset(sp)
                0b110 => Inst::Sw {
                    offset: code.immediate_u(&[(7..=8, 6), (9..=12, 2)]),
                    src: code.rs2(),
                    base: Reg::SP,
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            _ => return Err(decode_error(code, "op")),
        };
        Ok(inst)
    }

    /// Decode a normal (not compressed) instruction.
    pub fn decode_normal(code: u32) -> Result<Inst, DecodeError> {
        let code = InstCode(code);
        let inst = match code.opcode() {
            // LUI
            0b0110111 => Inst::Lui {
                uimm: code.imm_u(),
                dest: code.rd(),
            },
            // AUIPC
            0b0010111 => Inst::Auipc {
                uimm: code.imm_u(),
                dest: code.rd(),
            },
            // JAL
            0b1101111 => Inst::Jal {
                offset: code.imm_j(),
                dest: code.rd(),
            },
            // JALR
            0b1100111 => match code.funct3() {
                0b000 => Inst::Jalr {
                    offset: code.imm_i(),
                    base: code.rs1(),
                    dest: code.rd(),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // BRANCH
            0b1100011 => match code.funct3() {
                0b000 => Inst::Beq {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                0b001 => Inst::Bne {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                0b100 => Inst::Blt {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                0b101 => Inst::Bge {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                0b110 => Inst::Bltu {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                0b111 => Inst::Bgeu {
                    offset: code.imm_b(),
                    src1: code.rs1(),
                    src2: code.rs2(),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // LOAD
            0b0000011 => match code.funct3() {
                0b000 => Inst::Lb {
                    offset: code.imm_i(),
                    dest: code.rd(),
                    base: code.rs1(),
                },
                0b001 => Inst::Lh {
                    offset: code.imm_i(),
                    dest: code.rd(),
                    base: code.rs1(),
                },
                0b010 => Inst::Lw {
                    offset: code.imm_i(),
                    dest: code.rd(),
                    base: code.rs1(),
                },
                0b100 => Inst::Lbu {
                    offset: code.imm_i(),
                    dest: code.rd(),
                    base: code.rs1(),
                },
                0b101 => Inst::Lhu {
                    offset: code.imm_i(),
                    dest: code.rd(),
                    base: code.rs1(),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // STORE
            0b0100011 => match code.funct3() {
                0b000 => Inst::Sb {
                    offset: code.imm_s(),
                    src: code.rs2(),
                    base: code.rs1(),
                },
                0b001 => Inst::Sh {
                    offset: code.imm_s(),
                    src: code.rs2(),
                    base: code.rs1(),
                },
                0b010 => Inst::Sw {
                    offset: code.imm_s(),
                    src: code.rs2(),
                    base: code.rs1(),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // OP-IMM
            0b0010011 => match code.funct3() {
                0b000 => Inst::Addi {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b010 => Inst::Slti {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b011 => Inst::Sltiu {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b100 => Inst::Xori {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b110 => Inst::Ori {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b111 => Inst::Andi {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b001 => {
                    if code.funct7() != 0 {
                        return Err(decode_error(code, "slli shift overflow"));
                    }
                    Inst::Slli {
                        imm: code.imm_i(),
                        dest: code.rd(),
                        src1: code.rs1(),
                    }
                }
                0b101 => match code.funct7() {
                    0b0000000 => Inst::Srli {
                        imm: code.rs2_imm(),
                        dest: code.rd(),
                        src1: code.rs1(),
                    },
                    0b0100000 => Inst::Srai {
                        imm: code.rs2_imm(),
                        dest: code.rd(),
                        src1: code.rs1(),
                    },
                    _ => return Err(decode_error(code, "funct7")),
                },
                _ => return Err(decode_error(code, "funct3")),
            },
            // OP
            0b0110011 => {
                let (dest, src1, src2) = (code.rd(), code.rs1(), code.rs2());
                match (code.funct3(), code.funct7()) {
                    (0b000, 0b0000000) => Inst::Add { dest, src1, src2 },
                    (0b000, 0b0100000) => Inst::Sub { dest, src1, src2 },
                    (0b001, 0b0000000) => Inst::Sll { dest, src1, src2 },
                    (0b010, 0b0000000) => Inst::Slt { dest, src1, src2 },
                    (0b011, 0b0000000) => Inst::Sltu { dest, src1, src2 },
                    (0b100, 0b0000000) => Inst::Xor { dest, src1, src2 },
                    (0b101, 0b0000000) => Inst::Srl { dest, src1, src2 },
                    (0b101, 0b0100000) => Inst::Sra { dest, src1, src2 },
                    (0b110, 0b0000000) => Inst::Or { dest, src1, src2 },
                    (0b111, 0b0000000) => Inst::And { dest, src1, src2 },

                    (0b000, 0b0000001) => Inst::Mul { dest, src1, src2 },
                    (0b001, 0b0000001) => Inst::Mulh { dest, src1, src2 },
                    (0b010, 0b0000001) => Inst::Mulhsu { dest, src1, src2 },
                    (0b011, 0b0000001) => Inst::Mulhu { dest, src1, src2 },
                    (0b100, 0b0000001) => Inst::Div { dest, src1, src2 },
                    (0b101, 0b0000001) => Inst::Divu { dest, src1, src2 },
                    (0b110, 0b0000001) => Inst::Rem { dest, src1, src2 },
                    (0b111, 0b0000001) => Inst::Remu { dest, src1, src2 },
                    _ => return Err(decode_error(code, "funct3/funct7")),
                }
            }
            // MISC-MEM
            0b0001111 => {
                let fm = code.extract(28..=31);
                let pred = FenceSet {
                    device_input: code.extract(27..=27) == 1,
                    device_output: code.extract(26..=26) == 1,
                    memory_read: code.extract(25..=25) == 1,
                    memory_write: code.extract(24..=24) == 1,
                };
                let succ = FenceSet {
                    device_input: code.extract(23..=23) == 1,
                    device_output: code.extract(22..=22) == 1,
                    memory_read: code.extract(21..=21) == 1,
                    memory_write: code.extract(20..=20) == 1,
                };

                match code.funct3() {
                    0b000 => Inst::Fence {
                        fence: Fence {
                            fm: fm as u8,
                            pred,
                            succ,
                            dest: code.rd(),
                            src: code.rs1(),
                        },
                    },
                    _ => return Err(decode_error(code, "funct3")),
                }
            }
            // SYSTEM
            0b1110011 => {
                if code.0 == 0b11000000000000000001000001110011 {
                    return Err(decode_error(code, "unimp instruction"));
                }
                if code.rd().0 != 0 {
                    return Err(decode_error(code, "rd"));
                }
                if code.funct3() != 0 {
                    return Err(decode_error(code, "funct3"));
                }
                if code.rs1().0 != 0 {
                    return Err(decode_error(code, "rs1"));
                }
                match code.imm_i() {
                    0b000000000000 => Inst::Ecall,
                    0b000000000001 => Inst::Ebreak,
                    _ => return Err(decode_error(code, "imm")),
                }
            }
            // AMO
            0b00101111 => {
                // width must be W
                if code.funct3() != 0b010 {
                    return Err(decode_error(code, "funct3"));
                }

                let kind = code.extract(27..=31);
                let aq = code.extract(26..=26) == 1;
                let rl = code.extract(25..=25) == 1;

                let order = AmoOrdering::from_aq_rl(aq, rl);

                match kind {
                    // LR
                    0b00010 => {
                        if code.rs2().0 != 0 {
                            return Err(decode_error(code, "rs2"));
                        }

                        Inst::LrW {
                            order,
                            dest: code.rd(),
                            addr: code.rs1(),
                        }
                    }
                    // SC
                    0b00011 => Inst::ScW {
                        order,
                        dest: code.rd(),
                        addr: code.rs1(),
                        src: code.rs2(),
                    },
                    _ => {
                        let op = match kind {
                            0b00001 => AmoOp::Swap,
                            0b00000 => AmoOp::Add,
                            0b00100 => AmoOp::Xor,
                            0b01100 => AmoOp::And,
                            0b01000 => AmoOp::Or,
                            0b10000 => AmoOp::Min,
                            0b10100 => AmoOp::Max,
                            0b11000 => AmoOp::Minu,
                            0b11100 => AmoOp::Maxu,
                            _ => return Err(decode_error(code, "funct7")),
                        };
                        Inst::AmoW {
                            order,
                            op,
                            dest: code.rd(),
                            addr: code.rs1(),
                            src: code.rs2(),
                        }
                    }
                }
            }
            _ => return Err(decode_error(code, "opcode")),
        };
        Ok(inst)
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use std::prelude::rust_2024::*;

    use std::fmt::Write as _;
    use std::io::Write as _;

    use object::Object;
    use object::ObjectSection;

    use crate::Fence;
    use crate::FenceSet;
    use crate::Inst;
    use crate::Reg;

    #[test]
    #[cfg_attr(not(slow_tests), ignore = "cfg(slow_tests) not enabled")]
    fn exhaustive_decode_no_panic() {
        for i in 0..u32::MAX {
            if (i % (2 << 25)) == 0 {
                let percent = i as f32 / (u32::MAX as f32);
                let done = (100.0 * percent) as usize;
                std::print!("\r{}{}", "#".repeat(done), "-".repeat(100 - done));
                std::io::stdout().flush().unwrap();
            }
            let _ = Inst::decode(i);
        }
        let _ = Inst::decode(u32::MAX);
    }

    #[test]
    fn size_of_instruction() {
        assert!(size_of::<Inst>() <= 12);
    }

    const TEST_SECTION_NAME: &str = ".text.rvdctest";

    /// Some instruction fields are reserved and not printed in the assembly,
    /// but should be ignored instead of rejected by the decoder.
    /// We filter out non-canonical forms of these instructions as they do not roundtrip.
    fn is_inst_supposed_to_roundtrip(inst: &Inst) -> bool {
        match inst {
            // Canonical fence.tso
            Inst::Fence {
                fence:
                    Fence {
                        fm: 0b1000,
                        pred:
                            FenceSet {
                                device_input: false,
                                device_output: false,
                                memory_read: true,
                                memory_write: true,
                            },
                        succ:
                            FenceSet {
                                device_input: false,
                                device_output: false,
                                memory_read: true,
                                memory_write: true,
                            },
                        src: Reg::ZERO,
                        dest: Reg::ZERO,
                    },
            } => true,
            // Only canonical normal fence with x0 and x0
            Inst::Fence {
                fence:
                    Fence {
                        fm: 0b0000,
                        pred: _,
                        succ: _,
                        src: Reg::ZERO,
                        dest: Reg::ZERO,
                    },
            } => true,
            // All other fences are reserved
            Inst::Fence { .. } => false,
            _ => true,
        }
    }

    // turn this up for debugging, only run the test directly in these cases
    const SKIP_CHUNKS: u32 = 0;

    #[test]
    fn ensure_no_chunks_are_skipped() {
        assert_eq!(SKIP_CHUNKS, 0);
    }

    #[test]
    #[cfg_attr(not(slow_tests), ignore = "cfg(slow_tests) not enabled")]
    fn normal_clang_roundtrip() {
        const CHUNKS: u32 = 128;
        const CHUNK_SIZE: u32 = u32::MAX / CHUNKS;

        for (chunk_idx, start) in ((SKIP_CHUNKS * CHUNK_SIZE)..u32::MAX)
            .step_by(CHUNK_SIZE as usize)
            .enumerate()
        {
            let chunk_idx = chunk_idx + SKIP_CHUNKS as usize;

            let start_time = std::time::Instant::now();

            let insts = (start..=(start + CHUNK_SIZE))
                .filter_map(|code| Some((code, Inst::decode_normal(code).ok()?)))
                .filter(|(_, inst)| is_inst_supposed_to_roundtrip(inst))
                .collect::<Vec<_>>();

            let mut text = std::format!(".section {TEST_SECTION_NAME}\n.globl _start\n_start:\n");
            for (_, inst) in &insts {
                writeln!(text, "  {inst}").unwrap();
            }

            let data = clang_assemble(&text, "-march=rv32ima_zihintpause");

            for (i, result_code) in data.chunks(4).enumerate() {
                let result_code = u32::from_le_bytes(result_code.try_into().unwrap());

                assert_eq!(
                    insts[i].0, result_code,
                    "failed to rountrip!\n\
                 instruction `{:0>32b}` failed to rountrip\n\
                 resulted in `{:0>32b}` instead.\n\
                 disassembly of original instruction: `{}`",
                    insts[i].0, result_code, insts[i].1
                );
            }

            let elapsed = start_time.elapsed();
            let chunk_number = chunk_idx + 1;
            let remaining = elapsed * (CHUNKS - (chunk_number as u32));

            writeln!(
                std::io::stdout(),
                "Completed chunk {chunk_number}/{CHUNKS} (estimated {remaining:?} remaining)",
            )
            .unwrap();
        }
    }

    #[test]
    #[ignore = "clang doesn't always generate compressed instructions?"]
    fn compressed_clang_roundtrip() {
        let insts = (0..=u16::MAX)
            .filter_map(|code| Some((code, Inst::decode_compressed(code).ok()?)))
            .collect::<Vec<_>>();

        let mut text = std::format!(".section {TEST_SECTION_NAME}\n.globl _start\n_start:\n");
        for (_, inst) in &insts {
            writeln!(text, "  {inst}").unwrap();
        }

        let data = clang_assemble(&text, "-march=rv32imac");

        for (i, result_code) in data.chunks(2).enumerate() {
            let result_code = u16::from_le_bytes(result_code.try_into().unwrap());

            assert_eq!(
                insts[i].0, result_code,
                "failed to rountrip!\n\
                 instruction `{:0>32b}` failed to rountrip\n\
                 resulted in `{:0>32b}` instead.\n\
                 disassembly of original instruction: `{}`",
                insts[i].0, result_code, insts[i].1
            );
        }
    }

    fn clang_assemble(text: &str, march_flag: &str) -> Vec<u8> {
        let tmp = tempfile::tempdir().unwrap();

        let path = tmp.path().join("16.s");
        let bin_path = tmp.path().join("16.o");
        std::fs::write(&path, text).unwrap();

        let mut clang = std::process::Command::new("clang");
        clang.args(["-target", "riscv32-unknown-none-elf", march_flag, "-c"]);
        clang.arg(path);
        clang.arg("-o");
        clang.arg(&bin_path);
        let out = clang.output().unwrap();
        if !out.status.success() {
            panic!(
                "failed to run clang:\n{}",
                String::from_utf8_lossy(&out.stderr)
            );
        }

        let obj = std::fs::read(bin_path).unwrap();
        let obj = object::File::parse(&*obj).unwrap();
        let section = obj.section_by_name(TEST_SECTION_NAME).unwrap();
        let data = section.data().unwrap();
        data.to_owned()
    }
}
