use crate::emu::{Reg, Status};
use std::fmt::{Debug, Display};
use std::ops::RangeInclusive;

#[derive(Clone, Copy)]
#[rustfmt::skip]
pub enum Inst {
    Lui { uimm: u32, dest: Reg },
    Auipc { uimm: u32, dest: Reg },

    Jal { offset: u32, dest: Reg },
    Jalr { offset: u32, base: Reg, dest: Reg },

    Beq { offset: u32, src1: Reg, src2: Reg },
    Bne { offset: u32, src1: Reg, src2: Reg },
    Blt { offset: u32, src1: Reg, src2: Reg },
    Bge { offset: u32, src1: Reg, src2: Reg },
    Bltu { offset: u32, src1: Reg, src2: Reg },
    Bgeu { offset: u32, src1: Reg, src2: Reg },

    Lb { offset: u32, dest: Reg, base: Reg },
    Lbu { offset: u32, dest: Reg, base: Reg },
    Lh { offset: u32, dest: Reg, base: Reg },
    Lhu { offset: u32, dest: Reg, base: Reg },
    Lw { offset: u32, dest: Reg, base: Reg },

    Sb { offset: u32, src: Reg, base: Reg },
    Sh { offset: u32, src: Reg, base: Reg },
    Sw { offset: u32, src: Reg, base: Reg },

    Addi { imm: u32, dest: Reg, src1: Reg },
    Slti { imm: u32, dest: Reg, src1: Reg },
    Sltiu { imm: u32, dest: Reg, src1: Reg },
    Xori { imm: u32, dest: Reg, src1: Reg },
    Ori { imm: u32, dest: Reg, src1: Reg },
    Andi { imm: u32, dest: Reg, src1: Reg },
    Slli { imm: u32, dest: Reg, src1: Reg },
    Srli { imm: u32, dest: Reg, src1: Reg },
    Srai { imm: u32, dest: Reg, src1: Reg },

    Add { dest: Reg, src1: Reg, src2: Reg },
    Sub { dest: Reg, src1: Reg, src2: Reg },
    Sll { dest: Reg, src1: Reg, src2: Reg },
    Slt { dest: Reg, src1: Reg, src2: Reg },
    Sltu { dest: Reg, src1: Reg, src2: Reg },
    Xor { dest: Reg, src1: Reg, src2: Reg },
    Srl { dest: Reg, src1: Reg, src2: Reg },
    Sra { dest: Reg, src1: Reg, src2: Reg },
    Or { dest: Reg, src1: Reg, src2: Reg },
    And { dest: Reg, src1: Reg, src2: Reg },

    Fence { fence: Fence },

    Ecall,
    Ebreak,

    // M
    Mul { dest: Reg, src1: Reg, src2: Reg },
    Mulh { dest: Reg, src1: Reg, src2: Reg },
    Mulhsu { dest: Reg, src1: Reg, src2: Reg },
    Mulhu { dest: Reg, src1: Reg, src2: Reg },
    Div { dest: Reg, src1: Reg, src2: Reg },
    Divu { dest: Reg, src1: Reg, src2: Reg },
    Rem { dest: Reg, src1: Reg, src2: Reg },
    Remu { dest: Reg, src1: Reg, src2: Reg },

    // A
    LrW {
        order: AmoOrdering,
        dest: Reg,
        addr: Reg,  
    },
    ScW {
        order: AmoOrdering,
        dest: Reg,
        addr: Reg,
        src: Reg,
    },
    AmoW {
        order: AmoOrdering,
        op: AmoOp,
        dest: Reg,
        addr: Reg,
        src: Reg,
    },
}

#[derive(Clone, Copy)]
pub struct Fence {
    pub fm: u32,
    pub pred: FenceSet,
    pub succ: FenceSet,
    pub dest: Reg,
    pub src: Reg,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FenceSet {
    pub device_input: bool,
    pub device_output: bool,
    pub memory_read: bool,
    pub memory_write: bool,
}

#[derive(Clone, Copy)]
pub enum AmoOrdering {
    Relaxed,
    Acquire,
    Release,
    SeqCst,
}

#[derive(Clone, Copy)]
pub enum AmoOp {
    Swap,
    Add,
    Xor,
    And,
    Or,
    Min,
    Max,
    Minu,
    Maxu,
}

impl Fence {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                write!(f, "jalr {dest}, {}({base})", offset as i32)
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
            Inst::Addi {
                imm,
                dest: rd,
                src1: rs1,
            } => {
                if rs1.0 == 0 {
                    write!(f, "li {rd}, {}", imm as i32)
                } else if imm == 0 {
                    write!(f, "mv {rd}, {rs1}")
                } else {
                    write!(f, "addi {rd}, {rs1}, {}", imm as i32)
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
            Inst::Add { dest, src1, src2 } => write!(f, "add {dest}, {src1}, {src2}"),
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
                0b1000 => write!(f, "fence.TSO"),
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
            } => write!(f, "am{op}.w{order} {dest}, {src}, ({addr})",),
        }
    }
}

impl Display for FenceSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.device_input {
            write!(f, "i")?;
        }
        if self.device_output {
            write!(f, "o")?;
        }
        if self.memory_read {
            write!(f, "r")?;
        }
        if self.memory_write {
            write!(f, "w")?;
        }
        Ok(())
    }
}

impl Display for AmoOrdering {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AmoOrdering::Relaxed => write!(f, ""),
            AmoOrdering::Acquire => write!(f, ".aq"),
            AmoOrdering::Release => write!(f, ".rl"),
            AmoOrdering::SeqCst => write!(f, ".aqrl"),
        }
    }
}

impl Display for AmoOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

fn sign_extend32(value: u32, size: u32) -> u32 {
    assert!(size <= u32::BITS);
    let sign = value >> (size - 1);
    let imm = if sign == 1 {
        (u32::MAX << size) | value
    } else {
        value
    };
    imm
}
fn sign_extend16(value: u16, size: u16) -> u16 {
    assert!(size <= u16::BITS as u16);
    let sign = value >> (size - 1);
    let imm = if sign == 1 {
        (u16::MAX << size) | value
    } else {
        value
    };
    imm
}

#[derive(Clone, Copy)]
pub struct InstCode(u32);

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
        sign_extend32(imm, size)
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
        Reg(self.extract(15..=19))
    }
    fn rs2(self) -> Reg {
        Reg(self.extract(20..=24))
    }
    fn rd(self) -> Reg {
        Reg(self.extract(7..=11))
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
        self.immediate_u(&[(31..=31, 20), (21..=30, 1), (20..=20, 11), (12..=19, 12)])
    }
}

impl Debug for InstCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>32b}", self.0)
    }
}

#[derive(Clone, Copy)]
pub struct InstCodeC(u16);

impl InstCodeC {
    fn extract(self, range: RangeInclusive<u16>) -> u16 {
        let end_span = 32 - (range.end() + 1);
        (self.0 << (end_span)) >> (end_span + range.start())
    }
    fn immediate_u(self, mappings: &[(RangeInclusive<u16>, u16)]) -> u16 {
        let mut imm = 0;
        for (from, to) in mappings {
            let value = self.extract(from.clone());
            imm |= value << to;
        }
        imm
    }

    fn immediate_s(self, mappings: &[(RangeInclusive<u16>, u16)]) -> u16 {
        let mut imm = 0;
        let mut size = 0;
        for (from, to) in mappings {
            let value = self.extract(from.clone());
            imm |= value << to;
            let this_size = from.end() - from.start() + 1;
            size = size.max(*to + this_size);
        }
        sign_extend16(imm, size)
    }
    fn op(self) -> u16 {
        self.0 & 0b11
    }
    fn funct3(self) -> u16 {
        self.extract(13..=15)
    }
    fn rd(self) -> Reg {
        Reg(self.extract(7..=11) as u32)
    }
}

impl From<InstCodeC> for InstCode {
    fn from(value: InstCodeC) -> Self {
        Self(value.0 as u32)
    }
}

impl Inst {
    pub fn decode(code: u32) -> Result<Inst, Status> {
        let is_compressed = (code & 0b11) != 0b11;
        if is_compressed {
            Self::decode_compressed(code as u16)
        } else {
            Self::decode_normal(code)
        }
    }

    fn decode_compressed(code: u16) -> Result<Inst, Status> {
        let code = InstCodeC(code);

        let inst = match code.op() {
            // C0
            0b00 => todo!(),
            // C1
            0b01 => todo!(),
            // C2
            0b10 => {
                match code.funct3() {
                    // C.LDSP
                    010 => {
                        let dest = code.rd();
                        if dest.0 == 0 {
                            return Err(Status::IllegalInstruction(code.into(), "rd"));
                        }
                        let imm = code.immediate_s(&[(12..=12, 5), (4..=6, 2), (2..=3, 6)]);

                        Inst::Lw {
                            offset: imm as u32,
                            dest,
                            base: Reg::SP,
                        }
                    }
                    _ => return Err(Status::IllegalInstruction(code.into(), "funct3")),
                }
            }
            _ => return Err(Status::IllegalInstruction(code.into(), "op")),
        };
        Ok(inst)
    }

    fn decode_normal(code: u32) -> Result<Inst, Status> {
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
                _ => return Err(Status::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Status::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Status::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Status::IllegalInstruction(code, "funct3")),
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
                0b001 => Inst::Slli {
                    imm: code.imm_i(),
                    dest: code.rd(),
                    src1: code.rs1(),
                },
                0b101 => match code.funct7() {
                    0b0000000 => Inst::Srli {
                        imm: code.rs2().0,
                        dest: code.rd(),
                        src1: code.rs1(),
                    },
                    0b0100000 => Inst::Srai {
                        imm: code.rs2().0,
                        dest: code.rd(),
                        src1: code.rs1(),
                    },
                    _ => return Err(Status::IllegalInstruction(code, "funct7")),
                },
                _ => return Err(Status::IllegalInstruction(code, "funct3")),
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
                    _ => return Err(Status::IllegalInstruction(code, "funct3/funct7")),
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
                            fm,
                            pred,
                            succ,
                            dest: code.rd(),
                            src: code.rs1(),
                        },
                    },
                    _ => return Err(Status::IllegalInstruction(code, "funct3")),
                }
            }
            // SYSTEM
            0b1110011 => {
                if code.0 == 0b11000000000000000001000001110011 {
                    return Err(Status::Trap("unimp instruction"));
                }
                if code.rd().0 != 0 {
                    return Err(Status::IllegalInstruction(code, "rd"));
                }
                if code.funct3() != 0 {
                    return Err(Status::IllegalInstruction(code, "funct3"));
                }
                if code.rs1().0 != 0 {
                    return Err(Status::IllegalInstruction(code, "rs1"));
                }
                match code.imm_i() {
                    0b000000000000 => Inst::Ecall,
                    0b000000000001 => Inst::Ebreak,
                    _ => return Err(Status::IllegalInstruction(code, "imm")),
                }
            }
            // AMO
            0b00101111 => {
                // width must be W
                if code.funct3() != 0b010 {
                    return Err(Status::IllegalInstruction(code, "funct3"));
                }

                let kind = code.extract(27..=31);
                let aq = code.extract(26..=26) == 1;
                let rl = code.extract(25..=25) == 1;

                let order = AmoOrdering::from_aq_rl(aq, rl);

                match kind {
                    // LR
                    0b00010 => {
                        if code.rs2().0 != 0 {
                            return Err(Status::IllegalInstruction(code, "rs2"));
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
                            _ => return Err(Status::IllegalInstruction(code, "funct7")),
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
            _ => return Err(Status::IllegalInstruction(code, "opcode")),
        };
        Ok(inst)
    }
}
