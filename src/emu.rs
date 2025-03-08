use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut, RangeInclusive},
    u32,
};

pub struct Memory {
    pub mem: Vec<u8>,
}

impl Memory {
    fn check_align(&self, addr: u32, align: u32) -> Result<(), Error> {
        if addr % 2 != 0 {
            Err(Error::UnaligneMemoryAccess {
                addr,
                required_align: align,
            })
        } else {
            Ok(())
        }
    }
    pub fn slice(&self, addr: u32, len: u32) -> Result<&[u8], Error> {
        Ok(self
            .mem
            .get((addr as usize)..)
            .ok_or(Error::InvalidMemoryAccess(addr))?
            .get(..(len as usize))
            .ok_or(Error::InvalidMemoryAccess(addr))?)
    }
    pub fn slice_mut(&mut self, addr: u32, len: u32) -> Result<&mut [u8], Error> {
        Ok(self
            .mem
            .get_mut((addr as usize)..)
            .ok_or(Error::InvalidMemoryAccess(addr))?
            .get_mut(..(len as usize))
            .ok_or(Error::InvalidMemoryAccess(addr))?)
    }

    pub fn load_u8(&self, addr: u32) -> Result<u8, Error> {
        Ok(u8::from_le_bytes(self.slice(addr, 1)?.try_into().unwrap()))
    }
    pub fn load_u16(&self, addr: u32) -> Result<u16, Error> {
        Ok(u16::from_le_bytes(self.slice(addr, 2)?.try_into().unwrap()))
    }
    pub fn load_u32(&self, addr: u32) -> Result<u32, Error> {
        Ok(u32::from_le_bytes(self.slice(addr, 4)?.try_into().unwrap()))
    }
    pub fn store_u8(&mut self, addr: u32, value: u8) -> Result<(), Error> {
        Ok(self
            .slice_mut(addr, 1)?
            .copy_from_slice(&value.to_le_bytes()))
    }
    pub fn store_u16(&mut self, addr: u32, value: u16) -> Result<(), Error> {
        self.check_align(addr, 2)?;
        Ok(self
            .slice_mut(addr, 2)?
            .copy_from_slice(&value.to_le_bytes()))
    }
    pub fn store_u32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        self.check_align(addr, 4)?;
        Ok(self
            .slice_mut(addr, 4)?
            .copy_from_slice(&value.to_le_bytes()))
    }
}

#[derive(Debug)]
#[expect(dead_code)]
pub enum Error {
    Trap(&'static str),
    IllegalInstruction(InstCode, &'static str),
    InvalidMemoryAccess(u32),
    UnalignedPc(u32),
    UnaligneMemoryAccess { addr: u32, required_align: u32 },
    Ebreak,
    Exit { code: i32 },
}

pub struct Emulator {
    pub mem: Memory,
    pub xreg: [u32; 32],
    /// Written to insterad of xreg[0].
    pub xreg0_value: u32,
    pub pc: u32,

    pub debug: bool,

    pub ecall_handler: Box<dyn FnMut(&mut Memory, &mut [u32; 32]) -> Result<(), Error>>,
}

impl Index<Reg> for Emulator {
    type Output = u32;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.xreg[index.0 as usize]
    }
}
impl IndexMut<Reg> for Emulator {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        if index.0 == 0 {
            &mut self.xreg0_value
        } else {
            &mut self.xreg[index.0 as usize]
        }
    }
}

#[derive(Clone, Copy)]
pub struct Reg(pub u32);

#[expect(dead_code)]
impl Reg {
    pub const RA: Reg = Reg(1);
    pub const SP: Reg = Reg(2);
    // arguments, return values:
    pub const A0: Reg = Reg(10);
    pub const A1: Reg = Reg(11);
    // arguments:
    pub const A2: Reg = Reg(12);
    pub const A3: Reg = Reg(13);
    pub const A4: Reg = Reg(14);
    pub const A5: Reg = Reg(15);
    pub const A6: Reg = Reg(16);
    pub const A7: Reg = Reg(17);
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == Self::SP.0 {
            write!(f, "sp")
        } else if self.0 == Self::RA.0 {
            write!(f, "ra")
        } else {
            write!(f, "x{}", self.0)
        }
    }
}

enum Inst {
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

    Ecall,
    Ebreak,
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
            Inst::Ecall => write!(f, "ecall"),
            Inst::Ebreak => write!(f, "ebreak"),
        }
    }
}

fn sign_extend(value: u32, size: u32) -> u32 {
    assert!(size <= u32::BITS);
    let sign = value >> (size - 1);
    let imm = if sign == 1 {
        (u32::MAX << size) | value
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
        let imm = self.extract(20..=31);
        sign_extend(imm, 12)
    }
    fn imm_s(self) -> u32 {
        let imm_11_5 = self.extract(25..=31);
        let imm_4_0 = self.extract(7..=11);
        let imm = (imm_11_5 << 5) | imm_4_0;
        sign_extend(imm, 12)
    }
    fn imm_b(self) -> u32 {
        let imm_12 = self.extract(31..=31);
        let imm_10_5 = self.extract(25..=30);
        let imm_4_1 = self.extract(8..=11);
        let imm_11 = self.extract(7..=7);
        let imm = (imm_12 << 12) | (imm_11 << 11) | (imm_10_5 << 5) | (imm_4_1 << 1);
        sign_extend(imm, 13) // 13 due to 2-byte immediate offset
    }
    fn imm_u(self) -> u32 {
        let imm_12_31 = self.extract(12..=31);
        imm_12_31 << 12
    }
    fn imm_j(self) -> u32 {
        let imm_20 = self.extract(31..=31);
        let imm_10_1 = self.extract(21..=30);
        let imm_11 = self.extract(20..=20);
        let imm_19_12 = self.extract(12..=19);
        let imm = (imm_20 << 19) | (imm_19_12 << 11) | (imm_11 << 10) | imm_10_1;
        sign_extend(imm, 20) << 1
    }
}

impl Debug for InstCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>32b}", self.0)
    }
}

impl Inst {
    fn decode(code: InstCode) -> Result<Inst, Error> {
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
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
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
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
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
                    _ => return Err(Error::IllegalInstruction(code, "funct7")),
                },
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
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
                    _ => return Err(Error::IllegalInstruction(code, "funct3/funct7")),
                }
            }
            // SYSTEM
            0b1110011 => {
                if code.0 == 0b11000000000000000001000001110011 {
                    return Err(Error::Trap("unimp instruction"));
                }
                if code.rd().0 != 0 {
                    return Err(Error::IllegalInstruction(code, "rd"));
                }
                if code.funct3() != 0 {
                    return Err(Error::IllegalInstruction(code, "funct3"));
                }
                if code.rs1().0 != 0 {
                    return Err(Error::IllegalInstruction(code, "rs1"));
                }
                match code.imm_i() {
                    0b000000000000 => Inst::Ecall,
                    0b000000000001 => Inst::Ebreak,
                    _ => return Err(Error::IllegalInstruction(code, "imm")),
                }
            }
            _ => return Err(Error::IllegalInstruction(code, "opcode")),
        };
        Ok(inst)
    }
}

impl Emulator {
    pub fn start_linux(&mut self) -> Result<(), Error> {
        // set top of stack. just some yolo address. with no values there. who needs abi?
        self[Reg::SP] = 4096;

        match self.execute() {
            Ok(()) => {}
            Err(Error::Exit { code }) => {
                eprintln!("exited with code {code}");
            }
            Err(Error::Trap(cause)) => eprintln!("program trapped: {cause}"),
            Err(e) => return Err(e),
        }
        Ok(())
    }

    fn execute(&mut self) -> Result<(), Error> {
        loop {
            self.step()?;
        }
    }

    fn set_pc(&mut self, pc: u32) -> Result<(), Error> {
        if pc % 4 != 0 {
            return Err(Error::UnalignedPc(pc));
        }
        self.pc = pc;
        Ok(())
    }

    fn step(&mut self) -> Result<(), Error> {
        let code = self.mem.load_u32(self.pc)?;
        let inst = Inst::decode(InstCode(code))?;

        if self.debug {
            println!("executing 0x{:x} {inst:?}", self.pc);
        }

        let mut jumped = false;

        match inst {
            Inst::Lui { uimm, dest } => self[dest] = uimm,
            Inst::Auipc { uimm, dest } => self[dest] = self.pc.wrapping_add(uimm),
            Inst::Jal { offset, dest } => {
                let target = self.pc.wrapping_add(offset);
                self[dest] = self.pc.wrapping_add(4);
                self.set_pc(target)?;
                jumped = true;
            }
            Inst::Jalr { offset, base, dest } => {
                let target = self[base].wrapping_add(offset) & !1;
                self[dest] = self.pc.wrapping_add(4);
                self.set_pc(target)?;
                jumped = true;
            }
            Inst::Beq { offset, src1, src2 } => {
                let take = self[src1] == self[src2];
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bne { offset, src1, src2 } => {
                let take = self[src1] != self[src2];
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Blt { offset, src1, src2 } => {
                let take = (self[src1] as i32) < (self[src2] as i32);
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bge { offset, src1, src2 } => {
                let take = (self[src1] as i32) >= (self[src2] as i32);
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bltu { offset, src1, src2 } => {
                let take = self[src1] < self[src2];
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bgeu { offset, src1, src2 } => {
                let take = self[src1] >= self[src2];
                if take {
                    let target = self.pc.wrapping_add(offset);
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Lb { offset, dest, base } => {
                let addr = self[base].wrapping_add(offset);
                self[dest] = self.mem.load_u8(addr)? as i8 as i32 as u32;
            }
            Inst::Lbu { offset, dest, base } => {
                let addr = self[base].wrapping_add(offset);
                self[dest] = self.mem.load_u8(addr)? as u32;
            }
            Inst::Lh { offset, dest, base } => {
                let addr = self[base].wrapping_add(offset);
                self[dest] = self.mem.load_u16(addr)? as i16 as i32 as u32;
            }
            Inst::Lhu { offset, dest, base } => {
                let addr = self[base].wrapping_add(offset);
                self[dest] = self.mem.load_u8(addr)? as u32;
            }
            Inst::Lw { offset, dest, base } => {
                let addr = self[base].wrapping_add(offset);
                self[dest] = self.mem.load_u32(addr)?;
            }
            Inst::Sb { offset, src, base } => {
                let addr = self[base].wrapping_add(offset);
                self.mem.store_u8(addr, self[src] as u8)?;
            }
            Inst::Sh { offset, src, base } => {
                let addr = self[base].wrapping_add(offset);
                self.mem.store_u16(addr, self[src] as u16)?;
            }
            Inst::Sw { offset, src, base } => {
                let addr = self[base].wrapping_add(offset);
                self.mem.store_u32(addr, self[src] as u32)?;
            }
            Inst::Addi { imm, dest, src1 } => {
                self[dest] = self[src1].wrapping_add(imm as u32);
            }
            Inst::Slti { imm, dest, src1 } => {
                let result = (self[src1] as i32) < (imm as i32);
                self[dest] = result as u32;
            }
            Inst::Sltiu { imm, dest, src1 } => {
                let result = (self[src1] as u32) < imm as u32;
                self[dest] = result as u32;
            }
            Inst::Andi { imm, dest, src1 } => {
                self[dest] = self[src1] & imm;
            }
            Inst::Ori { imm, dest, src1 } => {
                self[dest] = self[src1] | imm;
            }
            Inst::Xori { imm, dest, src1 } => {
                self[dest] = self[src1] ^ imm;
            }
            Inst::Slli { imm, dest, src1 } => self[dest] = self[src1].wrapping_shl(imm),
            Inst::Srli { imm, dest, src1 } => self[dest] = self[src1].wrapping_shr(imm),
            Inst::Srai { imm, dest, src1 } => {
                self[dest] = (self[src1] as i32).wrapping_shr(imm) as u32
            }
            Inst::Add { dest, src1, src2 } => self[dest] = self[src1].wrapping_add(self[src2]),
            Inst::Sub { dest, src1, src2 } => self[dest] = self[src1].wrapping_sub(self[src2]),
            Inst::Sll { dest, src1, src2 } => self[dest] = self[src1].wrapping_shl(self[src2]),
            Inst::Slt { dest, src1, src2 } => {
                self[dest] = ((self[src1] as i32) < (self[src2] as i32)) as u32;
            }
            Inst::Sltu { dest, src1, src2 } => {
                self[dest] = (self[src1] < self[src2]) as u32;
            }
            Inst::Xor { dest, src1, src2 } => self[dest] = self[src1] ^ self[src2],
            Inst::Srl { dest, src1, src2 } => self[dest] = self[src1].wrapping_shr(self[src2]),
            Inst::Sra { dest, src1, src2 } => {
                self[dest] = (self[src1] as i32).wrapping_shr(self[src2]) as u32
            }
            Inst::Or { dest, src1, src2 } => self[dest] = self[src1] | self[src2],
            Inst::And { dest, src1, src2 } => self[dest] = self[src1] & self[src2],
            Inst::Ecall => {
                (self.ecall_handler)(&mut self.mem, &mut self.xreg)?;
            }
            Inst::Ebreak => return Err(Error::Ebreak),
        }

        if !jumped {
            self.set_pc(self.pc + 4)?;
        }
        Ok(())
    }
}
