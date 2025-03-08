use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut, RangeInclusive},
    u32,
};

use aligned_vec::{AVec, ConstAlign};

use crate::PAGE_SIZE;

pub struct Memory {
    pub mem: AVec<u8, ConstAlign<PAGE_SIZE>>,
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
    fn index(&self, addr: u32, len: u32) -> Result<&[u8], Error> {
        Ok(self
            .mem
            .get((addr as usize)..)
            .ok_or(Error::InvalidMemoryAccess(addr))?
            .get(..(len as usize))
            .ok_or(Error::InvalidMemoryAccess(addr))?)
    }
    fn index_mut(&mut self, addr: u32, len: u32) -> Result<&mut [u8], Error> {
        Ok(self
            .mem
            .get_mut((addr as usize)..)
            .ok_or(Error::InvalidMemoryAccess(addr))?
            .get_mut(..(len as usize))
            .ok_or(Error::InvalidMemoryAccess(addr))?)
    }

    fn load_u8(&self, addr: u32) -> Result<u8, Error> {
        Ok(u8::from_le_bytes(self.index(addr, 1)?.try_into().unwrap()))
    }
    fn load_u16(&self, addr: u32) -> Result<u16, Error> {
        Ok(u16::from_le_bytes(self.index(addr, 2)?.try_into().unwrap()))
    }
    fn load_u32(&self, addr: u32) -> Result<u32, Error> {
        Ok(u32::from_le_bytes(self.index(addr, 4)?.try_into().unwrap()))
    }
    fn store_u8(&mut self, addr: u32, value: u8) -> Result<(), Error> {
        Ok(self
            .index_mut(addr, 1)?
            .copy_from_slice(&value.to_le_bytes()))
    }
    fn store_u16(&mut self, addr: u32, value: u16) -> Result<(), Error> {
        self.check_align(addr, 2)?;
        Ok(self
            .index_mut(addr, 2)?
            .copy_from_slice(&value.to_le_bytes()))
    }
    fn store_u32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        self.check_align(addr, 4)?;
        Ok(self
            .index_mut(addr, 4)?
            .copy_from_slice(&value.to_le_bytes()))
    }
}

#[derive(Debug)]
#[expect(dead_code)]
pub enum Error {
    IllegalInstruction(InstCode, &'static str),
    InvalidMemoryAccess(u32),
    UnalignedPc(u32),
    UnaligneMemoryAccess { addr: u32, required_align: u32 },
}

pub struct Emulator {
    pub mem: Memory,
    pub xreg: [u32; 32],
    /// Written to insterad of xreg[0].
    pub xreg0_value: u32,
    pub pc: u32,
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

struct Reg(u32);

impl Reg {
    const RA: Reg = Reg(1);
    const SP: Reg = Reg(2);
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
    Jal { offset: u32, dest: Reg },
    Jalr { offset: u32, base: Reg, dest: Reg },

    Lb { offset: u32, dest: Reg, base: Reg },
    Lbu { offset: u32, dest: Reg, base: Reg },
    Lh { offset: u32, dest: Reg, base: Reg },
    Lhu { offset: u32, dest: Reg, base: Reg },
    Lw { offset: u32, dest: Reg, base: Reg },

    Sb { offset: u32, src: Reg, base: Reg },
    Sh { offset: u32, src: Reg, base: Reg },
    Sw { offset: u32, src: Reg, base: Reg },

    Addi { imm: u32, rd: Reg, rs1: Reg },
    Slti { imm: u32, rd: Reg, rs1: Reg },
    Sltiu { imm: u32, rd: Reg, rs1: Reg },
    Andi { imm: u32, rd: Reg, rs1: Reg },
    Ori { imm: u32, rd: Reg, rs1: Reg },
    Xori { imm: u32, rd: Reg, rs1: Reg },
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
    fn imm_j(self) -> u32 {
        let imm_20 = self.extract(31..=31);
        let imm_10_1 = self.extract(21..=30);
        let imm_19_12 = self.extract(12..=19);
        let imm = (imm_20 << 19) | (imm_19_12 << 11) | imm_10_1;
        sign_extend(imm, 20)
    }
    fn imm_s(self) -> u32 {
        let imm_11_5 = self.extract(25..=31);
        let imm_4_0 = self.extract(7..=11);
        let full_imm = (imm_11_5 << 5) | imm_4_0;
        sign_extend(full_imm, 12)
    }
}

impl Debug for InstCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>32b}", self.0)
    }
}

impl Inst {
    fn decode(code: InstCode) -> Result<Inst, Error> {
        eprintln!("decoding: {code:?}");
        let inst = match code.opcode() {
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
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                0b010 => Inst::Slti {
                    imm: code.imm_i(),
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                0b011 => Inst::Sltiu {
                    imm: code.imm_i(),
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                0b100 => Inst::Xori {
                    imm: code.imm_i(),
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                0b110 => Inst::Ori {
                    imm: code.imm_i(),
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                0b111 => Inst::Andi {
                    imm: code.imm_i(),
                    rd: code.rd(),
                    rs1: code.rs1(),
                },
                _ => return Err(Error::IllegalInstruction(code, "funct3")),
            },
            _ => return Err(Error::IllegalInstruction(code, "opcode")),
        };
        eprintln!("  parsed {inst:?}");
        Ok(inst)
    }
}

impl Debug for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}
impl Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Inst::Jal { offset, dest } => write!(f, "jal {dest}, {offset}"),
            Inst::Jalr { offset, base, dest } => write!(f, "jalr {dest}, {offset}({base})"),
            Inst::Lb { offset, dest, base } => write!(f, "lb {dest}, {offset}({base})"),
            Inst::Lbu { offset, dest, base } => write!(f, "lbu {dest}, {offset}({base})"),
            Inst::Lh { offset, dest, base } => write!(f, "lh {dest}, {offset}({base})"),
            Inst::Lhu { offset, dest, base } => write!(f, "lhu {dest}, {offset}({base})"),
            Inst::Lw { offset, dest, base } => write!(f, "lw {dest}, {offset}({base})"),
            Inst::Sb { offset, src, base } => write!(f, "sb {src}, {offset}({base})"),
            Inst::Sh { offset, src, base } => write!(f, "sh {src}, {offset}({base})"),
            Inst::Sw { offset, src, base } => write!(f, "sw {src}, {offset}({base})"),
            Inst::Addi { imm, rd, rs1 } => write!(f, "addi {rd}, {rs1}, {}", *imm as i32),
            Inst::Slti { imm, rd, rs1 } => write!(f, "slti {rd}, {rs1}, {}", *imm as i32),
            Inst::Sltiu { imm, rd, rs1 } => write!(f, "sltiu {rd}, {rs1}, {}", *imm as i32),
            Inst::Andi { imm, rd, rs1 } => write!(f, "andi {rd}, {rs1}, {}", *imm as i32),
            Inst::Ori { imm, rd, rs1 } => write!(f, "ori {rd}, {rs1}, {}", *imm as i32),
            Inst::Xori { imm, rd, rs1 } => write!(f, "xori {rd}, {rs1}, {}", *imm as i32),
        }
    }
}

impl Emulator {
    pub fn start_linux(&mut self) -> Result<(), Error> {
        // set top of stack. just some yolo address. with no values there. who needs abi?
        self[Reg::SP] = 4096;

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
        let instruction = self.mem.load_u32(self.pc)?;

        let instruction = Inst::decode(InstCode(instruction))?;

        match instruction {
            Inst::Jal { offset, dest } => {
                let target = self.pc.wrapping_add(offset);
                self[dest] = self.pc.wrapping_add(4);
                self.set_pc(target)?;
            }
            Inst::Jalr { offset, base, dest } => {
                let target = self[base].wrapping_add(offset.wrapping_mul(2)) & !1;
                self[dest] = self.pc.wrapping_add(4);
                self.set_pc(target)?;
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
            Inst::Addi { imm, rd, rs1 } => {
                self[rd] = self[rs1].wrapping_add(imm as u32);
            }
            Inst::Slti { imm, rd, rs1 } => {
                let result = (self[rs1] as i32) < (imm as i32);
                self[rd] = result as u32;
            }
            Inst::Sltiu { imm, rd, rs1 } => {
                let result = (self[rs1] as u32) < imm as u32;
                self[rd] = result as u32;
            }
            Inst::Andi { imm, rd, rs1 } => {
                self[rd] = self[rs1] & imm;
            }
            Inst::Ori { imm, rd, rs1 } => {
                self[rd] = self[rs1] | imm;
            }
            Inst::Xori { imm, rd, rs1 } => {
                self[rd] = self[rs1] ^ imm;
            }
        }

        self.set_pc(self.pc + 4)?;
        Ok(())
    }
}
