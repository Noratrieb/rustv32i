use crate::inst::{Inst, InstCode};
use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
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

impl Emulator {
    pub fn start_linux(&mut self) -> Error {
        // set top of stack. just some yolo address. with no values there. who needs abi?
        self[Reg::SP] = 4096;

        self.execute()
    }

    fn execute(&mut self) -> Error {
        loop {
            if let Err(err) = self.step() {
                return err;
            }
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
        let inst = Inst::decode(code)?;

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
            Inst::Fence { fence: _ } => { /* dont care */ }
            Inst::Ecall => {
                (self.ecall_handler)(&mut self.mem, &mut self.xreg)?;
            }
            Inst::Ebreak => return Err(Error::Ebreak),
            Inst::Mul { dest, src1, src2 } => {
                self[dest] = ((self[src1] as i32).wrapping_mul(self[src2] as i32)) as u32;
            }
            Inst::Mulh { dest, src1, src2 } => {
                let shifted = ((self[src1] as i64).wrapping_mul(self[src2] as i64) as i64) >> 32;
                self[dest] = shifted as u32;
            }
            Inst::Mulhsu { .. } => todo!("mulhsu"),
            Inst::Mulhu { dest, src1, src2 } => {
                let shifted = ((self[src1] as u64).wrapping_mul(self[src2] as u64) as u64) >> 32;
                self[dest] = shifted as u32;
            }
            Inst::Div { dest, src1, src2 } => {
                if self[src2] == 0 {
                    self[dest] = u32::MAX;
                } else if self[src2] == u32::MAX {
                    self[dest] = u32::MAX;
                } else {
                    self[dest] = ((self[src1] as i32) / (self[src2] as i32)) as u32;
                }
            }
            Inst::Divu { dest, src1, src2 } => {
                if self[src2] == 0 {
                    self[dest] = 2_u32.pow(32) - 1;
                } else {
                    self[dest] = self[src1] / self[src2];
                }
            }
            Inst::Rem { dest, src1, src2 } => {
                if self[src2] == 0 {
                    self[dest] = self[src1];
                } else if self[src2] == u32::MAX {
                    self[dest] = 0;
                } else {
                    self[dest] = ((self[src1] as i32) % (self[src2] as i32)) as u32;
                }
            }
            Inst::Remu { dest, src1, src2 } => {
                if self[src2] == 0 {
                    self[dest] = 2_u32.pow(32) - 1;
                } else {
                    self[dest] = self[src1] % self[src2];
                }
            }
        }

        if !jumped {
            self.set_pc(self.pc + 4)?;
        }
        Ok(())
    }
}
