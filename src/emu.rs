use crate::inst::{AmoOp, DecodeError, Inst, IsCompressed};
use std::{
    fmt::{Debug, Display},
    io::Write,
    ops::{Index, IndexMut},
};

pub struct Memory {
    pub mem: Vec<u8>,
}

impl Memory {
    fn check_align<XLEN: XLen>(&self, addr: XLEN, align: u32) -> Result<(), Status> {
        if addr.as_usize() % 2 != 0 {
            Err(Status::UnaligneMemoryAccess {
                addr: addr.as_usize(),
                required_align: align,
            })
        } else {
            Ok(())
        }
    }
    pub fn slice<XLEN: XLen>(&self, addr: XLEN, len: u32) -> Result<&[u8], Status> {
        self.mem
            .get((addr.as_usize())..)
            .ok_or(Status::InvalidMemoryAccess(addr.as_usize()))?
            .get(..(len as usize))
            .ok_or(Status::InvalidMemoryAccess(addr.as_usize()))
    }
    pub fn slice_mut<XLEN: XLen>(&mut self, addr: XLEN, len: u32) -> Result<&mut [u8], Status> {
        self.mem
            .get_mut((addr.as_usize())..)
            .ok_or(Status::InvalidMemoryAccess(addr.as_usize()))?
            .get_mut(..(len as usize))
            .ok_or(Status::InvalidMemoryAccess(addr.as_usize()))
    }

    pub fn load_u8<XLEN: XLen>(&self, addr: XLEN) -> Result<u8, Status> {
        Ok(u8::from_le_bytes(self.slice(addr, 1)?.try_into().unwrap()))
    }
    pub fn load_u16<XLEN: XLen>(&self, addr: XLEN) -> Result<u16, Status> {
        Ok(u16::from_le_bytes(self.slice(addr, 2)?.try_into().unwrap()))
    }
    pub fn load_u32<XLEN: XLen>(&self, addr: XLEN) -> Result<u32, Status> {
        Ok(u32::from_le_bytes(self.slice(addr, 4)?.try_into().unwrap()))
    }
    pub fn store_u8<XLEN: XLen>(&mut self, addr: XLEN, value: u8) -> Result<(), Status> {
        self.slice_mut(addr, 1)?
            .copy_from_slice(&value.to_le_bytes());
        Ok(())
    }
    pub fn store_u16<XLEN: XLen>(&mut self, addr: XLEN, value: u16) -> Result<(), Status> {
        self.check_align(addr, 2)?;
        self.slice_mut(addr, 2)?
            .copy_from_slice(&value.to_le_bytes());
        Ok(())
    }
    pub fn store_u32<XLEN: XLen>(&mut self, addr: XLEN, value: u32) -> Result<(), Status> {
        self.check_align(addr, 4)?;
        self.slice_mut(addr, 4)?
            .copy_from_slice(&value.to_le_bytes());
        Ok(())
    }
}

#[derive(Debug)]
pub enum Status {
    Trap(&'static str),
    IllegalInstruction(DecodeError),
    InvalidMemoryAccess(usize),
    UnalignedPc(u32),
    UnaligneMemoryAccess { addr: usize, required_align: u32 },
    Ebreak,
    Exit { code: i32 },
}

impl From<DecodeError> for Status {
    fn from(value: DecodeError) -> Self {
        Status::IllegalInstruction(value)
    }
}

pub struct Emulator<XLEN> {
    pub mem: Memory,
    pub xreg: [XLEN; 32],
    /// Written to insterad of xreg[0].
    pub xreg0_value: XLEN,
    pub pc: XLEN,
    /// We need to store the most recent reservation set for LR/SC
    /// to make SC fail if it's not to this address.
    pub reservation_set: Option<XLEN>,

    pub is_breaking: bool,

    pub debug: bool,
    pub break_pc: XLEN,
    pub ecall_handler: Box<dyn FnMut(&mut Memory, &mut [XLEN; 32]) -> Result<(), Status>>,
}

impl<XLEN> Index<Reg> for Emulator<XLEN> {
    type Output = XLEN;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.xreg[index.0 as usize]
    }
}
impl<XLEN> IndexMut<Reg> for Emulator<XLEN> {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        if index.0 == 0 {
            &mut self.xreg0_value
        } else {
            &mut self.xreg[index.0 as usize]
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reg(pub u32);

impl Reg {
    pub const ZERO: Reg = Reg(0);

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

fn hash_color(value: usize) -> impl Display {
    use owo_colors::*;
    use std::hash::{Hash, Hasher};
    let mut w = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut w);
    let hash = w.finish();
    let arr = [
        AnsiColors::Black,
        AnsiColors::Red,
        AnsiColors::Green,
        AnsiColors::Yellow,
        AnsiColors::Blue,
        AnsiColors::Magenta,
        AnsiColors::Cyan,
        AnsiColors::White,
        AnsiColors::Default,
    ];
    format!(
        "{}",
        format!("{value:x}").color(arr[(hash % arr.len() as u64) as usize])
    )
}

impl<XLEN: XLen> Emulator<XLEN> {
    pub fn start_linux(&mut self) -> Status {
        self.setup_linux_stack().unwrap();

        self.execute()
    }

    fn setup_linux_stack(&mut self) -> Result<(), Status> {
        // set top of stack. just some yolo address. with no values there. who needs abi?
        let sp: u32 = 4096 * 16;
        self[Reg::SP] = XLEN::from_32_z(sp);

        // The x86-64 psABI has a nice diagram of the stack layout (it's not arch specific).

        // | Purpose                          | Start Address | Length         |
        // |----------------------------------|--------- -----|----------------|
        // | Information block of strings etc | (high)        | varies         |
        // | Unspecified                      |               |                |
        // | Null auxiliary vector entry      |               | word           |
        // | Auxiliary vector entries         |               | two words each |
        // | 0                                |               | word           |
        // | Environment pointers             |               | word each      |
        // | 0                                | 4+4*argc+sp   | word           |
        // | Argument pointers                | 4+sp          | argc words     |
        // | Argument count                   | sp            | word           |
        // | Undefined                        | (low)         |                |

        let xlen = 4;

        let mut offset: u32 = 0;
        let mut next_word = || {
            let n = offset;
            offset += xlen;
            sp + n
        };

        // argc
        self.mem.store_u32(next_word(), 0)?;

        // null terminated argument pointers... (we have none.)
        self.mem.store_u32(next_word(), 0)?;

        // null terminated environment pointers... (we have none.)
        self.mem.store_u32(next_word(), 0)?;

        // null terminated auxiliary vector entries
        // TODO: add some auxv for the poor process.
        self.mem.store_u32(next_word(), 0)?;

        Ok(())
    }

    fn execute(&mut self) -> Status {
        loop {
            if let Err(err) = self.step() {
                return err;
            }
        }
    }

    fn set_pc(&mut self, pc: XLEN) -> Result<(), Status> {
        self.pc = pc;
        Ok(())
    }

    fn step(&mut self) -> Result<(), Status> {
        let code = self.mem.load_u32(self.pc)?;

        if self.debug {
            print!("0x{:x} ", self.pc.as_usize());
        }

        let (inst, was_compressed) = Inst::decode(code)?;

        if self.debug {
            println!(
                "{} (sp: {}) {inst:?}",
                match was_compressed {
                    IsCompressed::Yes => "C",
                    IsCompressed::No => " ",
                },
                hash_color(self.xreg[Reg::SP.0 as usize].as_usize())
            );
        }

        if self.pc == self.break_pc {
            self.is_breaking = true;
        }

        if self.is_breaking {
            self.debug_interactive();
        }

        let next_pc = self.pc.add(match was_compressed {
            IsCompressed::Yes => XLEN::from_32_z(2),
            IsCompressed::No => XLEN::from_32_z(4),
        });
        let mut jumped = false;

        match inst {
            Inst::Lui { uimm, dest } => self[dest] = XLEN::from_32_z(uimm),
            Inst::Auipc { uimm, dest } => self[dest] = self.pc.add(XLEN::from_32_z(uimm)),
            Inst::Jal { offset, dest } => {
                let target = self.pc.add(XLEN::from_32_s(offset));
                self[dest] = next_pc;
                self.set_pc(target)?;
                jumped = true;
            }
            Inst::Jalr { offset, base, dest } => {
                let target = self[base]
                    .add(XLEN::from_32_s(offset))
                    .and(XLEN::from_32_s(!1));
                self[dest] = next_pc;
                self.set_pc(target)?;
                jumped = true;
            }
            Inst::Beq { offset, src1, src2 } => {
                let take = self[src1] == self[src2];
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bne { offset, src1, src2 } => {
                let take = self[src1] != self[src2];
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Blt { offset, src1, src2 } => {
                let take = self[src1].signed_lt(self[src2]);
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bge { offset, src1, src2 } => {
                let take = self[src1].signed_ge(self[src2]);
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bltu { offset, src1, src2 } => {
                let take = self[src1].unsigned_lt(self[src2]);
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Bgeu { offset, src1, src2 } => {
                let take = self[src1].unsigned_ge(self[src2]);
                if take {
                    let target = self.pc.add(XLEN::from_32_s(offset));
                    self.set_pc(target)?;
                    jumped = true;
                }
            }
            Inst::Lb { offset, dest, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self[dest] = XLEN::from_8_s(self.mem.load_u8(addr)?);
            }
            Inst::Lbu { offset, dest, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self[dest] = XLEN::from_8_z(self.mem.load_u8(addr)?);
            }
            Inst::Lh { offset, dest, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self[dest] = XLEN::from_16_s(self.mem.load_u16(addr)?);
            }
            Inst::Lhu { offset, dest, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self[dest] = XLEN::from_16_z(self.mem.load_u16(addr)?);
            }
            Inst::Lw { offset, dest, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self[dest] = XLEN::from_32_s(self.mem.load_u32(addr)?);
            }
            Inst::Sb { offset, src, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self.mem.store_u8(addr, self[src].truncate8())?;
            }
            Inst::Sh { offset, src, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self.mem.store_u16(addr, self[src].truncate16())?;
            }
            Inst::Sw { offset, src, base } => {
                let addr = self[base].add(XLEN::from_32_s(offset));
                self.mem.store_u32(addr, self[src].truncate32())?;
            }
            Inst::Addi { imm, dest, src1 } => {
                self[dest] = self[src1].add(XLEN::from_32_s(imm));
            }
            Inst::Slti { imm, dest, src1 } => {
                let result = self[src1].signed_lt(XLEN::from_32_s(imm));
                self[dest] = XLEN::from_bool(result);
            }
            Inst::Sltiu { imm, dest, src1 } => {
                let result = self[src1].unsigned_lt(XLEN::from_32_s(imm));
                self[dest] = XLEN::from_bool(result);
            }
            Inst::Andi { imm, dest, src1 } => {
                self[dest] = self[src1].and(XLEN::from_32_s(imm));
            }
            Inst::Ori { imm, dest, src1 } => {
                self[dest] = self[src1].or(XLEN::from_32_s(imm));
            }
            Inst::Xori { imm, dest, src1 } => {
                self[dest] = self[src1].xor(XLEN::from_32_s(imm));
            }
            Inst::Slli { imm, dest, src1 } => self[dest] = self[src1].shl(imm),
            Inst::Srli { imm, dest, src1 } => self[dest] = self[src1].unsigned_shr(imm),
            Inst::Srai { imm, dest, src1 } => self[dest] = self[src1].signed_shr(imm),
            Inst::Add { dest, src1, src2 } => self[dest] = self[src1].add(self[src2]),
            Inst::Sub { dest, src1, src2 } => self[dest] = self[src1].sub(self[src2]),
            Inst::Sll { dest, src1, src2 } => self[dest] = self[src1].shl(self[src2].truncate32()),
            Inst::Slt { dest, src1, src2 } => {
                self[dest] = XLEN::from_bool(self[src1].signed_lt(self[src2]));
            }
            Inst::Sltu { dest, src1, src2 } => {
                self[dest] = XLEN::from_bool(self[src1].unsigned_lt(self[src2]));
            }
            Inst::Xor { dest, src1, src2 } => self[dest] = self[src1].xor(self[src2]),
            Inst::Srl { dest, src1, src2 } => {
                self[dest] = self[src1].unsigned_shr(self[src2].truncate32())
            }
            Inst::Sra { dest, src1, src2 } => {
                self[dest] = self[src1].signed_shr(self[src2].truncate32())
            }
            Inst::Or { dest, src1, src2 } => self[dest] = self[src1].or(self[src2]),
            Inst::And { dest, src1, src2 } => self[dest] = self[src1].and(self[src2]),
            Inst::Fence { fence: _ } => { /* dont care */ }
            Inst::Ecall => {
                (self.ecall_handler)(&mut self.mem, &mut self.xreg)?;
            }
            Inst::Ebreak => {
                if self.debug {
                    self.is_breaking = true;
                } else {
                    return Err(Status::Ebreak);
                }
            }
            Inst::Mul { dest, src1, src2 } => {
                self[dest] = self[src1].mul_lower(self[src2]);
            }
            Inst::Mulh { dest, src1, src2 } => {
                self[dest] = self[src1].signed_mul_upper(self[src2]);
            }
            Inst::Mulhsu { .. } => todo!("mulhsu"),
            Inst::Mulhu { dest, src1, src2 } => {
                self[dest] = self[src1].unsigned_mul_upper(self[src2]);
            }
            Inst::Div { dest, src1, src2 } => {
                if self[src2] == XLEN::ZERO {
                    self[dest] = XLEN::MAX;
                } else if self[src1] == XLEN::SIGNED_MIN && self[src2] == XLEN::MAX {
                    self[dest] = XLEN::MAX;
                } else {
                    self[dest] = self[src1].signed_div(self[src2]);
                }
            }
            Inst::Divu { dest, src1, src2 } => {
                if self[src2] == XLEN::ZERO {
                    self[dest] = XLEN::MAX;
                } else {
                    self[dest] = self[src1].unsigned_div(self[src2]);
                }
            }
            Inst::Rem { dest, src1, src2 } => {
                if self[src2] == XLEN::ZERO {
                    self[dest] = self[src1];
                } else if self[src1] == XLEN::SIGNED_MIN && self[src2] == XLEN::MAX {
                    self[dest] = XLEN::ZERO;
                } else {
                    self[dest] = self[src1].signed_rem(self[src2]);
                }
            }
            Inst::Remu { dest, src1, src2 } => {
                if self[src2] == XLEN::ZERO {
                    self[dest] = self[src1];
                } else {
                    self[dest] = self[src1].unsigned_rem(self[src2]);
                }
            }
            Inst::AmoW {
                order: _,
                op,
                dest,
                addr,
                src,
            } => {
                let addr = self[addr];
                self[dest] = XLEN::from_32_s(self.mem.load_u32(addr)?);
                let result = match op {
                    AmoOp::Swap => self[src],
                    AmoOp::Add => self[dest].add(self[src]),
                    AmoOp::Xor => self[dest].xor(self[src]),
                    AmoOp::And => self[dest].and(self[src]),
                    AmoOp::Or => self[dest].or(self[src]),
                    AmoOp::Min => self[dest].signed_min(self[src]),
                    AmoOp::Max => self[dest].signed_max(self[src]),
                    AmoOp::Minu => self[dest].unsigned_min(self[src]),
                    AmoOp::Maxu => self[dest].unsigned_max(self[src]),
                };
                self.mem.store_u32(addr, result.truncate32())?;
            }
            Inst::LrW {
                order: _,
                dest,
                addr,
            } => {
                let addr = self[addr];
                self[dest] = XLEN::from_32_s(self.mem.load_u32(addr)?);
                self.reservation_set = Some(addr);
            }
            Inst::ScW {
                order: _,
                dest,
                addr,
                src,
            } => {
                let addr = self[addr];
                self.mem.store_u32(addr, self[src].truncate32())?;
                if self.reservation_set != Some(addr) {
                    self[dest] = XLEN::from_32_z(1); // error
                } else {
                    self[dest] = XLEN::from_32_z(0); // success
                }
                self.reservation_set = None;
            }
        }

        if !jumped {
            self.set_pc(next_pc)?;
        }
        Ok(())
    }

    pub fn debug_interactive(&mut self) {
        use owo_colors::OwoColorize;
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let mut input = input.trim().split_whitespace();
            match input.next().unwrap_or_default() {
                "c" => {
                    self.is_breaking = false;
                    return;
                }
                "s" => {
                    return;
                }
                "q" => std::process::exit(0),
                "m" => {
                    let Some(size) = input.next() else {
                        eprintln!("require b/h/w argument for m command");
                        continue;
                    };
                    let Some(addr) = input.next() else {
                        eprintln!("require address argument for m command");
                        continue;
                    };
                    let Ok(addr) = (if let Some(addr) = addr.strip_prefix("0x") {
                        u32::from_str_radix(addr, 16)
                    } else {
                        u32::from_str_radix(&addr, 10)
                    }) else {
                        eprintln!("invalid address");
                        continue;
                    };
                    let value = match size {
                        "w" => self.mem.load_u32(addr),
                        "h" => self.mem.load_u16(addr).map(Into::into),
                        "b" => self.mem.load_u8(addr).map(Into::into),
                        _ => {
                            eprintln!("require b/h/w argument for m command");
                            continue;
                        }
                    };
                    let value = match value {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("failed to load value: {e:?}");
                            continue;
                        }
                    };

                    println!("{value} (0x{value:x})");
                }
                "r" => {
                    let format_value = |v: usize| {
                        if v == 0 {
                            format!("{:0>8x}", v.black())
                        } else {
                            format!("{:0>8x}", v)
                        }
                    };
                    let r = |i: usize| format_value(self.xreg[i].as_usize());
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "ra".red(),
                        r(1),
                        "sp".red(),
                        r(2),
                        "gp".red(),
                        r(3),
                        "tp".red(),
                        r(4)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "a0".green(),
                        r(10),
                        "a1".green(),
                        r(11),
                        "a2".green(),
                        r(12),
                        "a3".green(),
                        r(13)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "a4".green(),
                        r(14),
                        "a5".green(),
                        r(15),
                        "a6".green(),
                        r(16),
                        "a7".green(),
                        r(17)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "s0".cyan(),
                        r(8),
                        "s1".cyan(),
                        r(9),
                        "s2".cyan(),
                        r(18),
                        "s3".cyan(),
                        r(19)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "s4".cyan(),
                        r(20),
                        "s5".cyan(),
                        r(21),
                        "s6".cyan(),
                        r(22),
                        "s7".cyan(),
                        r(23)
                    );
                    println!(
                        "{}: {} | {}: {} | {}: {} | {}: {}",
                        "s8".cyan(),
                        r(24),
                        "s9".cyan(),
                        r(25),
                        "s10".cyan(),
                        r(26),
                        "s11".cyan(),
                        r(27)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "t0".yellow(),
                        r(5),
                        "t1".yellow(),
                        r(6),
                        "t2".yellow(),
                        r(7),
                        "t3".yellow(),
                        r(28)
                    );
                    println!(
                        "{}: {} | {}: {} |  {}: {} |  {}: {}",
                        "t4".yellow(),
                        r(29),
                        "t5".yellow(),
                        r(30),
                        "t6".yellow(),
                        r(31),
                        "pc".red(),
                        format_value(self.pc.as_usize())
                    );
                }
                _ => println!(
                    "commands:
- ?: help
- r: print registers
- m <b|h|w> <addr>: read a byte/half/word of memory at an address
- c: continue until next breakpoint
- s: step one instruction
- q: quit"
                ),
            }
        }
    }
}

pub trait XLen: Copy + PartialEq + Eq {
    const ZERO: Self;
    const SIGNED_MIN: Self;
    const MAX: Self;

    fn from_bool(v: bool) -> Self;
    fn from_8_z(v: u8) -> Self {
        Self::from_32_z(v as u32)
    }
    fn from_8_s(v: u8) -> Self {
        Self::from_32_s(v as i8 as i32 as u32)
    }
    fn from_16_z(v: u16) -> Self {
        Self::from_32_z(v as u32)
    }
    fn from_16_s(v: u16) -> Self {
        Self::from_32_s(v as i16 as i32 as u32)
    }
    fn from_32_z(v: u32) -> Self;
    fn from_32_s(v: u32) -> Self;

    fn truncate8(self) -> u8 {
        self.truncate32() as u8
    }
    fn truncate16(self) -> u16 {
        self.truncate32() as u16
    }
    fn truncate32(self) -> u32;
    fn as_usize(self) -> usize;

    fn add(self, other: Self) -> Self;
    fn sub(self, other: Self) -> Self;
    fn and(self, other: Self) -> Self;
    fn or(self, other: Self) -> Self;
    fn xor(self, other: Self) -> Self;
    fn signed_lt(self, other: Self) -> bool;
    fn unsigned_lt(self, other: Self) -> bool;
    fn signed_ge(self, other: Self) -> bool;
    fn unsigned_ge(self, other: Self) -> bool;
    fn shl(self, other: u32) -> Self;
    fn signed_shr(self, other: u32) -> Self;
    fn unsigned_shr(self, other: u32) -> Self;
    fn signed_min(self, other: Self) -> Self;
    fn unsigned_min(self, other: Self) -> Self;
    fn signed_max(self, other: Self) -> Self;
    fn unsigned_max(self, other: Self) -> Self;
    fn mul_lower(self, other: Self) -> Self;
    fn signed_mul_upper(self, other: Self) -> Self;
    fn unsigned_mul_upper(self, other: Self) -> Self;
    fn signed_div(self, other: Self) -> Self;
    fn unsigned_div(self, other: Self) -> Self;
    fn signed_rem(self, other: Self) -> Self;
    fn unsigned_rem(self, other: Self) -> Self;
}

impl XLen for u32 {
    const ZERO: Self = 0;
    const SIGNED_MIN: Self = i32::MIN as u32;
    const MAX: Self = u32::MAX;

    fn from_bool(v: bool) -> Self {
        v as u32
    }
    fn from_32_s(v: u32) -> Self {
        v
    }
    fn from_32_z(v: u32) -> Self {
        v
    }
    fn as_usize(self) -> usize {
        self as usize
    }

    fn truncate32(self) -> u32 {
        self
    }

    fn add(self, other: Self) -> Self {
        self.wrapping_add(other)
    }
    fn sub(self, other: Self) -> Self {
        self.wrapping_sub(other)
    }
    fn and(self, other: Self) -> Self {
        self & other
    }
    fn or(self, other: Self) -> Self {
        self | other
    }
    fn xor(self, other: Self) -> Self {
        self ^ other
    }
    fn signed_lt(self, other: Self) -> bool {
        (self as i32) < (other as i32)
    }
    fn unsigned_lt(self, other: Self) -> bool {
        self < other
    }
    fn signed_ge(self, other: Self) -> bool {
        (self as i32) >= (other as i32)
    }
    fn unsigned_ge(self, other: Self) -> bool {
        self >= other
    }
    fn shl(self, other: u32) -> Self {
        self.wrapping_shl(other)
    }
    fn unsigned_shr(self, other: u32) -> Self {
        self.wrapping_shr(other)
    }
    fn signed_shr(self, other: u32) -> Self {
        ((self as i32).wrapping_shr(other)) as u32
    }
    fn signed_min(self, other: Self) -> Self {
        (self as i32).min(other as i32) as u32
    }
    fn unsigned_min(self, other: Self) -> Self {
        self.min(other)
    }
    fn signed_max(self, other: Self) -> Self {
        (self as i32).max(other as i32) as u32
    }
    fn unsigned_max(self, other: Self) -> Self {
        self.max(other)
    }
    fn mul_lower(self, other: Self) -> Self {
        (self as i32).wrapping_mul(other as i32) as u32
    }
    fn signed_mul_upper(self, other: Self) -> Self {
        let mul_result = (self as i32 as i64).wrapping_mul(other as i32 as i64);
        let shifted = (mul_result as u64) >> 32;
        shifted as u32
    }
    fn unsigned_mul_upper(self, other: Self) -> Self {
        let shifted = ((self as u64).wrapping_mul(other as u64)) >> 32;
        shifted as u32
    }
    fn signed_div(self, other: Self) -> Self {
        ((self as i32) / (other as i32)) as u32
    }
    fn unsigned_div(self, other: Self) -> Self {
        self / other
    }
    fn signed_rem(self, other: Self) -> Self {
        ((self as i32) % (other as i32)) as u32
    }
    fn unsigned_rem(self, other: Self) -> Self {
        self % other
    }
}
