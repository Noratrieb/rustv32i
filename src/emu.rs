use aligned_vec::{AVec, ConstAlign};

use crate::PAGE_SIZE;

pub struct Memory {
    pub mem: AVec<u8, ConstAlign<PAGE_SIZE>>,
}

impl Memory {
    fn load_u32(&self, addr: u32) -> Result<u32, ()> {
        Ok(u32::from_le_bytes(
            self.mem
                .get((addr as usize)..)
                .ok_or(())?
                .get(..4)
                .ok_or(())?
                .try_into()
                .unwrap(),
        ))
    }
}

pub struct Emulator {
    pub mem: Memory,
    pub xreg: [u32; 32],
    pub pc: u32,
}

impl Emulator {
    pub fn execute(&mut self) -> Result<(), ()> {
        loop {
            self.step();
        }
    }

    fn step(&mut self) -> Result<(), ()> {
        let instruction = self.mem.load_u32(self.pc)?;

        todo!()
    }
}
