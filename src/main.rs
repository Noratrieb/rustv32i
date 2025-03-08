use aligned_vec::avec;
use eyre::{OptionExt, bail};

mod elf;
mod emu;

const PAGE_SIZE: usize = 4096;

// 2 MiB
const MEMORY_SIZE: usize = 2 << 21;

fn main() -> eyre::Result<()> {
    let content = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();

    let elf = elf::Elf { content };
    let header = elf.header()?;

    dbg!(&header);

    let segments = elf.segments()?;

    let mut mem = emu::Memory {
        mem: avec![[PAGE_SIZE]| 0; MEMORY_SIZE],
    };

    for phdr in segments {
        dbg!(&phdr);
        match phdr.p_type {
            // PT_NULL
            0 => {}
            // PT_LOAD
            1 => {
                if phdr.p_filesz > 0 {
                    let contents = &elf
                        .content
                        .get((phdr.p_offset as usize)..)
                        .ok_or_eyre("invalid offset")?
                        .get(..(phdr.p_filesz as usize))
                        .ok_or_eyre("invalid offset")?;

                    mem.mem
                        .get_mut((phdr.p_vaddr as usize)..)
                        .ok_or_eyre("invalid offset")?
                        .get_mut(..(phdr.p_filesz as usize))
                        .ok_or_eyre("invalid offset")?
                        .copy_from_slice(contents);
                }
            }
            // PT_PHDR
            6 => {}
            // PT_GNU_EH_FRAME
            1685382480 => {}
            // PT_GNU_STACK
            1685382481 => {}
            // PT_RISCV_ATTRIBUTES
            0x70000003 => {}
            _ => bail!("unknown program header type: {}", phdr.p_type),
        }
    }

    let start = header.e_entry;

    let mut emu = emu::Emulator {
        mem,
        xreg: [0; 32],
        xreg0_value: 0,
        pc: start,
    };
    emu.start_linux().unwrap();

    Ok(())
}
