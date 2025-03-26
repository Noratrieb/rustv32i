use eyre::{OptionExt, bail};

pub mod elf;
pub mod emu;

// 2 MiB
const MEMORY_SIZE: usize = 2 << 21;

pub fn execute_linux_elf(
    elf: &[u8],
    debug: bool,
    break_addr: u64,
    ecall_handler32: Box<dyn FnMut(&mut emu::Memory, &mut [u32; 32]) -> Result<(), emu::Status>>,
    ecall_handler64: Box<dyn FnMut(&mut emu::Memory, &mut [u64; 32]) -> Result<(), emu::Status>>,
) -> eyre::Result<emu::Status> {
    let elf = elf::Elf { content: elf };
    let header = elf.header()?;

    let segments = elf.segments()?;

    let mut mem = emu::Memory {
        mem: vec![0; MEMORY_SIZE],
    };

    for phdr in segments {
        match phdr.p_type {
            // PT_NULL
            0 => {}
            // PT_LOAD
            1 => {
                if phdr.p_filesz > 0 {
                    let contents = &elf
                        .content
                        .get((phdr.p_offset.0 as usize)..)
                        .ok_or_eyre("invalid offset")?
                        .get(..(phdr.p_filesz as usize))
                        .ok_or_eyre("invalid offset")?;

                    mem.mem
                        .get_mut((phdr.p_vaddr.0 as usize)..)
                        .ok_or_eyre("invalid offset")?
                        .get_mut(..(phdr.p_filesz as usize))
                        .ok_or_eyre("invalid offset")?
                        .copy_from_slice(contents);
                }
            }
            // PT_DYNAMIC
            2 => {}
            // PT_PHDR
            6 => {}
            // PT_GNU_EH_FRAME
            1685382480 => {}
            // PT_GNU_STACK
            1685382481 => {}
            // PT_GNU_RELRO
            1685382482 => {}
            // PT_RISCV_ATTRIBUTES
            0x70000003 => {}
            _ => bail!("unknown program header type: {}", phdr.p_type),
        }
    }

    let start = header.e_entry;

    match header.class {
        elf::ElfClass::Elf32 => {
            let mut emu =
                emu::Emulator::<u32>::new(mem, start.0 as u32, break_addr as u32, debug, ecall_handler32);
            Ok(emu.start_linux())
        }
        elf::ElfClass::Elf64 => {
            let mut emu =
                emu::Emulator::<u64>::new(mem, start.0, break_addr, debug, ecall_handler64);
            Ok(emu.start_linux())
        }
    }
}
