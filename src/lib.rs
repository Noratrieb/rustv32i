use eyre::{OptionExt, bail};

pub mod elf;
pub mod emu;

// 2 MiB
const MEMORY_SIZE: usize = 2 << 21;

pub fn execute_linux_elf(
    elf: &[u8],
    debug: bool,
    break_addr: u32,
    ecall_handler: Box<dyn FnMut(&mut emu::Memory, &mut [u32; 32]) -> Result<(), emu::Status>>,
) -> eyre::Result<emu::Status> {
    let elf = elf::Elf { content: elf };
    let header = elf.header()?;

    let segments = elf.segments_32()?;

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

    let mut emu = emu::Emulator {
        mem,
        xreg: [0; 32],
        xreg0_value: 0,
        pc: start.0 as u32,
        reservation_set: None,

        is_breaking: false,

        break_pc: break_addr,
        debug,
        ecall_handler,
    };

    Ok(emu.start_linux())
}
