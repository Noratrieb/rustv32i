use emu::{Memory, Reg};
use eyre::{OptionExt, bail, eyre};

mod elf;
mod emu;
mod inst;

// 2 MiB
const MEMORY_SIZE: usize = 2 << 21;

fn main() -> eyre::Result<()> {
    let content = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();

    let elf = elf::Elf { content };
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
        pc: start,

        debug: std::env::args().any(|arg| arg == "--debug"),

        ecall_handler: Box::new(ecall_handler),
    };

    match emu.start_linux() {
        emu::Error::Exit { code } => {
            eprintln!("exited with code {code}");
        }
        emu::Error::Trap(cause) => eprintln!("program trapped: {cause}"),
        e => return Err(eyre!("error: {e:?}")),
    }

    Ok(())
}

fn ecall_handler(mem: &mut Memory, xreg: &mut [u32; 32]) -> Result<(), emu::Error> {
    let nr = xreg[Reg::A7.0 as usize];

    match nr {
        // read
        63 => {
            let fd = xreg[Reg::A0.0 as usize];
            let ptr = xreg[Reg::A1.0 as usize];
            let len = xreg[Reg::A2.0 as usize];

            let buf = mem.slice_mut(ptr, len)?;

            let len = unsafe { libc::read(fd as i32, buf.as_mut_ptr().cast(), buf.len()) };
            let ret = if len < 0 {
                (-std::io::Error::last_os_error().raw_os_error().unwrap_or(1)) as u32
            } else {
                len as u32
            };

            xreg[Reg::A0.0 as usize] = ret;
        }
        // write
        64 => {
            let fd = xreg[Reg::A0.0 as usize];
            let ptr = xreg[Reg::A1.0 as usize];
            let len = xreg[Reg::A2.0 as usize];

            let data = mem.slice(ptr, len)?;

            let len = unsafe { libc::write(fd as i32, data.as_ptr().cast(), data.len()) };
            let ret = if len < 0 {
                (-std::io::Error::last_os_error().raw_os_error().unwrap_or(1)) as u32
            } else {
                len as u32
            };

            xreg[Reg::A0.0 as usize] = ret;
        }
        // exit
        93 => {
            return Err(emu::Error::Exit {
                code: xreg[Reg::A0.0 as usize] as i32,
            });
        }
        _ => {
            todo!("unkonwn syscall: {nr}");
        }
    }

    Ok(())
}
