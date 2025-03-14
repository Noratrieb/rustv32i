use eyre::eyre;
use rustv32i::emu::{self, Memory, Reg};

fn main() -> eyre::Result<()> {
    let content = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();

    let mut syscall_state = SyscallState { set_child_tid: 0 };

    let status = rustv32i::execute_linux_elf(
        &content,
        std::env::args().any(|arg| arg == "--debug"),
        Box::new(move |mem, xreg| ecall_handler(mem, xreg, &mut syscall_state)),
    )?;

    match status {
        emu::Status::Exit { code } => {
            eprintln!("exited with code {code}");
        }
        emu::Status::Trap(cause) => eprintln!("program trapped: {cause}"),
        e => return Err(eyre!("error: {e:?}")),
    }

    Ok(())
}

struct SyscallState {
    set_child_tid: u32,
}

fn ecall_handler(
    mem: &mut Memory,
    xreg: &mut [u32; 32],
    syscall_state: &mut SyscallState,
) -> Result<(), emu::Status> {
    let nr = xreg[Reg::A7.0 as usize];

    let arg0 = xreg[Reg::A0.0 as usize];

    // https://jborza.com/post/2021-05-11-riscv-linux-syscalls/
    match nr {
        // read
        63 => {
            let fd = arg0;
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
            let fd = arg0;
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
            return Err(emu::Status::Exit {
                code: xreg[Reg::A0.0 as usize] as i32,
            });
        }
        // <https://man7.org/linux/man-pages/man2/set_tid_address.2.html>
        96 => {
            let tidptr = arg0;
            syscall_state.set_child_tid = tidptr;

            xreg[Reg::A0.0 as usize] = 1; // thread ID
        }
        _ => {
            todo!("unkonwn syscall: {nr}");
        }
    }

    Ok(())
}
