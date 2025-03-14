use std::io::Write;

use eyre::eyre;
use rustv32i::emu::{self, Memory, Reg};

fn main() -> eyre::Result<()> {
    let content = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();

    let break_addr = std::env::args()
        .skip_while(|arg| arg != "--break")
        .nth(1)
        .map(|addr| {
            if let Some(addr) = addr.strip_prefix("0x") {
                u32::from_str_radix(addr, 16)
            } else {
                u32::from_str_radix(&addr, 10)
            }
        })
        .unwrap_or(Ok(0))?;

    let debug = std::env::args().any(|arg| arg == "--debug");

    let mut syscall_state = SyscallState { set_child_tid: 0 };

    let status = rustv32i::execute_linux_elf(
        &content,
        debug,
        break_addr,
        Box::new(move |mem, xreg| ecall_handler(mem, xreg, &mut syscall_state)),
    )?;

    std::io::stdout().flush()?;

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
    let arg1 = xreg[Reg::A1.0 as usize];
    let arg2 = xreg[Reg::A2.0 as usize];

    // https://jborza.com/post/2021-05-11-riscv-linux-syscalls/
    // https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/unistd.h
    match nr {
        // ioctl
        29 => {
            let fd = arg0;
            let request = arg1;

            match request {
                // TIOCGWINSZ
                0x5413 => {
                    let wsz_ptr = xreg[Reg::A2.0 as usize];

                    let mut wsz: libc::winsize = unsafe { std::mem::zeroed() };

                    let r = unsafe { libc::ioctl(fd as i32, libc::TIOCGWINSZ, &mut wsz) };

                    xreg[Reg::A0.0 as usize] = r as u32;
                    if r >= 0 {
                        mem.store_u16(wsz_ptr, wsz.ws_row)?;
                        mem.store_u16(wsz_ptr + 2, wsz.ws_col)?;
                        mem.store_u16(wsz_ptr + 4, wsz.ws_xpixel)?;
                        mem.store_u16(wsz_ptr + 6, wsz.ws_ypixel)?;
                    }
                }
                _ => todo!("unknown ioctl: {request}"),
            }
        }
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
        // https://man7.org/linux/man-pages/man3/writev.3p.html
        66 => {
            let fd = arg0;
            let iovec = arg1;
            let iovcnt = arg2;

            let mut written = 0;

            for i in 0..iovcnt {
                let iov_ptr = mem.load_u32(iovec + i * 8)?;
                let iov_len = mem.load_u32(iovec + i * 8 + 4)?;

                let data = mem.slice(iov_ptr, iov_len)?;

                let len = unsafe { libc::write(fd as i32, data.as_ptr().cast(), data.len()) };
                let ret = if len < 0 {
                    (-std::io::Error::last_os_error().raw_os_error().unwrap_or(1)) as u32
                } else {
                    len as u32
                };

                if (ret as i32) < 0 {
                    xreg[Reg::A0.0 as usize] = ret;
                    return Ok(());
                } else {
                    written += ret;
                }
            }

            xreg[Reg::A0.0 as usize] = written;
        }
        // exit | exit_group
        93 | 94 => {
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
        // ppoll - called for some stdin/stdout/stderr check.
        414 => {
            // musl uses ppoll to batch check whether FDs 0,1,2 are invalid,
            // and opens /dev/null for them if they are.
            // They're always valid here, so just get out.

            xreg[Reg::A0.0 as usize] = 0;
        }
        _ => {
            todo!("unkonwn syscall: {nr}");
        }
    }

    Ok(())
}
