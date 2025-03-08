#![no_std]
#![no_main]

use core::fmt::Write;

fn exit(code: i32) -> ! {
    unsafe {
        core::arch::asm!(
            "li a7, 93",
            "ecall",
            in("a0") code,
            options(noreturn),
        );
    }
}

fn write(fd: i32, data: &[u8]) -> isize {
    let mut out;
    unsafe {
        core::arch::asm!(
            "li a7, 64",
            "ecall",
            in("a0") fd,
            in("a1") data.as_ptr(),
            in("a2") data.len(),
            lateout("a0") out,
            out("a7") _,
        )
    }
    out
}

fn read(fd: i32, buf: &mut [u8]) -> isize {
    let mut out;
    unsafe {
        core::arch::asm!(
            "li a7, 63",
            "ecall",
            in("a0") fd,
            in("a1") buf.as_mut_ptr(),
            in("a2") buf.len(),
            lateout("a0") out,
            out("a7") _,
        )
    }
    out
}

struct Stderr;

impl core::fmt::Write for Stderr {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let ret = write(2, s.as_bytes());
        if ret < 0 { Err(core::fmt::Error) } else { Ok(()) }
    }
}

#[panic_handler]
fn handle(info: &core::panic::PanicInfo<'_>) -> ! {
    let _ = writeln!(Stderr, "panicked: {}", info.message());
    unsafe { core::arch::asm!("unimp", options(noreturn)) }
}

#[no_mangle]
fn _start() {
    write(1, b"enter a number: ");
    let mut buf = [0; 10];
    let len = read(0, &mut buf);
    let buf = &buf[..(len as usize)];
    let n = str::from_utf8(buf).unwrap().trim().parse::<i32>().unwrap();

    exit(n);
}
