use std::{
    io::Write,
    path::{Path, PathBuf},
};

use eyre::{Context, bail};
use rustv32i::emu::{Reg, Status};

#[test]
fn check() -> eyre::Result<()> {
    let tmpdir = tempfile::tempdir().wrap_err("failed to create tempdir")?;

    let dir =
        Path::new(&std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is not set"))
            .join("tests")
            .join("check");
    let files = std::fs::read_dir(&dir).wrap_err(format!("reading {}", dir.display()))?;

    for file in files {
        let file = file.wrap_err(format!("reading file in {}", dir.display()))?;

        let name = file.file_name();
        let name = name.to_str().unwrap();
        if !name.ends_with(".s") {
            continue;
        }

        write!(std::io::stdout(), "test {name} ...")?;
        std::io::stdout().flush()?;

        let output = build(&tmpdir.path(), &file.path()).wrap_err(format!("building {name}"))?;
        let content =
            std::fs::read(&output).wrap_err(format!("reading output from {}", output.display()))?;

        let status = rustv32i::execute_linux_elf(
            &content,
            matches!(std::env::var("EMULATOR_DEBUG").as_deref(), Ok(v) if v != "0"),
            Box::new(|_, xreg| {
                if xreg[Reg::A7.0 as usize] == u32::MAX {
                    if xreg[Reg::A0.0 as usize] == 1 {
                        Err(rustv32i::emu::Status::Exit { code: 0 })
                    } else {
                        Err(rustv32i::emu::Status::Trap("wrong exit code"))
                    }
                } else {
                    Err(rustv32i::emu::Status::Trap("wrong syscall"))
                }
            }),
        )
        .wrap_err(format!("{name} failed"))?;

        if let Status::Exit { code: 0 } = status {
            writeln!(std::io::stdout(), " ✅")?;
        } else {
            bail!("{name} returned an error: {status:?}");
        }
    }

    Ok(())
}

fn build(tmpdir: &Path, src: &Path) -> eyre::Result<PathBuf> {
    let out_path = tmpdir.join(Path::new(src.file_name().unwrap()).with_extension(""));

    let mut cmd = std::process::Command::new("clang");
    cmd.args([
        "-target",
        "riscv32-unknown-none-elf",
        "-nostdlib",
        "-march=rv32ima",
    ]);
    cmd.arg(src);
    cmd.arg("-o");
    cmd.arg(&out_path);

    let output = cmd.output().wrap_err("failed to spawn clang")?;
    if !output.status.success() {
        bail!(
            "failed to compile {}:\n{}",
            src.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(out_path)
}
