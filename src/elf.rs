use eyre::{Result, bail};

pub struct Elf<'a> {
    pub content: &'a [u8],
}

#[derive(Debug)]
pub struct Addr(pub u64);

#[derive(Debug)]
pub struct Offset(pub u64);

#[derive(Debug)]
pub enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug)]
pub struct Header {
    pub class: ElfClass,
    pub e_entry: Addr,
    pub e_phoff: Offset,
    pub e_shoff: Offset,
    pub e_flags: u32,
    pub e_ehsize: u16,
    pub e_phentsize: u16,
    pub e_phnum: u16,
    pub e_shentsize: u16,
    pub e_shnum: u16,
    pub e_shstrndx: u16,
}

#[derive(Debug)]
pub struct Phdr {
    pub p_type: u32,
    pub p_offset: Offset,
    pub p_vaddr: Addr,
    pub p_paddr: Addr,
    pub p_filesz: u64,
    pub p_memsz: u64,
    pub p_flags: u32,
    pub p_align: u64,
}

impl Elf<'_> {
    fn class(&self) -> Result<ElfClass> {
        let (_, class) = self.content.split_u32()?;
        let (class, _) = class.split_bytes(1)?;
        Ok(match class[0] {
            // ELFCLASS32
            1 => ElfClass::Elf32,
            // ELFCLASS64
            2 => ElfClass::Elf64,
            _ => bail!("not a ELF32 or ELF64 file (EI_CLASS={})", class[0]),
        })
    }

    pub fn header(&self) -> Result<Header> {
        let (ident, rest) = self.content.split_bytes(16)?;
        if ident[..4] != *b"\x7fELF" {
            bail!("not an elf file (invalid magic)");
        }

        let class = match ident[4] {
            // ELFCLASS32
            1 => ElfClass::Elf32,
            // ELFCLASS64
            2 => ElfClass::Elf64,
            _ => bail!("not a ELF32 or ELF64 file (EI_CLASS={})", ident[4]),
        };

        // EV_CURRENT
        if ident[5] != 1 {
            bail!("not a LE file (EI_DATA={})", ident[5]);
        }
        // EV_CURRENT
        if ident[6] != 1 {
            bail!("invalid ELF version (EI_VERSION={})", ident[6]);
        }
        // ELFOSABI_NONE
        if ident[7] != 0 {
            bail!("invalid OS ABI (EI_OSABI={})", ident[7]);
        }
        if ident[8] != 0 {
            bail!("invalid OS ABI version (EI_ABIVERSION={})", ident[8]);
        }

        let (e_type, rest) = rest.split_u16()?;
        // ET_EXEC|ET_DYN
        if e_type != 2 && e_type != 3 {
            bail!("not an executable: (e_type={e_type})");
        }

        let (e_machine, rest) = rest.split_u16()?;
        // EM_RISCV
        if e_machine != 243 {
            bail!("not a RISC-V executable (e_machine={e_machine})");
        }

        let (e_version, rest) = rest.split_u32()?;
        // e_version
        if e_version != 1 {
            bail!("invalid OS ABI version (e_version={e_version})");
        }

        let (e_entry, e_phoff, e_shoff, rest) = match class {
            ElfClass::Elf32 => {
                let (e_entry, rest) = rest.split_u32()?;
                let (e_phoff, rest) = rest.split_u32()?;
                let (e_shoff, rest) = rest.split_u32()?;
                (
                    Addr(e_entry as u64),
                    Offset(e_phoff as u64),
                    Offset(e_shoff as u64),
                    rest,
                )
            }
            ElfClass::Elf64 => {
                let (e_entry, rest) = rest.split_u64()?;
                let (e_phoff, rest) = rest.split_u64()?;
                let (e_shoff, rest) = rest.split_u64()?;
                (
                    Addr(e_entry as u64),
                    Offset(e_phoff as u64),
                    Offset(e_shoff as u64),
                    rest,
                )
            }
        };

        let (e_flags, rest) = rest.split_u32()?;
        let (e_ehsize, rest) = rest.split_u16()?;
        let (e_phentsize, rest) = rest.split_u16()?;
        let (e_phnum, rest) = rest.split_u16()?;
        let (e_shentsize, rest) = rest.split_u16()?;
        let (e_shnum, rest) = rest.split_u16()?;
        let (e_shstrndx, _) = rest.split_u16()?;

        Ok(Header {
            class,
            e_entry,
            e_phoff,
            e_shoff,
            e_flags,
            e_ehsize,
            e_phentsize,
            e_phnum,
            e_shentsize,
            e_shnum,
            e_shstrndx,
        })
    }

    pub fn segments(&self) -> Result<Vec<Phdr>> {
        let header = self.header()?;
        let class = self.class()?;

        let (_, phdrs) = self.content.split_bytes(header.e_phoff.0 as usize)?;
        let (mut phdrs, _) = phdrs.split_bytes((header.e_phentsize * header.e_phnum) as usize)?;

        let mut parsed_phdrs = vec![];

        while !phdrs.is_empty() {
            let phdr;
            (phdr, phdrs) = phdrs.split_bytes(header.e_phentsize as usize)?;

            let phdr = match class {
                ElfClass::Elf32 => {
                    let (p_type, phdr) = phdr.split_u32()?;
                    let (p_offset, phdr) = phdr.split_u32()?;
                    let (p_vaddr, phdr) = phdr.split_u32()?;
                    let (p_paddr, phdr) = phdr.split_u32()?;
                    let (p_filesz, phdr) = phdr.split_u32()?;
                    let (p_memsz, phdr) = phdr.split_u32()?;
                    let (p_flags, phdr) = phdr.split_u32()?;
                    let (p_align, _) = phdr.split_u32()?;

                    Phdr {
                        p_type,
                        p_offset: Offset(p_offset as u64),
                        p_vaddr: Addr(p_vaddr as u64),
                        p_paddr: Addr(p_paddr as u64),
                        p_filesz: p_filesz as u64,
                        p_memsz: p_memsz as u64,
                        p_flags,
                        p_align: p_align as u64,
                    }
                }
                ElfClass::Elf64 => {
                    let (p_type, phdr) = phdr.split_u32()?;
                    let (p_flags, phdr) = phdr.split_u32()?;
                    let (p_offset, phdr) = phdr.split_u64()?;
                    let (p_vaddr, phdr) = phdr.split_u64()?;
                    let (p_paddr, phdr) = phdr.split_u64()?;
                    let (p_filesz, phdr) = phdr.split_u64()?;
                    let (p_memsz, phdr) = phdr.split_u64()?;
                    let (p_align, _) = phdr.split_u64()?;

                    Phdr {
                        p_type,
                        p_offset: Offset(p_offset),
                        p_vaddr: Addr(p_vaddr),
                        p_paddr: Addr(p_paddr),
                        p_filesz,
                        p_memsz,
                        p_flags,
                        p_align,
                    }
                }
            };
            parsed_phdrs.push(phdr);
        }

        Ok(parsed_phdrs)
    }
}

pub trait SplitAtCheckedErr {
    fn split_bytes(&self, split: usize) -> Result<(&[u8], &[u8])>;
    fn split_u16(&self) -> Result<(u16, &[u8])>;
    fn split_u32(&self) -> Result<(u32, &[u8])>;
    fn split_u64(&self) -> Result<(u64, &[u8])>;
}
impl SplitAtCheckedErr for [u8] {
    fn split_bytes(&self, mid: usize) -> Result<(&[u8], &[u8])> {
        if self.len() < mid {
            bail!("invalid file: too short");
        }
        Ok(self.split_at(mid))
    }
    fn split_u16(&self) -> Result<(u16, &[u8])> {
        let (bytes, rest) = self.split_bytes(2)?;
        Ok((u16::from_le_bytes(bytes.try_into().unwrap()), rest))
    }
    fn split_u32(&self) -> Result<(u32, &[u8])> {
        let (bytes, rest) = self.split_bytes(4)?;
        Ok((u32::from_le_bytes(bytes.try_into().unwrap()), rest))
    }
    fn split_u64(&self) -> Result<(u64, &[u8])> {
        let (bytes, rest) = self.split_bytes(8)?;
        Ok((u64::from_le_bytes(bytes.try_into().unwrap()), rest))
    }
}
