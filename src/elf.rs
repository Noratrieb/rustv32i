use eyre::{Result, bail};

pub struct Elf<'a> {
    pub content: &'a [u8],
}

#[derive(Debug)]
pub struct Header {
    pub e_entry: u32,
    pub e_phoff: u32,
    pub e_shoff: u32,
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
    pub p_offset: u32,
    pub p_vaddr: u32,
    pub p_paddr: u32,
    pub p_filesz: u32,
    pub p_memsz: u32,
    pub p_flags: u32,
    pub p_align: u32,
}

impl<'a> Elf<'a> {
    pub fn header(&self) -> Result<Header> {
        let (ident, rest) = self.content.split_bytes(16)?;
        if ident[..4] != *b"\x7fELF" {
            bail!("not an elf file (invalid magic)");
        }
        // ELFCLASS32
        if ident[5] != 1 {
            bail!("not a ELF32 file (EI_CLASS={})", ident[5]);
        }

        let (e_type, rest) = rest.split_u16()?;
        // ET_EXEC|ET_DYN
        if e_type != 2 && e_type != 3 {
            bail!("not an executable: {e_type}");
        }

        let (e_machine, rest) = rest.split_u16()?;
        // EM_RISCV
        if e_machine != 243 {
            bail!("not a RISC-V executable");
        }

        let (_e_version, rest) = rest.split_u32()?;

        let (e_entry, rest) = rest.split_u32()?;
        let (e_phoff, rest) = rest.split_u32()?;
        let (e_shoff, rest) = rest.split_u32()?;
        let (e_flags, rest) = rest.split_u32()?;
        let (e_ehsize, rest) = rest.split_u16()?;
        let (e_phentsize, rest) = rest.split_u16()?;
        let (e_phnum, rest) = rest.split_u16()?;
        let (e_shentsize, rest) = rest.split_u16()?;
        let (e_shnum, rest) = rest.split_u16()?;
        let (e_shstrndx, _) = rest.split_u16()?;

        Ok(Header {
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

        let (_, phdrs) = self.content.split_bytes(header.e_phoff as usize)?;
        let (mut phdrs, _) = phdrs.split_bytes((header.e_phentsize * header.e_phnum) as usize)?;

        let mut parsed_phdrs = vec![];

        while !phdrs.is_empty() {
            let phdr;
            (phdr, phdrs) = phdrs.split_bytes(header.e_phentsize as usize)?;

            let (p_type, phdr) = phdr.split_u32()?;
            let (p_offset, phdr) = phdr.split_u32()?;
            let (p_vaddr, phdr) = phdr.split_u32()?;
            let (p_paddr, phdr) = phdr.split_u32()?;
            let (p_filesz, phdr) = phdr.split_u32()?;
            let (p_memsz, phdr) = phdr.split_u32()?;
            let (p_flags, phdr) = phdr.split_u32()?;
            let (p_align, _) = phdr.split_u32()?;

            parsed_phdrs.push(Phdr {
                p_type,
                p_offset,
                p_vaddr,
                p_paddr,
                p_filesz,
                p_memsz,
                p_flags,
                p_align,
            });
        }

        Ok(parsed_phdrs)
    }
}

pub trait SplitAtCheckedErr {
    fn split_bytes(&self, split: usize) -> Result<(&[u8], &[u8])>;
    fn split_u16(&self) -> Result<(u16, &[u8])>;
    fn split_u32(&self) -> Result<(u32, &[u8])>;
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
}
