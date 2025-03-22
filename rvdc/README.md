RISC-V instruction decoder.

The main function is [`Inst::decode`], which will decode an instruction into the [`Inst`] enum.
The [`std::fmt::Display`] impl of [`Inst`] provides disassembly functionality
(note that the precise output of that implementation is not considered stable).

# Register size support

This crate currenly only supports RV32 instructions.
RV64 instructions that are the same between versions will still be decoded successfully, but the user
has to be careful around sign-extended immediates to preserve the correct value when extending them to 64 bits.

RV64-specific instructions are not yet implemented, but will be in the future.
The immediates will also be switched to `u64` in the future to allow for easier usage of RV64.

RV128 is not intended to be supported.

# Extension support

The decoder currently supports the following instructions:

- [x] Base RV32I instruction set
- [x] M standard extension
- [x] A standard extension
  - [x] Zalrsc standard extension
  - [x] Zaamo standard extension
- [x] C standard extension

More extensions may be implemented in the future.

# Examples

```rust
// addi sp, sp, -0x20 (compressed)
let x = 0x1101_u32;
let expected = rvdc::Inst::Addi { imm: (-0x20_i32) as u32, dest: rvdc::Reg::SP, src1: rvdc::Reg::SP };

let (inst, is_compressed) = rvdc::Inst::decode(x).unwrap();
assert_eq!(inst, expected);
assert_eq!(is_compressed, rvdc::IsCompressed::Yes);
assert_eq!(format!("{inst}"), "addi sp, sp, -32")
```

```rust
// auipc t1, 0xa
let x = 0x0000a317;
let expected = rvdc::Inst::Auipc { uimm: 0xa << 12, dest: rvdc::Reg::T1 };

let (inst, is_compressed) = rvdc::Inst::decode(x).unwrap();
assert_eq!(inst, expected);
assert_eq!(is_compressed, rvdc::IsCompressed::No);
assert_eq!(format!("{inst}"), "auipc t1, 10")
```

# Panics

[`Inst::decode`] is guaranteed to **never** panic. This is ensured with a 32-bit exhaustive test.

# Testing

This crate is currently tested as part of an emulator, which tests many different kinds of instructions.
In the future, more tests of the decoder specifically may be added.

# MSRV

This crate targets the latest stable as its MSRV.
