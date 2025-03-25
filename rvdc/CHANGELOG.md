## 2.0.0

- BREAKING CHANGE: Make `Inst` `#[non_exhaustive]`
- BREAKING CHANGE: Change immediate fields in `Inst` to `Imm`

## 0.1.1

- Add `Fence::is_tso`
- Several fixes to disassembly
- Forbid overflow for `slli` immediates
- Add `Zihintpause` extension to README (it was already implemented in disassembly)
- Add exhaustive tests

## 0.1.0

Initial release.
