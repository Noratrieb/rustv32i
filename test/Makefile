CC = clang -Wall -Wpedantic -target riscv32-unknown-linux-gnu -fuse-ld=lld -march=rv32imac
CC_STATIC = $(CC) -static -nostdlib -nodefaultlibs

RUSTC = rustc --target riscv32imac-unknown-none-elf

all: init init1 x

init: init.c
	$(CC_STATIC) init.c -o init

init1: init1.rs
	$(RUSTC) init1.rs

x: x.s
	$(CC_STATIC) x.s -o x