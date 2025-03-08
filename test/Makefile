CC = clang -Wall -Wpedantic -target riscv32-unknown-linux-gnu -fuse-ld=lld -march=rv32i
CC_STATIC = $(CC) -static -nostdlib -nodefaultlibs

init: init.c
	$(CC_STATIC) init.c -o init
