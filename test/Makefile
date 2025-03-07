CC = clang -target riscv32-unknown-linux-gnu -fuse-ld=lld -march=rv32i
CC_STATIC = $(CC) -static -nostdlib -nodefaultlibs

test: test.c
	$(CC_STATIC) test.c -o test
