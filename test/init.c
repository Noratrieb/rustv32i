// <https://jborza.com/post/2021-05-11-riscv-linux-syscalls/>

_Noreturn void __attribute__ ((noinline)) exit(int code)
{
    __asm__ volatile(
        "li a7, 93;" // exit
        "mv a0, %0;"  // code
        "ecall"
        :
        : "r"(code)
        : "a7", "a0", "memory"
    );
    __builtin_unreachable();
}

_Noreturn void _start(void)
{
    
    exit(10);
}
