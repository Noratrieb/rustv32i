void _start(void)
{
    __asm__ volatile (
        "li a7, 93;" // exit
        "li a0, 0;" // code 0
        "ecall"
        :
        :
        : "a7", "a0", "memory"
    );
}
