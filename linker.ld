/* linker.ld - Linker script for kernel */

ENTRY(_start)

SECTIONS
{
    /* Kernel is loaded at 4KB by the bootloader */
    . = 0x1000;

    .text : {
        *(.text)
    }

    .rodata : {
        *(.rodata)
    }

    .data : {
        *(.data)
    }

    .bss : {
        *(.bss)
        *(COMMON)
    }
}