// kernel.c - Main OS kernel
// This is loaded and called by entry.c after system initialization

#include <stdint.h>
#include "boot_info.h"

// This is the real kernel entry point
// Called from entry.c after hardware setup and module loading
void kernel_main(boot_info_t* boot_info) {
    // TODO: Implement your OS kernel here
    // For now, just hang
    while(1) {
        __asm__ __volatile__("hlt");
    }
}
