; kernel_entry.asm - Assembly entry point for kernel
; This ensures the kernel starts at a known location

[BITS 32]
[EXTERN kernel_main]

; Entry point
global _start
_start:
    ; Boot info pointer is in EAX (passed from loader)
    ; Push it as parameter for kernel_main
    push eax
    
    ; Call the C kernel with boot_info as parameter
    call kernel_main
    
    ; If kernel_main returns (it shouldn't), hang
    cli
.hang:
    hlt
    jmp .hang