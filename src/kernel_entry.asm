; kernel_entry.asm - Assembly entry point for bootstrap
; This ensures the system entry starts at a known location

[BITS 32]
[EXTERN entry_main]

; Entry point
global _start
_start:
    ; Boot info pointer is in EAX (passed from loader)
    ; Push it as parameter for entry_main
    push eax
    
    ; Call the C bootstrap/entry with boot_info as parameter
    call entry_main
    
    ; If entry_main returns (it shouldn't), hang
    cli
.hang:
    hlt
    jmp .hang