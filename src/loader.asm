; loader.asm - Stage 2 Loader
; Loaded at 0x7E00 by boot sector
; Switches to protected mode and loads kernel
; Gathers boot info (memory map, framebuffer)

[BITS 16]
[ORG 0x7E00]

KERNEL_OFFSET equ 0x1000   ; Load kernel at 0x1000 (4KB)
BOOT_INFO equ 0x6000       ; Boot info structure at 0x6000 (safe location)

; Video mode configuration (like Multiboot)
; Change these constants to set your desired video mode:
; Common resolutions: 640x480, 800x600, 1024x768, 1280x1024, 1920x1080
; Set VIDEOMODE=1 to force text mode (disables graphics)
VIDEOMODE equ 1            ; 0 = try graphics, 1 = force text mode
VIDEOWIDTH equ 1024        ; Desired width in pixels
VIDEOHEIGHT equ 768        ; Desired height in pixels
VIDEODEPTH equ 32          ; Desired bits per pixel (24 or 32)

start:
    ; Save boot drive (passed from Stage 1 in DL)
    mov [boot_drive], dl
    
    mov si, msg_loader
    call print_string

    ; Detect memory
    call detect_memory
    
    ; Set up VESA framebuffer
    call setup_framebuffer

    ; Load kernel from disk
    call load_kernel

    ; Enable A20 line (allows access to memory above 1MB)
    call enable_a20

    ; Switch to protected mode
    cli
    lgdt [gdt_descriptor]
    mov eax, cr0
    or eax, 1
    mov cr0, eax

    ; Far jump to set CS and enter 32-bit mode
    jmp CODE_SEG:protected_mode

; Load kernel from disk into memory
load_kernel:
    mov si, msg_kernel
    call print_string

    mov ah, 0x02            ; BIOS read
    mov al, 32              ; Read 32 sectors (16KB kernel max)
    mov ch, 0               ; Cylinder
    mov cl, 18              ; Start after boot + loader (sector 18)
    mov dh, 0               ; Head
    mov dl, [boot_drive]    ; Use saved boot drive
    
    ; Set up segment:offset for load address
    mov bx, KERNEL_OFFSET
    shr bx, 4               ; Convert to segment
    mov es, bx
    xor bx, bx              ; Offset 0
    int 0x13

    jc .error
    ret

.error:
    mov si, msg_error
    call print_string
    jmp $

; Detect memory using BIOS INT 0x15, E820
detect_memory:
    mov si, msg_memory
    call print_string
    
    ; Initialize boot info structure
    push es
    xor ax, ax
    mov es, ax
    mov di, BOOT_INFO
    xor ax, ax
    mov cx, 256
    rep stosw
    pop es
    
    ; E820 Memory map destination (after boot_info struct)
    ; boot_info is 512 bytes max, so start E820 entries at BOOT_INFO + 0x200
    mov di, BOOT_INFO + 0x200
    xor ebx, ebx          ; EBX = continuation value (start at 0)
    xor bp, bp            ; BP = entry count
    mov edx, 0x534D4150   ; "SMAP" signature
    
.e820_loop:
    mov eax, 0xE820       ; E820 function
    mov ecx, 24           ; Buffer size (24 bytes for entry)
    int 0x15
    
    jc .e820_done         ; Carry flag = error or done
    
    cmp eax, 0x534D4150   ; Verify "SMAP" signature
    jne .e820_done
    
    ; Entry stored at ES:DI (already at correct location)
    ; Move to next entry
    add di, 24            ; Each E820 entry is 24 bytes
    inc bp                ; Increment entry count
    
    test ebx, ebx         ; EBX = 0 means last entry
    jz .e820_done
    
    cmp bp, 32            ; Limit to 32 entries max
    jge .e820_done
    
    jmp .e820_loop

.e820_done:
    ; Store E820 memory map info in boot_info
    mov dword [BOOT_INFO], 0x00000041  ; MEM + MMAP flags
    mov [BOOT_INFO + 16], bp           ; mmap_length = number of entries (word, but stored as dword)
    mov dword [BOOT_INFO + 20], BOOT_INFO + 0x200  ; mmap_addr
    
    ; Also fill legacy mem_lower/mem_upper for compatibility
    ; Get lower memory (0-640KB)
    clc
    int 0x12
    jc .no_lower
    mov [BOOT_INFO + 4], ax  ; mem_lower in KB
.no_lower:
    
    ; Get extended memory (simple method)
    mov ah, 0x88
    int 0x15
    jc .no_upper
    mov [BOOT_INFO + 8], ax  ; mem_upper in KB
.no_upper:
    
    ret

; Setup VESA framebuffer
setup_framebuffer:
    mov si, msg_vesa
    call print_string
    
    ; Try to set VBE mode first
    call setup_vbe
    jnc .done           ; If VBE succeeded, we're done
    
    ; Fallback to VGA text mode if VBE fails
    mov si, msg_vbe_fallback
    call print_string
    
    mov ax, 0x0003
    int 0x10
    
    ; Set framebuffer flag
    or dword [BOOT_INFO], 0x1000
    
    ; Set text mode framebuffer info (correct offsets matching boot_info.h)
    mov dword [BOOT_INFO + 24], 0xB8000  ; framebuffer.address
    mov dword [BOOT_INFO + 28], 160      ; framebuffer.pitch (80 chars * 2 bytes)
    mov dword [BOOT_INFO + 32], 80       ; framebuffer.width
    mov dword [BOOT_INFO + 36], 25       ; framebuffer.height
    mov byte  [BOOT_INFO + 40], 16       ; framebuffer.bpp (16 bits per char)
    mov byte  [BOOT_INFO + 41], 2        ; framebuffer.type = text mode
    
.done:
    ret

; Setup VBE (VESA BIOS Extensions) graphics mode
; Returns: CF=0 on success, CF=1 on failure
setup_vbe:
%if VIDEOMODE != 0
    ; Video mode forced to text, skip VBE
    jmp .fail
%endif

    ; Get VBE controller info
    mov ax, 0x4F00
    mov di, 0x5000      ; Temporary buffer for VBE info
    int 0x10
    cmp ax, 0x004F
    jne .fail
    
    ; Get pointer to mode list (offset 14 in VBE info block)
    mov si, [0x5000 + 14]    ; Offset of mode list pointer
    mov ax, [0x5000 + 16]    ; Segment of mode list pointer
    mov es, ax
    
    ; First pass: try to find exact BPP match
    mov byte [vbe_exact_match], 1
    
.check_mode:
    ; Load next mode number
    mov cx, [es:si]
    cmp cx, 0xFFFF           ; End of list?
    je .try_fallback
    
    ; Get mode info
    push si
    push es
    mov ax, 0x4F01
    mov di, 0x5200           ; Mode info buffer
    push cx
    int 0x10
    pop cx
    pop es
    pop si
    
    cmp ax, 0x004F
    jne .next_mode
    
    ; Check if mode matches our requirements
    ; VBE ModeInfoBlock at 0x5200:
    ; +0: mode attributes (bit 7 = linear framebuffer supported)
    ; +18: width
    ; +20: height  
    ; +25: bits per pixel
    
    ; Check mode attributes (must support linear FB - bit 7)
    mov ax, [0x5200 + 0]
    test ax, 0x80
    jz .next_mode
    
    ; Check width
    mov ax, [0x5200 + 18]
    cmp ax, VIDEOWIDTH
    jne .next_mode
    
    ; Check height
    mov ax, [0x5200 + 20]
    cmp ax, VIDEOHEIGHT
    jne .next_mode
    
    ; Check bpp
    mov al, [0x5200 + 25]
    cmp byte [vbe_exact_match], 1
    je .check_exact
    
    ; Second pass: accept VIDEODEPTH-8 as fallback
    cmp al, VIDEODEPTH - 8
    je .found_mode
    jmp .next_mode
    
.check_exact:
    ; First pass: only accept exact VIDEODEPTH
    cmp al, VIDEODEPTH
    je .found_mode
    
.next_mode:
    add si, 2                ; Next mode number (word)
    jmp .check_mode

.try_fallback:
    ; If first pass failed, try again accepting VIDEODEPTH-8
    cmp byte [vbe_exact_match], 1
    jne .fail                ; Already tried fallback
    
    mov byte [vbe_exact_match], 0
    mov si, [0x5000 + 14]    ; Reset to start of mode list
    mov ax, [0x5000 + 16]
    mov es, ax
    jmp .check_mode

.found_mode:
    ; Set this mode with linear framebuffer
    mov ax, 0x4F02
    mov bx, cx
    or bx, 0x4000            ; Bit 14 = use linear framebuffer
    int 0x10
    cmp ax, 0x004F
    jne .fail
    
    ; Mode info is already in buffer at 0x5200
    jmp .set_info

.set_info:
    ; Reset ES
    xor ax, ax
    mov es, ax
    
    ; Set framebuffer flag
    or dword [BOOT_INFO], 0x1000
    
    ; VBE ModeInfoBlock structure (at 0x5200):
    ; +0: mode attributes
    ; +16: bytes per scanline (pitch)
    ; +18: width in pixels
    ; +20: height in pixels
    ; +25: bits per pixel
    ; +40: physical base pointer (framebuffer address)
    
    ; Get framebuffer address (dword at offset 40)
    mov eax, [0x5200 + 40]
    mov [BOOT_INFO + 24], eax
    
    ; Get pitch (word at offset 16)
    xor eax, eax
    mov ax, [0x5200 + 16]
    mov [BOOT_INFO + 28], eax
    
    ; Get width (word at offset 18)
    xor eax, eax
    mov ax, [0x5200 + 18]
    mov [BOOT_INFO + 32], eax
    
    ; Get height (word at offset 20)
    xor eax, eax
    mov ax, [0x5200 + 20]
    mov [BOOT_INFO + 36], eax
    
    ; Get bpp (byte at offset 25)
    mov al, [0x5200 + 25]
    mov [BOOT_INFO + 40], al
    
    ; Set type to RGB (1)
    mov byte [BOOT_INFO + 41], 1
    
    clc                 ; Clear carry = success
    ret

.fail:
    stc                 ; Set carry = failure
    ret

; Enable A20 gate via keyboard controller
enable_a20:
    in al, 0x92
    or al, 2
    out 0x92, al
    ret

; Print string in real mode (before protected mode)
print_string:
    pusha
.loop:
    lodsb
    test al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp .loop
.done:
    popa
    ret

; GDT (Global Descriptor Table)
gdt_start:
    ; Null descriptor
    dq 0

gdt_code:
    ; Code segment descriptor
    dw 0xFFFF       ; Limit (bits 0-15)
    dw 0x0000       ; Base (bits 0-15)
    db 0x00         ; Base (bits 16-23)
    db 10011010b    ; Access byte (present, ring 0, code, executable, readable)
    db 11001111b    ; Flags (4-bit) + Limit (bits 16-19)
    db 0x00         ; Base (bits 24-31)

gdt_data:
    ; Data segment descriptor
    dw 0xFFFF       ; Limit
    dw 0x0000       ; Base
    db 0x00         ; Base
    db 10010010b    ; Access (present, ring 0, data, writable)
    db 11001111b    ; Flags + Limit
    db 0x00         ; Base

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Size
    dd gdt_start                 ; Offset

CODE_SEG equ gdt_code - gdt_start
DATA_SEG equ gdt_data - gdt_start

; Data
msg_loader: db 'Stage 2 loaded', 0x0D, 0x0A, 0
msg_memory: db 'Detecting memory...', 0x0D, 0x0A, 0
msg_vesa: db 'Setting up graphics...', 0x0D, 0x0A, 0
msg_vbe_fallback: db 'VBE failed, using text mode', 0x0D, 0x0A, 0
msg_kernel: db 'Loading kernel...', 0x0D, 0x0A, 0
msg_error: db 'Error loading kernel!', 0x0D, 0x0A, 0
boot_drive: db 0
vbe_exact_match: db 0

; --- 32-bit Protected Mode ---
[BITS 32]
protected_mode:
    ; Set up segment registers
    mov ax, DATA_SEG
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov fs, ax
    mov gs, ax

    ; Set up stack
    mov esp, 0x90000

    ; Clear screen in protected mode
    mov edi, 0xB8000
    mov ecx, 80*25
    mov ax, 0x0F20      ; White space
    rep stosw

    ; Print to screen directly (VGA text mode at 0xB8000)
    mov esi, msg_protected
    mov edi, 0xB8000
    mov ah, 0x0F        ; White on black
.print_loop:
    lodsb
    test al, al
    jz .done
    stosw
    jmp .print_loop
.done:

    ; Small delay to see the message
    mov ecx, 0x2FFFFFF
.delay:
    loop .delay

    ; Load boot info pointer into EAX to pass to kernel
    mov eax, 0x6000
    
    ; Jump to kernel (we'll pass the parameter there)
    jmp KERNEL_OFFSET
    
    ; If kernel returns, show error
    mov esi, msg_kernel_ret
    mov edi, 0xB8000
    add edi, 160        ; Next line
    mov ah, 0x0C        ; Red on black
.error_loop:
    lodsb
    test al, al
    jz .hang
    stosw
    jmp .error_loop
.hang:
    jmp $

msg_protected: db 'Protected mode OK. Jumping to kernel...', 0
msg_kernel_ret: db 'ERROR: Kernel returned!', 0

; Padding to make this 8KB (16 sectors)
times 8192-($-$$) db 0