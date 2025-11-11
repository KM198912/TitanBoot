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
VIDEOMODE equ 0            ; 0 = try graphics, 1 = force text mode
VIDEOWIDTH equ 1024        ; Desired width in pixels
VIDEOHEIGHT equ 768        ; Desired height in pixels
VIDEODEPTH equ 32          ; Desired bits per pixel (24 or 32)

start:
    ; Initialize segment registers FIRST (critical!)
    ; print_string uses DS:SI, E820/VBE use ES:DI
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7E00      ; Stack just below loader
    sti
    
    ; Save boot drive (passed from Stage 1 in DL)
    mov [boot_drive], dl
    
    mov si, msg_loader
    call print_string

    ; Detect memory (also initializes boot_info structure)
    call detect_memory
    
    ; Find ACPI RSDP (must be after detect_memory initializes boot_info)
    call find_rsdp
    
    ; Set up VESA framebuffer
    call setup_framebuffer

    ; Load kernel from disk
    call load_kernel
    
    ; Load config file and modules
    call load_config

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

    ; Try LBA extended read first
    mov ah, 0x41        ; Check extensions present
    mov bx, 0x55AA
    mov dl, [boot_drive]
    int 0x13
    jc .use_chs         ; No LBA support
    cmp bx, 0xAA55
    jne .use_chs
    
    ; Use LBA extended read (INT 13h Extensions)
    mov si, kernel_dap
    mov ah, 0x42        ; Extended read
    mov dl, [boot_drive]
    int 0x13
    jnc .success        ; Success
    
.use_chs:
    ; Fall back to CHS read
    mov ah, 0x02        ; BIOS read
    mov al, 32          ; Read 32 sectors (16KB kernel max)
    mov ch, 0           ; Cylinder
    mov cl, 18          ; Start after boot + loader (sector 18)
    mov dh, 0           ; Head
    mov dl, [boot_drive]; Use saved boot drive
    
    ; Set up segment:offset for load address
    mov bx, KERNEL_OFFSET
    shr bx, 4           ; Convert to segment
    mov es, bx
    xor bx, bx          ; Offset 0
    int 0x13
    jc .error

.success:
    ret

.error:
    mov si, msg_error
    call print_string
    jmp $

; Find ACPI RSDP (Root System Description Pointer)
; RSDP signature is "RSD PTR " (8 bytes)
; Located in EBDA (Extended BIOS Data Area) or 0xE0000-0xFFFFF
find_rsdp:
    mov si, msg_rsdp
    call print_string
    
    push es
    push ds
    
    ; Set DS to 0 for accessing BIOS data area
    xor ax, ax
    mov ds, ax
    
    ; First, search EBDA (Extended BIOS Data Area)
    ; EBDA physical address is stored at 0x040E (word, in segments)
    mov ax, [0x040E]        ; Get EBDA segment
    test ax, ax
    jz .search_high         ; If 0, skip to high memory search
    
    ; EBDA found, search first 1KB
    mov es, ax
    xor di, di
    mov cx, 1024 / 16       ; Search first 1KB of EBDA (64 iterations)
    call .search_region
    cmp eax, 0
    jne .found
    
.search_high:
    ; Search 0xE0000 to 0xFFFFF (BIOS ROM area, 128KB)
    mov ax, 0xE000
    mov es, ax
    xor di, di
    mov cx, 0x20000 / 16    ; 128KB / 16 bytes per iteration = 8192 iterations
    call .search_region
    cmp eax, 0
    jne .found
    
    ; QEMU fallback: Check known SeaBIOS RSDP locations
    ; Try 0xF5AD0 (common location)
    mov ax, 0xF000
    mov es, ax
    mov di, 0x5AD0
    mov eax, [es:di]
    cmp eax, 0x20445352         ; "RSD "
    jne .try_alt1
    mov eax, [es:di+4]
    cmp eax, 0x20525450         ; "PTR "
    jne .try_alt1
    mov eax, 0xF5AD0
    jmp .found
    
.try_alt1:
    ; Try 0xF6C30 (alternate location)
    mov di, 0x6C30
    mov eax, [es:di]
    cmp eax, 0x20445352
    jne .try_alt2
    mov eax, [es:di+4]
    cmp eax, 0x20525450
    jne .try_alt2
    mov eax, 0xF6C30
    jmp .found
    
.try_alt2:
    ; Try 0xF0000 (another location)
    mov di, 0x0000
    mov eax, [es:di]
    cmp eax, 0x20445352
    jne .not_found
    mov eax, [es:di+4]
    cmp eax, 0x20525450
    jne .not_found
    mov eax, 0xF0000
    jmp .found
    
.not_found:
    ; Not found - restore registers and return
    pop ds
    pop es
    ret

.found:
    ; EAX contains the physical address of RSDP
    ; Store in boot_info structure (need ES=0 to access BOOT_INFO)
    push eax
    xor ax, ax
    mov es, ax
    pop eax
    
    mov [es:BOOT_INFO + 52], eax    ; rsdp_addr at offset 52
    or dword [es:BOOT_INFO], 0x2000 ; Set RSDP flag
    
    pop ds
    pop es
    ret

; Search for RSDP in ES:DI region
; CX = number of 16-byte paragraphs to search
; Returns: EAX = physical address if found, 0 if not found
.search_region:
   push si
    push di
    push cx
    push bx
    
.search_loop:
    ; Check for "RSD PTR " signature (8 bytes)
    mov eax, [es:di]
    cmp eax, 0x20445352         ; "RSD " (little-endian)
    jne .next
    mov eax, [es:di+4]
    cmp eax, 0x20525450         ; "PTR " (little-endian)
    jne .next
    
    ; Found signature! For debugging, skip checksum for now
    ; TODO: Re-enable checksum verification
    ; push cx
    ; push di
    ; xor bl, bl                  ; BL = checksum accumulator
    ; mov cx, 20                  ; RSDP 1.0 is 20 bytes
; .checksum_loop:
    ; mov al, [es:di]
    ; add bl, al
    ; inc di
    ; loop .checksum_loop
    ; pop di
    ; pop cx
    
    ; test bl, bl                 ; Checksum should be 0
    ; jnz .next
    
    ; Found valid RSDP! Calculate physical address
    xor eax, eax
    mov ax, es
    shl eax, 4
    movzx ebx, di
    add eax, ebx
    
    pop bx
    pop cx
    pop di
    pop si
    ret

.next:
    add di, 16                  ; RSDP is 16-byte aligned
    dec cx
    jnz .search_loop
    
    xor eax, eax                ; Not found
    pop bx
    pop cx
    pop di
    pop si
    ret

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
    ; New offsets: mmap_length at offset 28, mmap_addr at offset 32
    mov dword [BOOT_INFO], 0x00000041  ; MEM + MMAP flags
    mov [BOOT_INFO + 28], bp           ; mmap_length = number of entries (offset 28)
    mov dword [BOOT_INFO + 32], BOOT_INFO + 0x200  ; mmap_addr (offset 32)
    
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
    
    ; Set text mode framebuffer info (framebuffer starts at offset 36)
    mov dword [BOOT_INFO + 36], 0xB8000  ; framebuffer.address (offset 36)
    mov dword [BOOT_INFO + 40], 160      ; framebuffer.pitch (offset 40)
    mov dword [BOOT_INFO + 44], 80       ; framebuffer.width (offset 44)
    mov dword [BOOT_INFO + 48], 25       ; framebuffer.height (offset 48)
    mov byte  [BOOT_INFO + 52], 16       ; framebuffer.bpp (offset 52)
    mov byte  [BOOT_INFO + 53], 2        ; framebuffer.type = text mode (offset 53)
    
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
    
    ; Framebuffer structure now starts at offset 36 in boot_info
    ; Get framebuffer address (dword at offset 40)
    mov eax, [0x5200 + 40]
    mov [BOOT_INFO + 36], eax       ; framebuffer.address at offset 36
    
    ; Get pitch (word at offset 16)
    xor eax, eax
    mov ax, [0x5200 + 16]
    mov [BOOT_INFO + 40], eax       ; framebuffer.pitch at offset 40
    
    ; Get width (word at offset 18)
    xor eax, eax
    mov ax, [0x5200 + 18]
    mov [BOOT_INFO + 44], eax       ; framebuffer.width at offset 44
    
    ; Get height (word at offset 20)
    xor eax, eax
    mov ax, [0x5200 + 20]
    mov [BOOT_INFO + 48], eax       ; framebuffer.height at offset 48
    
    ; Get bpp (byte at offset 25)
    mov al, [0x5200 + 25]
    mov [BOOT_INFO + 52], al        ; framebuffer.bpp at offset 52
    
    ; Set type to RGB (1)
    mov byte [BOOT_INFO + 53], 1    ; framebuffer.type at offset 53
    
    clc                 ; Clear carry = success
    ret

.fail:
    stc                 ; Set carry = failure
    ret

; Load config file and modules
; Config file is at sector 2+16+32 = 50 (after boot, loader, and kernel)
; Format: simple text file with module directives
load_config:
    mov si, msg_config
    call print_string
    
    ; Load config file (1 sector) to temporary buffer at 0x5000
    ; Try LBA first
    mov ah, 0x41
    mov bx, 0x55AA
    mov dl, [boot_drive]
    int 0x13
    jc .use_chs_config
    cmp bx, 0xAA55
    jne .use_chs_config
    
    ; Use LBA to load config
    mov si, config_dap
    mov ah, 0x42
    mov dl, [boot_drive]
    int 0x13
    jc .no_config       ; Config load failed, skip modules
    jmp .parse_config
    
.use_chs_config:
    ; Use CHS to load config
    mov ah, 0x02
    mov al, 1           ; Read 1 sector
    mov ch, 0
    mov cl, 50          ; Sector 50
    mov dh, 0
    mov dl, [boot_drive]
    mov bx, 0x500       ; Segment
    mov es, bx
    xor bx, bx          ; Offset 0
    int 0x13
    jc .no_config
    
.parse_config:
    ; Parse config file at 0x5000
    ; Initialize module count and storage
    mov word [module_count], 0
    mov dword [BOOT_INFO + 20], 0  ; mods_count at offset 20
    mov dword [BOOT_INFO + 24], MODULE_INFO_ADDR  ; mods_addr at offset 24
    
    ; Point to start of config buffer
    mov si, 0x5000
    mov di, MODULE_INFO_ADDR
    
.parse_line:
    ; Skip whitespace and comments
    call skip_whitespace
    cmp byte [si], 0
    je .done_parsing
    cmp si, 0x5200      ; End of 512-byte buffer
    jge .done_parsing
    
    ; Check for "module" keyword
    mov bx, si
    mov cx, 6
    mov di, str_module
    call compare_string
    jc .not_module
    
    ; Found "module" - parse: module <start_sector> <num_sectors> <name>
    add si, 6           ; Skip "module"
    call skip_whitespace
    
    ; Parse start sector (decimal number)
    call parse_number
    mov [temp_start_sector], ax
    
    call skip_whitespace
    
    ; Parse number of sectors
    call parse_number
    mov [temp_num_sectors], ax
    
    call skip_whitespace
    
    ; Rest of line is module name - store pointer
    mov [temp_name_ptr], si
    
    ; Load this module
    call load_single_module
    
.not_module:
    ; Skip to next line
    call skip_to_newline
    jmp .parse_line
    
.done_parsing:
    ; Update boot_info flags if we loaded any modules
    cmp word [module_count], 0
    je .no_modules
    
    ; Set module count in boot_info
    xor eax, eax
    mov ax, [module_count]
    mov [BOOT_INFO + 20], eax       ; mods_count at offset 20
    
    ; Set modules flag
    or dword [BOOT_INFO], 0x08
    
.no_modules:
.no_config:
    ret

; Load a single module based on temp variables
; temp_start_sector, temp_num_sectors are set
load_single_module:
    ; Calculate load address (modules start at 0x100000 - 1MB)
    xor eax, eax
    mov ax, [module_count]
    mov ebx, 0x10000    ; 64KB per module max
    mul ebx
    add eax, 0x100000   ; Start at 1MB
    mov [temp_load_addr], eax
    
    ; Create module info entry
    mov di, MODULE_INFO_ADDR
    xor eax, eax
    mov ax, [module_count]
    mov bx, 16          ; Each module_info_t is 16 bytes
    mul bx
    add di, ax
    
    ; Fill module_info_t structure
    mov eax, [temp_load_addr]
    mov [di + 0], eax       ; mod_start
    
    xor eax, eax
    mov ax, [temp_num_sectors]
    mov bx, 512
    mul bx                  ; EAX = size in bytes
    add eax, [temp_load_addr]
    mov [di + 4], eax       ; mod_end
    
    mov eax, [temp_name_ptr]
    mov [di + 8], eax       ; string pointer
    
    mov dword [di + 12], 0  ; reserved
    
    ; Note: Actual module loading from disk requires protected mode
    ; and setting up proper memory access above 1MB
    ; For now, we just create the module info structure
    ; The kernel will need to handle actual module loading
    
    ; Increment module count
    inc word [module_count]
    
    ret

; Helper: Skip whitespace (space, tab)
skip_whitespace:
    cmp byte [si], ' '
    je .skip
    cmp byte [si], 9    ; Tab
    je .skip
    ret
.skip:
    inc si
    jmp skip_whitespace

; Helper: Skip to newline or end
skip_to_newline:
    cmp byte [si], 0
    je .done
    cmp byte [si], 0x0A ; LF
    je .found
    cmp byte [si], 0x0D ; CR
    je .found
    inc si
    jmp skip_to_newline
.found:
    inc si
    ; Skip additional CR/LF
    cmp byte [si], 0x0A
    jne .done
    inc si
.done:
    ret

; Helper: Compare string
; BX = string 1, DI = string 2, CX = length
; Returns: CF=0 if match, CF=1 if no match
compare_string:
    push si
    mov si, bx
.loop:
    mov al, [si]
    cmp al, [di]
    jne .nomatch
    inc si
    inc di
    loop .loop
    clc
    pop si
    ret
.nomatch:
    stc
    pop si
    ret

; Helper: Parse decimal number from [SI], return in AX
parse_number:
    xor ax, ax
    xor bx, bx
.loop:
    mov bl, [si]
    cmp bl, '0'
    jb .done
    cmp bl, '9'
    ja .done
    sub bl, '0'
    mov cx, 10
    mul cx
    add ax, bx
    inc si
    jmp .loop
.done:
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
msg_rsdp: db 'Finding ACPI RSDP...', 0x0D, 0x0A, 0
msg_memory: db 'Detecting memory...', 0x0D, 0x0A, 0
msg_vesa: db 'Setting up graphics...', 0x0D, 0x0A, 0
msg_vbe_fallback: db 'VBE failed, using text mode', 0x0D, 0x0A, 0
msg_kernel: db 'Loading kernel...', 0x0D, 0x0A, 0
msg_config: db 'Loading config...', 0x0D, 0x0A, 0
msg_error: db 'Error loading kernel!', 0x0D, 0x0A, 0
boot_drive: db 0
vbe_exact_match: db 0

; Module loading data
MODULE_INFO_ADDR equ 0x8000  ; Module info array at 0x8000
module_count: dw 0
temp_start_sector: dw 0
temp_num_sectors: dw 0
temp_load_addr: dd 0
temp_name_ptr: dd 0
str_module: db 'module'

; Kernel DAP for LBA reads
align 4
kernel_dap:
    db 0x10, 0          ; Size of DAP (16 bytes), Reserved
    dw 32               ; Number of sectors to read (16KB kernel max)
    dw 0                ; Buffer offset (0)
    dw (KERNEL_OFFSET >> 4)  ; Buffer segment (0x1000 >> 4 = 0x100)
    dq 17               ; LBA start sector (after boot sector + 16 loader sectors)

; Config DAP for LBA reads
align 4
config_dap:
    db 0x10, 0          ; Size of DAP
    dw 1                ; Read 1 sector (512 bytes config)
    dw 0                ; Buffer offset
    dw 0x500            ; Buffer segment (0x5000)
    dq 50               ; LBA sector 50 (after boot + loader + kernel)

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