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
    ; +31: red mask size
    ; +32: red field position
    ; +33: green mask size
    ; +34: green field position
    ; +35: blue mask size
    ; +36: blue field position
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
    
    ; Get RGB mask information
    ; red_position (offset 54)
    mov al, [0x5200 + 32]
    mov [BOOT_INFO + 54], al
    
    ; red_mask_size (offset 55)
    mov al, [0x5200 + 31]
    mov [BOOT_INFO + 55], al
    
    ; green_position (offset 56)
    mov al, [0x5200 + 34]
    mov [BOOT_INFO + 56], al
    
    ; green_mask_size (offset 57)
    mov al, [0x5200 + 33]
    mov [BOOT_INFO + 57], al
    
    ; blue_position (offset 58)
    mov al, [0x5200 + 36]
    mov [BOOT_INFO + 58], al
    
    ; blue_mask_size (offset 59)
    mov al, [0x5200 + 35]
    mov [BOOT_INFO + 59], al
    
    clc                 ; Clear carry = success
    ret

.fail:
    stc                 ; Set carry = failure
    ret

; Load config file and modules
; Load config file - supports both raw disk and ISO9660
; For raw disk: config at sector 50
; For ISO9660: search for BOOT.CFG in root directory
load_config:
    mov si, msg_config
    call print_string
    
    ; Store drive number
    mov al, [boot_drive]
    mov [BOOT_INFO + 60], al        ; drive_number
    
    ; Try to detect if we're booting from ISO9660
    ; Check if we can read Primary Volume Descriptor at sector 16
    mov ah, 0x41
    mov bx, 0x55AA
    mov dl, [boot_drive]
    int 0x13
    jc .try_raw_disk                ; No LBA support
    cmp bx, 0xAA55
    jne .try_raw_disk
    
    ; Use LBA to read PVD
    mov si, iso_check_dap
    mov ah, 0x42
    mov dl, [boot_drive]
    int 0x13
    jc .try_raw_disk
    
    ; Check for ISO9660 signature "CD001" at offset 1
    mov bx, 0x5000 + 1
    mov di, iso_signature
    mov cx, 5
    call compare_string
    jnc .is_iso
    
.try_raw_disk:
    ; Not ISO or LBA failed, try raw disk approach
    ; Boot media info already set based on drive number
    mov word [BOOT_INFO + 62], 0    ; reserved
    mov dword [BOOT_INFO + 64], 0   ; iso_root_lba (not used)
    mov dword [BOOT_INFO + 68], 0   ; iso_root_size (not used)
    or dword [BOOT_INFO], 0x2000    ; Set BOOT_INFO_FLAG_MEDIA
    
    ; For raw disk, load INITRD module directly here in real mode
    ; This is simpler and more reliable than trying later
    mov word [module_count], 0
    mov dword [BOOT_INFO + 20], 0   ; mods_count at offset 20
    mov dword [BOOT_INFO + 24], MODULE_INFO_ADDR  ; mods_addr at offset 24
    
    ; Load INITRD.IMG to 0x50000 (320KB) - safe high memory area
    ; Use simple CHS mode (more compatible)
    push ds
    push es
    
    ; TEST: Write directly to 0x7000 to see if it survives to protected mode
    mov word [0x7000], 0xBEEF
    mov word [0x7002], 0xCAFE
    
    ; Read sector 100 to 0x1000 instead of 0x5000 (test different address)
    mov dword [config_dap + 8], 100    ; Change LBA to 100
    mov word [config_dap + 4], 0      ; Buffer offset = 0
    mov word [config_dap + 6], 0x100   ; Buffer segment = 0x100 (address 0x1000)
    
    ; Debug: Write DAP values to memory so we can inspect them
    mov eax, [config_dap + 0]    ; Size + sector count
    mov [0x7010], eax
    mov eax, [config_dap + 4]    ; Offset + segment
    mov [0x7014], eax
    mov eax, [config_dap + 8]    ; LBA low
    mov [0x7018], eax
    mov eax, [config_dap + 12]   ; LBA high
    mov [0x701C], eax
    
    mov si, config_dap
    mov ah, 0x42        ; Extended read
    mov dl, [boot_drive]
    int 0x13
    
    jc .read_failed
    
    ; Success marker
    mov word [0x7004], 0x00AA
    jmp .read_done
    
.read_failed:
    ; Failure marker
    mov word [0x7004], 0x00FF
    
.read_done:
    
    ; Restore segments (in reverse order)
    pop es
    pop ds
    
    ; Create module info entry (pointing to 0x1000 now)
    mov di, MODULE_INFO_ADDR
    mov dword [di + 0], 0x1000      ; mod_start at 0x1000
    mov dword [di + 4], 0x1200      ; mod_end
    mov dword [di + 8], hardcoded_initrd_name ; string
    mov dword [di + 12], 0          ; reserved
    inc word [module_count]
    
    ; Update boot_info
    mov dword [BOOT_INFO + 20], 1   ; mods_count = 1
    or dword [BOOT_INFO], 0x08      ; Set modules flag
    
    jmp .no_config
    
.try_raw_disk_with_config:
    ; Alternative: Try to load config file (requires knowing sector)
    ; Load config file (1 sector) to temporary buffer at 0x5000
    mov ah, 0x41
    mov bx, 0x55AA
    mov dl, [boot_drive]
    int 0x13
    jc .use_chs_config
    cmp bx, 0xAA55
    jne .use_chs_config
    
    ; Use LBA to load config from sector 50
    mov si, config_dap
    mov ah, 0x42
    mov dl, [boot_drive]
    int 0x13
    jc .no_config
    jmp .parse_config
    
.use_chs_config:
    ; Use CHS to load config
    mov ah, 0x02
    mov al, 1
    mov ch, 0
    mov cl, 50
    mov dh, 0
    mov dl, [boot_drive]
    mov bx, 0x500
    mov es, bx
    xor bx, bx
    int 0x13
    jc .no_config
    jmp .parse_config
    
.is_iso:
    ; We have an ISO9660 filesystem
    mov byte [using_iso], 1
    mov byte [BOOT_INFO + 61], 1    ; boot_type = ISO9660
    
    ; Root directory location is at offset 158 (LSB LBA) in PVD
    mov eax, [0x5000 + 158]  ; Root directory LBA
    mov [iso_root_lba], eax
    mov [BOOT_INFO + 64], eax       ; Store in boot_info
    
    mov eax, [0x5000 + 166]  ; Root directory size
    mov [iso_root_size], eax
    mov [BOOT_INFO + 68], eax       ; Store in boot_info
    
    mov word [BOOT_INFO + 62], 0    ; reserved
    or dword [BOOT_INFO], 0x2000    ; Set BOOT_INFO_FLAG_MEDIA
    
    ; Now search for BOOT.CFG in root directory
    mov si, filename_bootcfg
    call iso_find_file
    jc .no_config           ; File not found
    
    ; File found, sector in EAX, load it
    mov [temp_file_lba], eax
    mov si, file_load_dap
    mov eax, [temp_file_lba]
    mov [file_load_dap + 8], eax
    mov ah, 0x42
    mov dl, [boot_drive]
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
    
    ; Check for comment line
    cmp byte [si], '#'
    je .not_module      ; Skip comment lines
    cmp byte [si], 0x0D ; Empty line (CR)
    je .not_module
    cmp byte [si], 0x0A ; Empty line (LF)
    je .not_module
    
    ; Check for "module" keyword
    mov bx, si
    mov cx, 6
    mov di, str_module
    call compare_string
    jc .not_module
    
    ; Found "module" - parse parameters
    add si, 6           ; Skip "module"
    call skip_whitespace
    
    ; Debug: Write marker byte to 0x7000 to show we found a module
    mov byte [0x7000], 0xAA
    
    ; Print debug message
    push si
    mov si, msg_module_found
    call print_string
    pop si
    
    ; Check if we're using ISO9660
    cmp byte [using_iso], 1
    je .parse_iso_module
    
    ; For raw disk: parse "module <start_sector> <num_sectors> <name>"
.parse_raw_module:
    ; Parse start sector (decimal)
    call parse_decimal
    jc .not_module      ; Parse error
    mov [temp_start_sector], eax
    call skip_whitespace
    
    ; Parse num_sectors (decimal)
    call parse_decimal
    jc .not_module      ; Parse error
    mov [temp_num_sectors], ax
    call skip_whitespace
    
    ; Rest of line is the module name
    mov [temp_name_ptr], si
    
    ; Find line end to null-terminate name
    mov di, si
.find_eol_raw:
    mov al, [di]
    cmp al, 0x0D        ; CR
    je .found_eol_raw
    cmp al, 0x0A        ; LF
    je .found_eol_raw
    cmp al, 0
    je .found_eol_raw
    inc di
    jmp .find_eol_raw
.found_eol_raw:
    mov byte [di], 0    ; Null terminate
    
    ; Load this module from raw disk
    call load_single_module
    jmp .not_module
    
.parse_iso_module:
    ; For ISO: parse "module <filename>"
    ; Rest of line is the filename
    mov [temp_name_ptr], si
    
    ; Find line end to null-terminate filename
    mov di, si
.find_eol:
    mov al, [di]
    cmp al, 0x0D        ; CR
    je .found_eol
    cmp al, 0x0A        ; LF
    je .found_eol
    cmp al, 0
    je .found_eol
    inc di
    jmp .find_eol
.found_eol:
    mov byte [di], 0    ; Null terminate
    
    ; Look up file in ISO9660 root directory
    mov si, [temp_name_ptr]
    call iso_find_file
    jc .not_module      ; File not found
    
    ; File found, EAX = start LBA, ECX = size in bytes
    mov [temp_start_sector], eax
    ; Convert size to sectors (round up)
    add ecx, 511
    shr ecx, 9          ; Divide by 512
    mov [temp_num_sectors], cx
    
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
    
    ; Now actually load the module data from disk
    ; We need to load sectors into memory above 1MB
    ; Strategy: Load to low memory buffer, then copy to high memory
    
    push di
    mov cx, [temp_num_sectors]
    cmp cx, 0
    je .no_data_to_load
    
    ; Use 0x2000 as temporary buffer (safe area)
    mov word [module_temp_buffer], 0x2000
    
.load_sector_loop:
    cmp cx, 0
    je .done_loading
    
    ; Load one sector to temp buffer
    push cx
    
    ; Setup DAP for reading
    mov si, module_load_dap
    mov eax, [temp_start_sector]
    mov [module_load_dap + 8], eax
    
    mov ah, 0x42
    mov dl, [boot_drive]
    int 0x13
    jc .load_error
    
    ; Copy from temp buffer (0x2000) to high memory
    ; Source: DS:SI = 0:0x2000
    ; Dest: [temp_load_addr]
    call copy_to_high_memory
    
    ; Increment addresses
    inc dword [temp_start_sector]
    add dword [temp_load_addr], 512
    
    pop cx
    dec cx
    jmp .load_sector_loop
    
.load_error:
    pop cx
    ; Continue anyway - module might be partially loaded
    
.done_loading:
.no_data_to_load:
    pop di
    
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

; Helper: Parse decimal number from string
; Input: SI = string pointer
; Output: EAX = parsed number, SI = pointer after number, CF = error flag
parse_decimal:
    push ebx
    push ecx
    xor eax, eax        ; Result = 0
    xor ebx, ebx
    
.parse_loop:
    mov bl, [si]
    ; Check if digit (0-9)
    cmp bl, '0'
    jb .done_ok
    cmp bl, '9'
    ja .done_ok
    
    ; Convert ASCII to digit: '0' = 0x30, so digit = char - 0x30
    sub bl, '0'
    
    ; result = result * 10 + digit
    mov ecx, 10
    mul ecx             ; EAX = EAX * 10 (result in EDX:EAX)
    add eax, ebx        ; Add digit
    inc si
    jmp .parse_loop
    
.done_ok:
    clc                 ; Clear carry = success
    pop ecx
    pop ebx
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

; ISO9660 file finder
; Input: SI = filename (null-terminated, uppercase)
; Output: EAX = start LBA, ECX = file size, CF = 0 on success, CF = 1 on failure
iso_find_file:
    pusha
    
    ; Load root directory (we'll load 4 sectors = 2KB to be safe)
    mov eax, [iso_root_lba]
    mov [iso_dir_dap + 8], eax
    mov si, iso_dir_dap
    mov ah, 0x42
    mov dl, [boot_drive]
    int 0x13
    jc .not_found
    
    ; Search directory entries at 0x3000
    mov di, 0x3000
    mov cx, [iso_root_size]
    cmp cx, 2048        ; Limit to 2KB
    jbe .size_ok
    mov cx, 2048
.size_ok:
    
.search_loop:
    ; Check if we've processed all entries
    cmp cx, 0
    jle .not_found
    
    ; Get directory record length
    mov al, [di]
    cmp al, 0           ; End of directory
    je .not_found
    
    ; Get filename length (at offset 32)
    movzx bx, byte [di + 32]
    cmp bx, 0
    je .next_entry
    
    ; Compare filename (starts at offset 33)
    push si
    push di
    push cx
    
    lea di, [di + 33]   ; Point to filename in directory
    mov cx, bx          ; Filename length
    
    ; Compare with requested filename
    push si
.cmp_loop:
    cmp cx, 0
    je .cmp_end
    mov al, [si]
    cmp al, 0           ; End of search string?
    je .cmp_end
    cmp al, [di]
    jne .cmp_nomatch
    inc si
    inc di
    dec cx
    jmp .cmp_loop
    
.cmp_end:
    ; Check if both strings ended
    cmp byte [si], 0
    jne .cmp_nomatch
    cmp cx, 0
    je .found_match
    ; Check if remaining is just version (;1)
    cmp byte [di], ';'
    je .found_match
    
.cmp_nomatch:
    pop si
    pop cx
    pop di
    pop si
    
.next_entry:
    ; Move to next entry
    movzx ax, byte [di]
    add di, ax
    sub cx, ax
    jmp .search_loop
    
.found_match:
    pop si              ; Discard saved SI
    pop cx
    pop di
    pop si
    
    ; Get file location (LBA) at offset 2 (little-endian)
    mov eax, [di + 2]
    mov [temp_file_lba], eax
    
    ; Get file size at offset 10
    mov ecx, [di + 10]
    mov [temp_file_size], ecx
    
    popa
    mov eax, [temp_file_lba]
    mov ecx, [temp_file_size]
    clc
    ret
    
.not_found:
    popa
    stc
    ret

; Enable A20 gate via keyboard controller
enable_a20:
    in al, 0x92
    or al, 2
    out 0x92, al
    ret

; Copy 512 bytes from low memory (0x2000) to high memory address in [temp_load_addr]
; This uses unreal mode (big real mode) to access high memory
copy_to_high_memory:
    pusha
    push ds
    push es
    
    ; Enable protected mode temporarily
    cli
    lgdt [gdt_descriptor]
    mov eax, cr0
    or al, 1
    mov cr0, eax
    
    ; Load DS with data segment (allows 4GB access)
    mov ax, DATA_SEG
    mov ds, ax
    
    ; Back to real mode but with 32-bit segments (unreal mode)
    and al, 0xFE
    mov cr0, eax
    
    ; Now DS can access all 4GB even in real mode
    ; Copy 512 bytes from 0x2000 to [temp_load_addr]
    mov esi, 0x2000
    mov edi, [temp_load_addr]
    mov ecx, 128        ; 512 bytes / 4 = 128 dwords
    
.copy_loop:
    mov eax, [ds:esi]
    mov [ds:edi], eax
    add esi, 4
    add edi, 4
    loop .copy_loop
    
    ; Restore segments
    xor ax, ax
    mov ds, ax
    
    sti
    pop es
    pop ds
    popa
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
msg_config: db 'Loading config...', 0x0D, 0x0A, 0
msg_error: db 'Error loading kernel!', 0x0D, 0x0A, 0
boot_drive: db 0
vbe_exact_match: db 0

; Module loading data
MODULE_INFO_ADDR equ 0x8000  ; Module info array at 0x8000
module_count: dw 0
temp_start_sector: dd 0
temp_num_sectors: dw 0
temp_load_addr: dd 0
temp_name_ptr: dd 0
str_module: db 'module'
module_temp_buffer: dw 0

; ISO9660 data
using_iso: db 0
iso_root_lba: dd 0
iso_root_size: dd 0
temp_file_lba: dd 0
temp_file_size: dd 0
iso_signature: db 'CD001'
filename_bootcfg: db 'BOOT.CFG', 0
msg_raw_skip: db 'Raw disk: module skipped', 0x0D, 0x0A, 0
msg_module_found: db 'Found module directive', 0x0D, 0x0A, 0
hardcoded_initrd_name: db 'INITRD.IMG', 0

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

; ISO9660 Primary Volume Descriptor check DAP
align 4
iso_check_dap:
    db 0x10, 0
    dw 1                ; Read 1 sector
    dw 0                ; Offset 0
    dw 0x500            ; Segment 0x500 (load at 0x5000)
    dq 16               ; Sector 16 = Primary Volume Descriptor

; ISO9660 directory loading DAP
align 4
iso_dir_dap:
    db 0x10, 0
    dw 4                ; Read 4 sectors (2KB)
    dw 0                ; Offset 0
    dw 0x300            ; Segment 0x300 (load at 0x3000)
    dq 0                ; LBA (filled at runtime)

; Generic file loading DAP
align 4
file_load_dap:
    db 0x10, 0
    dw 1                ; Read 1 sector
    dw 0                ; Offset 0
    dw 0x500            ; Segment 0x500 (load at 0x5000)
    dq 0                ; LBA (filled at runtime)

; Module loading DAP (for loading modules to temp buffer)
align 4
module_load_dap:
    db 0x10, 0          ; Size, reserved
    dw 1                ; Sectors to read (1 sector at a time)
    dw 0                ; Offset 0
    dw 0x200            ; Segment 0x200 (load at 0x2000)
    dq 0                ; LBA (filled at runtime)

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