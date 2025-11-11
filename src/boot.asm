; boot.asm - Stage 1 Bootloader (MBR)
; This is loaded by BIOS at 0x7C00
; Must be exactly 512 bytes with boot signature

[BITS 16]
[ORG 0x7C00]

; Entry point
start:
    ; Save boot drive number (BIOS passes this in DL)
    mov [boot_drive], dl
    
    ; Clear interrupts and set up segments
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00      ; Stack grows downward from bootloader
    sti
    
    ; Clear screen first
    mov ah, 0x00
    mov al, 0x03        ; 80x25 text mode
    int 0x10

    ; Print loading message
    mov si, msg_loading
    call print_string

    ; Try LBA first, fall back to CHS if needed
    ; Check if LBA is available
    mov ah, 0x41        ; Check extensions present
    mov bx, 0x55AA
    mov dl, [boot_drive]
    int 0x13
    jc .use_chs         ; No LBA support, use CHS
    cmp bx, 0xAA55      ; Check if extensions installed
    jne .use_chs
    
    ; Load using LBA (INT 13h Extensions)
    mov si, dap         ; DS:SI = Disk Address Packet
    mov ah, 0x42        ; Extended read
    mov dl, [boot_drive]
    int 0x13
    jnc .read_success   ; Success, continue
    
.use_chs:
    ; Fall back to CHS read (legacy BIOS)
    mov ah, 0x02        ; BIOS read sector function
    mov al, 16          ; Number of sectors to read (8KB loader)
    mov ch, 0           ; Cylinder 0
    mov cl, 2           ; Start at sector 2 (sector 1 is boot sector)
    mov dh, 0           ; Head 0
    mov dl, [boot_drive]; Drive number
    mov bx, 0x7E00      ; Load to address 0x7E00
    int 0x13
    jc disk_error       ; Jump if carry flag set (error)

.read_success:
    ; Print success message
    mov si, msg_success
    call print_string

    ; Pass boot drive to Stage 2 in DL register
    mov dl, [boot_drive]
    
    ; Jump to Stage 2 loader
    jmp 0x7E00

disk_error:
    mov si, msg_error
    call print_string
    jmp $               ; Hang

; Print null-terminated string
; SI = pointer to string
print_string:
    pusha
.loop:
    lodsb               ; Load byte from SI into AL
    test al, al         ; Check if null terminator
    jz .done
    mov ah, 0x0E        ; BIOS teletype output
    int 0x10            ; Print character
    jmp .loop
.done:
    popa
    ret

; Data
boot_drive: db 0

; Disk Address Packet (DAP) for LBA reads
align 4
dap:
    db 0x10             ; Size of DAP (16 bytes)
    db 0                ; Reserved (0)
    dw 16               ; Number of sectors to read (8KB loader)
    dw 0x7E00           ; Offset to load to
    dw 0                ; Segment to load to
    dq 1                ; LBA start sector (sector 1 = second sector, after boot sector)

msg_loading: db 'Loading...', 0x0D, 0x0A, 0
msg_success: db 'OK', 0x0D, 0x0A, 0
msg_error: db 'Disk error!', 0x0D, 0x0A, 0

; Padding and boot signature
times 510-($-$$) db 0
dw 0xAA55               ; Boot signature