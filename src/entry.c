// entry.c - Protected mode entry/bootstrap
// This runs in 32-bit protected mode and sets up the system before jumping to kernel

#include <stdint.h>
#include <stddef.h>
#include "boot_info.h"

// VGA text mode buffer
#define VGA_MEMORY 0xB8000
#define VGA_WIDTH 80

// Serial port (COM1)
#define COM1 0xE9

// Global framebuffer info (set from boot_info)
static framebuffer_info_t* fb_info = 0;

// Port I/O functions
static inline void outb(uint16_t port, uint8_t value) {
    __asm__ __volatile__ ("outb %0, %1" : : "a"(value), "Nd"(port));
}

static inline uint8_t inb(uint16_t port) {
    uint8_t value;
    __asm__ __volatile__ ("inb %1, %0" : "=a"(value) : "Nd"(port));
    return value;
}

// Initialize serial port
static void serial_init(void) {
    outb(COM1 + 1, 0x00);    // Disable interrupts
    outb(COM1 + 3, 0x80);    // Enable DLAB (set baud rate divisor)
    outb(COM1 + 0, 0x03);    // Set divisor to 3 (lo byte) 38400 baud
    outb(COM1 + 1, 0x00);    //                  (hi byte)
    outb(COM1 + 3, 0x03);    // 8 bits, no parity, one stop bit
    outb(COM1 + 2, 0xC7);    // Enable FIFO, clear them, with 14-byte threshold
    outb(COM1 + 4, 0x0B);    // IRQs enabled, RTS/DSR set
}

// Write a character to serial port
static void serial_putchar(char c) {
    // Wait for transmit buffer to be empty
    while ((inb(COM1 + 5) & 0x20) == 0);
    outb(COM1, c);
}

// Write a string to serial port
static void serial_puts(const char* str) {
    while (*str) {
        if (*str == '\n') {
            serial_putchar('\r');  // Add carriage return for newlines
        }
        serial_putchar(*str++);
    }
}

// Plot a pixel in RGB framebuffer mode
static void plot_pixel(int x, int y, uint32_t color) {
    if (!fb_info || fb_info->type != 1) return;  // Only for RGB mode
    if (x < 0 || y < 0 || x >= (int)fb_info->width || y >= (int)fb_info->height) return;
    
    uint8_t* framebuffer = (uint8_t*)fb_info->address;
    uint32_t bytes_per_pixel = (fb_info->bpp + 7) / 8;  // Round up to bytes
    uint32_t offset = y * fb_info->pitch + x * bytes_per_pixel;
    
    // Write color bytes (BGR order for VBE)
    framebuffer[offset + 0] = (color >> 0) & 0xFF;   // Blue
    framebuffer[offset + 1] = (color >> 8) & 0xFF;   // Green
    framebuffer[offset + 2] = (color >> 16) & 0xFF;  // Red
    if (bytes_per_pixel == 4) {
        framebuffer[offset + 3] = 0;  // Alpha/padding
    }
}

// Draw a filled rectangle
static void fill_rect(int x, int y, int w, int h, uint32_t color) {
    for (int py = y; py < y + h; py++) {
        for (int px = x; px < x + w; px++) {
            plot_pixel(px, py, color);
        }
    }
}

// Helper functions (must be before serial_printf)
static void int_to_string(uint32_t value, char* buffer) {
    if (value == 0) {
        buffer[0] = '0';
        buffer[1] = '\0';
        return;
    }
    
    char temp[32];
    int i = 0;
    while (value > 0) {
        temp[i++] = '0' + (value % 10);
        value /= 10;
    }
    
    int j = 0;
    while (i > 0) {
        buffer[j++] = temp[--i];
    }
    buffer[j] = '\0';
}

static void hex_to_string(uint32_t value, char* buffer) {
    const char* hex = "0123456789ABCDEF";
    buffer[0] = '0';
    buffer[1] = 'x';
    for (int i = 7; i >= 0; i--) {
        buffer[2 + (7 - i)] = hex[(value >> (i * 4)) & 0xF];
    }
    buffer[10] = '\0';
}

static int strlen(const char* str) {
    int len = 0;
    while (str[len]) len++;
    return len;
}

// Simple serial printf (supports %s, %d, %x, %c)
static void serial_printf(const char* format, ...) {
    __builtin_va_list args;
    __builtin_va_start(args, format);
    
    char buffer[32];
    const char* p = format;
    
    while (*p) {
        if (*p == '%' && *(p + 1)) {
            p++;
            switch (*p) {
                case 's': {
                    const char* s = __builtin_va_arg(args, const char*);
                    serial_puts(s);
                    break;
                }
                case 'd': {
                    int val = __builtin_va_arg(args, int);
                    if (val < 0) {
                        serial_putchar('-');
                        val = -val;
                    }
                    int_to_string(val, buffer);
                    serial_puts(buffer);
                    break;
                }
                case 'x': {
                    uint32_t val = __builtin_va_arg(args, uint32_t);
                    hex_to_string(val, buffer);
                    serial_puts(buffer);
                    break;
                }
                case 'c': {
                    char c = (char)__builtin_va_arg(args, int);
                    serial_putchar(c);
                    break;
                }
                case '%': {
                    serial_putchar('%');
                    break;
                }
                default:
                    serial_putchar('%');
                    serial_putchar(*p);
                    break;
            }
        } else {
            if (*p == '\n') {
                serial_putchar('\r');
            }
            serial_putchar(*p);
        }
        p++;
    }
    
    __builtin_va_end(args);
}

#define DEBUG_PORT 0xE9
static void write_string(const char* str, int row, int col, uint8_t color) {
    volatile unsigned short* vga = (volatile unsigned short*)VGA_MEMORY;
    int offset = row * VGA_WIDTH + col;
    int i = 0;
    
    while (str[i] != '\0') {
        vga[offset + i] = (unsigned short)((color << 8) | str[i]);
        i++;
    }

}

static void clear_screen() {
    volatile unsigned short* vga = (volatile unsigned short*)VGA_MEMORY;
    for (int i = 0; i < VGA_WIDTH * 25; i++) {
        vga[i] = (0x00 << 8) | ' ';
    }
}

void* memcmp(const void* s1, const void* s2, size_t n) {
    const unsigned char* p1 = (const unsigned char*)s1;
    const unsigned char* p2 = (const unsigned char*)s2;
    for (size_t i = 0; i < n; i++) {
        if (p1[i] != p2[i]) {
            return (void*)(p1 + i);
        }
    }
    return NULL;
}

// ISO9660 structures and functions
#pragma pack(push, 1)
typedef struct {
    uint8_t  length;           // Length of directory record
    uint8_t  ext_length;       // Extended attribute record length
    uint32_t extent_lba;       // Location of extent (LBA) - little endian
    uint32_t extent_lba_be;    // Location of extent (LBA) - big endian
    uint32_t data_length;      // Data length - little endian
    uint32_t data_length_be;   // Data length - big endian
    uint8_t  recording_date[7]; // Recording date and time
    uint8_t  file_flags;       // File flags
    uint8_t  file_unit_size;   // File unit size
    uint8_t  interleave_gap;   // Interleave gap size
    uint16_t volume_seq;       // Volume sequence number - little endian
    uint16_t volume_seq_be;    // Volume sequence number - big endian
    uint8_t  name_length;      // Length of file identifier
    // Followed by: file identifier (name_length bytes)
    // Followed by: padding field (if name_length is even)
    // Followed by: system use (variable length)
} iso_directory_entry_t;
#pragma pack(pop)

// ISO9660 file finder
// Note: We can't use BIOS interrupts in protected mode, so this is currently
// a placeholder for when we implement a proper block device driver
// For now, the bootloader should have already loaded any necessary data
static int iso_find_file(const char* filename, uint32_t* out_lba, uint32_t* out_size) {
    // TODO: Implement ISO9660 file lookup
    // This would require:
    // 1. A block device driver to read sectors in protected mode
    // 2. Or have the bootloader pre-load the root directory
    // 3. Parse directory entries to find the file
    
    serial_printf("ISO9660: Looking for file '%s'\n", filename);
    serial_printf("ISO9660: File lookup not yet implemented (requires block driver)\n");
    return -1;  // Not found
}

// Parse config file and load modules
static void load_modules_from_config(boot_info_t* boot_info) {
    if (!(boot_info->flags & 0x2000)) {
        serial_printf("No boot media info available\n");
        return;
    }
    
    if (boot_info->boot_media.boot_type != 1) {
        serial_printf("Not booting from ISO, skipping ISO module loading\n");
        return;
    }
    
    serial_printf("\nISO9660 Module Loading:\n");
    serial_printf("  Root Directory LBA: %d\n", boot_info->boot_media.iso_root_lba);
    serial_printf("  Root Directory Size: %d bytes\n", boot_info->boot_media.iso_root_size);
    
    // Try to find and load modules from config
    uint32_t config_lba, config_size;
    if (iso_find_file("BOOT.CFG", &config_lba, &config_size) == 0) {
        serial_printf("  Found BOOT.CFG at LBA %d, size %d\n", config_lba, config_size);
        // TODO: Read and parse config file
        // TODO: Load modules specified in config
    } else {
        serial_printf("  BOOT.CFG not found, no modules to load\n");
    }
}

// Entry point (called from kernel_entry.asm)
// This is the bootstrap that sets up the system and loads the kernel
void entry_main(boot_info_t* boot_info) {
    // Initialize serial port for debug output
    serial_init();
    
    clear_screen();
    
    // Check if boot_info is valid
    if (boot_info == 0 || (uint32_t)boot_info < 0x1000) {
        serial_printf("ERROR: Invalid boot info pointer!\n");
        write_string("ERROR: Invalid boot info pointer!", 0, 0, 0x0C);
        while(1) __asm__ __volatile__("hlt");
    }
    
    // Serial debug output
    serial_printf("\n========================================\n");
    serial_printf("  TITANBOOT - SYSTEM BOOTSTRAP\n");
    serial_printf("========================================\n\n");
    
    serial_printf("Boot Info @ %x\n", (uint32_t)boot_info);
    serial_printf("Flags: %x\n\n", boot_info->flags);
    
    char buffer[32];
    int row = 0;
    
    // Header
    write_string("======================================", row++, 0, 0x0A);
    write_string("  CUSTOM BOOTLOADER - KERNEL LOADED", row++, 0, 0x0A);
    write_string("======================================", row++, 0, 0x0A);
    row++;
    
    // Check flags
    write_string("Boot Info Flags: ", row, 0, 0x0F);
    hex_to_string(boot_info->flags, buffer);
    write_string(buffer, row++, 17, 0x0F);
    row++;
    
    // Memory info (if available)
    if (boot_info->flags & 0x01) {
        serial_printf("Memory Information (legacy):\n");
        serial_printf("  Lower: %d KB\n", boot_info->mem_lower);
        serial_printf("  Upper: %d KB\n\n", boot_info->mem_upper);
        
        write_string("Memory Information (legacy):", row++, 0, 0x0F);
        
        write_string("  Lower memory: ", row, 0, 0x07);
        int_to_string(boot_info->mem_lower, buffer);
        write_string(buffer, row, 16, 0x07);
        write_string(" KB", row++, 16 + strlen(buffer), 0x07);
        
        write_string("  Upper memory: ", row, 0, 0x07);
        int_to_string(boot_info->mem_upper, buffer);
        write_string(buffer, row, 16, 0x07);
        write_string(" KB", row++, 16 + strlen(buffer), 0x07);
        
        row++;
    }
    
    // E820 Memory map (if available)
    if (boot_info->flags & 0x40) {
        e820_entry_t* entries = (e820_entry_t*)boot_info->mmap_addr;
        uint32_t count = boot_info->mmap_length;
        
        serial_printf("E820 Memory Map (%d entries):\n", count);
        for (uint32_t i = 0; i < count; i++) {
            serial_printf("  [%d] Base: %x:%x  Len: %x:%x  Type: %d\n",
                i,
                (uint32_t)(entries[i].base >> 32),
                (uint32_t)(entries[i].base & 0xFFFFFFFF),
                (uint32_t)(entries[i].length >> 32),
                (uint32_t)(entries[i].length & 0xFFFFFFFF),
                entries[i].type);
        }
        serial_printf("\n");
        
        write_string("E820 Memory Map:", row++, 0, 0x0F);
        
        // Show first few entries on screen (limit to prevent overflow)
        if (count > 5) count = 5;
        if (count > 5) count = 5;
        
        for (uint32_t i = 0; i < count; i++) {
            write_string("  ", row, 0, 0x07);
            
            // Base address
            hex_to_string((uint32_t)(entries[i].base >> 32), buffer);
            write_string(buffer, row, 2, 0x07);
            hex_to_string((uint32_t)(entries[i].base & 0xFFFFFFFF), buffer);
            write_string(buffer, row, 12, 0x07);
            
            write_string(" Len:", row, 22, 0x07);
            hex_to_string((uint32_t)(entries[i].length >> 32), buffer);
            write_string(buffer, row, 27, 0x07);
            hex_to_string((uint32_t)(entries[i].length & 0xFFFFFFFF), buffer);
            write_string(buffer, row, 37, 0x07);
            
            // Type
            write_string(" T:", row, 47, 0x07);
            int_to_string(entries[i].type, buffer);
            write_string(buffer, row, 50, 0x07);
            
            row++;
        }
        
        row++;
    }
    
    // Framebuffer info (if available)
    if (boot_info->flags & 0x1000) {
        serial_printf("Framebuffer Information:\n");
        serial_printf("  Address: %x\n", boot_info->framebuffer.address);
        serial_printf("  Resolution: %dx%d\n", boot_info->framebuffer.width, boot_info->framebuffer.height);
        serial_printf("  Pitch: %d\n", boot_info->framebuffer.pitch);
        serial_printf("  BPP: %d\n", boot_info->framebuffer.bpp);
        serial_printf("  Type: %d (%s)\n", boot_info->framebuffer.type,
            boot_info->framebuffer.type == 0 ? "Indexed" :
            boot_info->framebuffer.type == 1 ? "RGB" :
            boot_info->framebuffer.type == 2 ? "Text" : "Unknown");
        
        // If RGB mode, show color mask info
        if (boot_info->framebuffer.type == 1) {
            serial_printf("  Red:   mask_size=%d, position=%d\n", 
                boot_info->framebuffer.red_mask_size, boot_info->framebuffer.red_position);
            serial_printf("  Green: mask_size=%d, position=%d\n", 
                boot_info->framebuffer.green_mask_size, boot_info->framebuffer.green_position);
            serial_printf("  Blue:  mask_size=%d, position=%d\n", 
                boot_info->framebuffer.blue_mask_size, boot_info->framebuffer.blue_position);
        }
        serial_printf("\n");
        
        write_string("Framebuffer Information:", row++, 0, 0x0F);
        
        write_string("  Address: ", row, 0, 0x07);
        hex_to_string(boot_info->framebuffer.address, buffer);
        write_string(buffer, row++, 11, 0x07);
        
        write_string("  Resolution: ", row, 0, 0x07);
        int_to_string(boot_info->framebuffer.width, buffer);
        write_string(buffer, row, 14, 0x07);
        write_string("x", row, 14 + strlen(buffer), 0x07);
        int_to_string(boot_info->framebuffer.height, buffer);
        write_string(buffer, row, 15 + strlen(buffer), 0x07);
        row++;
        
        write_string("  BPP: ", row, 0, 0x07);
        int_to_string(boot_info->framebuffer.bpp, buffer);
        write_string(buffer, row, 7, 0x07);
        
        write_string("  Type: ", row, 15, 0x07);
        if (boot_info->framebuffer.type == 0) write_string("Indexed", row, 22, 0x07);
        else if (boot_info->framebuffer.type == 1) write_string("RGB", row, 22, 0x07);
        else if (boot_info->framebuffer.type == 2) write_string("Text", row, 22, 0x07);
        row++;
        
        row++;
    }
    
    // Module info (if available)
    if (boot_info->flags & 0x08) {
        module_info_t* modules = (module_info_t*)boot_info->mods_addr;
        uint32_t count = boot_info->mods_count;
        
        serial_printf("Modules Loaded (%d):\n", count);
        for (uint32_t i = 0; i < count; i++) {
            serial_printf("  [%d] Start: %x  End: %x  Name: %x\n",
                i,
                modules[i].mod_start,
                modules[i].mod_end,
                modules[i].string);
            
            // Try to print module name if valid pointer
            if (modules[i].string >= 0x5000 && modules[i].string < 0x10000) {
                const char* name = (const char*)modules[i].string;
                serial_printf("       Name: %s\n", name);
            }
        }
        serial_printf("\n");
        
        write_string("Modules Loaded: ", row, 0, 0x0F);
        int_to_string(count, buffer);
        write_string(buffer, row++, 16, 0x0E);
        
        // Show modules on screen (limit to prevent overflow)
        uint32_t display_count = count;
        if (row + display_count > 24) display_count = 24 - row;
        
        for (uint32_t i = 0; i < display_count; i++) {
            write_string("  [", row, 0, 0x07);
            int_to_string(i, buffer);
            write_string(buffer, row, 3, 0x07);
            write_string("] ", row, 3 + strlen(buffer), 0x07);
            
            hex_to_string(modules[i].mod_start, buffer);
            write_string(buffer, row, 6, 0x07);
            
            write_string("-", row, 16, 0x07);
            
            hex_to_string(modules[i].mod_end, buffer);
            write_string(buffer, row, 17, 0x07);
            
            row++;
        }
        
        row++;
    }
    
    write_string("System ready. Kernel running in protected mode.", row++, 0, 0x0E);
    
    serial_printf("System ready. Kernel running in protected mode.\n");
    
    // Display boot media info
    if (boot_info->flags & 0x2000) {
        serial_printf("\nBoot Media Information:\n");
        serial_printf("  Drive: %x\n", (uint32_t)boot_info->boot_media.drive_number);
        serial_printf("  Type: %s (%d)\n", 
            boot_info->boot_media.boot_type == 1 ? "ISO9660" : "Raw Disk",
            boot_info->boot_media.boot_type);
        if (boot_info->boot_media.boot_type == 1) {
            serial_printf("  ISO Root LBA: %d\n", boot_info->boot_media.iso_root_lba);
            serial_printf("  ISO Root Size: %d bytes\n", boot_info->boot_media.iso_root_size);
        }
        serial_printf("\n");
    }
    
    // Debug: Check if memory from real mode survives
    serial_printf("Memory test from real mode:\n");
    serial_printf("  Module load at START of load_config:\n");
    serial_printf("    0x7000: %x (should be BEEF)\n", *(volatile uint16_t*)0x7000);
    serial_printf("    0x7002: %x (CAFE=success, DEAD=failed)\n", *(volatile uint16_t*)0x7002);
    serial_printf("  0x3000 first 64 bytes (loaded BEFORE any other BIOS calls):\n    ");
    for (int i = 0; i < 64; i++) {
        uint32_t byte = *(volatile uint8_t*)(0x3000 + i);
        if (byte < 0x10) serial_putchar('0');
        hex_to_string(byte, buffer);
        if (byte < 0x10) {
            serial_putchar(buffer[7]);
        } else {
            serial_putchar(buffer[6]);
            serial_putchar(buffer[7]);
        }
        serial_putchar(' ');
        if ((i + 1) % 16 == 0) serial_printf("\n    ");
    }
    serial_printf("\n");
    
    // Display loaded modules
    if (boot_info->flags & 0x08 && boot_info->mods_count > 0) {
        serial_printf("Loaded Modules (%d):\n", boot_info->mods_count);
        module_info_t* mods = (module_info_t*)boot_info->mods_addr;
        for (uint32_t i = 0; i < boot_info->mods_count; i++) {
            serial_printf("  [%d] Start: %x  End: %x", i, mods[i].mod_start, mods[i].mod_end);
            if (mods[i].string) {
                serial_printf("  Name: %s", (char*)mods[i].string);
            }
            serial_printf("\n");
            
            // Dump first 16 bytes of module data to verify it loaded
            uint8_t* data = (uint8_t*)mods[i].mod_start;
            serial_printf("       Data: ");
            for (int j = 0; j < 16; j++) {
                uint32_t byte = data[j];
                if (byte < 0x10) serial_putchar('0');
                hex_to_string(byte, buffer);
                // Only print last 2 chars if value is small
                if (byte < 0x10) {
                    serial_putchar(buffer[7]);  // Last char
                } else {
                    serial_putchar(buffer[6]);
                    serial_putchar(buffer[7]);
                }
                serial_putchar(' ');
            }
            serial_printf("\n");
        }
        serial_printf("\n");
    }
    
    // Try to load modules from ISO if applicable
    load_modules_from_config(boot_info);
    
    // If we have RGB framebuffer, draw some test graphics
    if ((boot_info->flags & 0x1000) && boot_info->framebuffer.type == 1) {
        serial_printf("Drawing graphics test pattern...\n");
        fb_info = &boot_info->framebuffer;
        
        // Draw a gradient background
        for (int y = 0; y < (int)fb_info->height; y++) {
            for (int x = 0; x < (int)fb_info->width; x++) {
                uint8_t r = (x * 255) / fb_info->width;
                uint8_t g = (y * 255) / fb_info->height;
                uint8_t b = 64;
                uint32_t color = (r << 16) | (g << 8) | b;
                plot_pixel(x, y, color);
            }
        }
        
        // Draw some colored rectangles
        fill_rect(50, 50, 200, 150, 0xFF0000);    // Red
        fill_rect(300, 50, 200, 150, 0x00FF00);   // Green
        fill_rect(550, 50, 200, 150, 0x0000FF);   // Blue
        fill_rect(175, 250, 200, 150, 0xFFFF00);  // Yellow
        fill_rect(425, 250, 200, 150, 0xFF00FF);  // Magenta
    }
    
    //find acpi rsdp
    const char rsdp_signature[8] = {'R','S','D',' ','P','T','R',' '};
    uint32_t rsdp_address = 0;
    // Search EBDA
    uint16_t* ebda_ptr = (uint16_t*)0x40E;
    uint32_t ebda_address = ((*ebda_ptr) << 4);
    for (uint32_t addr = ebda_address; addr < ebda_address + 1024; addr += 16) {
        if (memcmp((void*)addr, rsdp_signature, 8) == 0) {
            rsdp_address = addr;
            break;
        }
    }
    // If not found, search 0xE0000 - 0xFFFFF
    if (rsdp_address == 0) {
        for (uint32_t addr = 0xE0000; addr < 0x100000; addr += 16) {
            if (memcmp((void*)addr, rsdp_signature, 8) == 0) {
                rsdp_address = addr;
                break;
            }
        }
    }
    if (rsdp_address != 0) {
        serial_printf("ACPI RSDP found at address: %x\n", rsdp_address);
        write_string("ACPI RSDP Address: ", row, 0, 0x0F);
        hex_to_string(rsdp_address, buffer);
        write_string(buffer, row++, 19, 0x07);
        row++;
    } else {
        serial_printf("ACPI RSDP not found.\n");
        write_string("ACPI RSDP not found.", row++, 0, 0x0C);
        row++;
    }

    // Bootstrap complete - jump to the real kernel
    serial_printf("\n========================================\n");
    serial_printf("  Jumping to kernel_main...\n");
    serial_printf("========================================\n\n");
    
    // Call the actual kernel (defined in kernel.c)
    extern void kernel_main(boot_info_t* boot_info);
    kernel_main(boot_info);
    
    // If kernel returns, hang
    serial_printf("ERROR: Kernel returned!\n");
    while(1) {
        __asm__ __volatile__("hlt");
    }
}