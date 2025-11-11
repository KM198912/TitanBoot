// boot_info.h - Boot information structure
// Similar to Multiboot info structure

#ifndef BOOT_INFO_H
#define BOOT_INFO_H

#include <stdint.h>

// E820 Memory map entry
typedef struct {
    uint64_t base;        // Base address
    uint64_t length;      // Length of region
    uint32_t type;        // 1 = available, 2 = reserved, 3 = ACPI reclaimable, 4 = ACPI NVS, 5 = bad memory
    uint32_t acpi_extended; // ACPI 3.0 extended attributes
} __attribute__((packed)) e820_entry_t;

// Framebuffer info
typedef struct {
    uint32_t address;
    uint32_t pitch;       // Bytes per line
    uint32_t width;
    uint32_t height;
    uint8_t  bpp;         // Bits per pixel
    uint8_t  type;        // 0 = indexed, 1 = RGB, 2 = text
    uint8_t  red_position;
    uint8_t  red_mask_size;
    uint8_t  green_position;
    uint8_t  green_mask_size;
    uint8_t  blue_position;
    uint8_t  blue_mask_size;
} __attribute__((packed)) framebuffer_info_t;

// Main boot information structure
typedef struct {
    uint32_t flags;       // Bit flags indicating which fields are valid
    
    // Memory info (flags & 0x01)
    uint32_t mem_lower;   // KB of lower memory (0-640KB) - deprecated, use mmap
    uint32_t mem_upper;   // KB of upper memory (1MB+) - deprecated, use mmap
    
    // Boot device (flags & 0x02)
    uint32_t boot_device;
    
    // E820 Memory map (flags & 0x40)
    uint32_t mmap_length; // Number of entries
    uint32_t mmap_addr;   // Address of e820_entry_t array
    
    // Framebuffer (flags & 0x1000)
    framebuffer_info_t framebuffer;
} __attribute__((packed)) boot_info_t;

// Flag definitions
#define BOOT_INFO_FLAG_MEM      0x00000001  // mem_lower/mem_upper valid (legacy)
#define BOOT_INFO_FLAG_BOOTDEV  0x00000002  // boot_device valid
#define BOOT_INFO_FLAG_MMAP     0x00000040  // E820 memory map valid
#define BOOT_INFO_FLAG_FB       0x00001000  // framebuffer info valid

// E820 Memory types
#define E820_RAM        1  // Usable RAM
#define E820_RESERVED   2  // Reserved (unusable)
#define E820_ACPI       3  // ACPI reclaimable
#define E820_NVS        4  // ACPI NVS (non-volatile storage)
#define E820_BADRAM     5  // Bad memory

#endif // BOOT_INFO_H