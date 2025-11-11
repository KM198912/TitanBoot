# x86 Bootloader

A custom x86 bootloader with VBE graphics support, E820 memory detection, and Multiboot-style boot information passing.

## Features

- **Two-Stage Bootloader**: MBR boot sector (stage 1) loads stage 2 loader from disk
- **VBE Graphics Support**: Automatic VESA BIOS Extensions mode detection and configuration
- **E820 Memory Detection**: Full memory map with 64-bit addressing support
- **Configurable Video Mode**: Easy-to-configure resolution and color depth via constants
- **Boot Info Structure**: Multiboot-like information passing to kernel (flags, memory, framebuffer)
- **Serial Debug Output**: Formatted debug output via COM1/debugcon for development
- **Protected Mode**: Switches from 16-bit real mode to 32-bit protected mode
- **Graphics Primitives**: Pixel plotting and rectangle filling for 24/32-bit framebuffers

## Video Mode Configuration

Edit the constants at the top of `src/loader.asm` to configure video settings:

```asm
VIDEOMODE equ 0            ; 0 = try graphics, 1 = force text mode
VIDEOWIDTH equ 1024        ; Desired width in pixels
VIDEOHEIGHT equ 768        ; Desired height in pixels
VIDEODEPTH equ 32          ; Desired bits per pixel (24 or 32)
```

Common resolutions: `640x480`, `800x600`, `1024x768`, `1280x1024`, `1920x1080`

The bootloader performs a two-pass VBE mode search:
1. First pass: Look for exact BPP match
2. Second pass: Accept BPP-8 fallback if no exact match found

## Memory Layout

| Address Range | Usage |
|--------------|-------|
| `0x0000-0x03FF` | Real Mode IVT |
| `0x0400-0x04FF` | BIOS Data Area |
| `0x0500-0x7BFF` | Free (conventional) |
| `0x7C00-0x7DFF` | Stage 1 Bootloader (512 bytes) |
| `0x7E00-0x9DFF` | Stage 2 Loader (~8KB) |
| `0x6000-0x61FF` | Boot Info Structure |
| `0x6200-0x65FF` | E820 Memory Map (up to 32 entries) |
| `0x1000+` | Kernel |

## Boot Info Structure

The bootloader passes a pointer to the boot info structure in the `EAX` register when jumping to the kernel. The structure contains:

- **flags**: Indicates which fields are valid (`0x1041` = MEM + MMAP + FB)
- **mem_lower/mem_upper**: Legacy memory info (< 4GB)
- **boot_device**: Boot drive number
- **mmap_addr/mmap_length**: E820 memory map pointer and entry count
- **framebuffer_info**: VBE framebuffer details (address, resolution, pitch, BPP, type)

See `inc/boot_info.h` for the complete structure definition.

## Building

### Prerequisites

- **NASM**: Netwide Assembler for x86 assembly
- **GCC**: GNU Compiler Collection (32-bit support)
- **LD**: GNU Linker
- **QEMU** (optional): For testing with `qemu-system-i386`

### Build Commands

```bash
# Build everything
make all

# Clean build artifacts
make clean

# Build and run in QEMU
make run
```

The makefile automatically discovers all C files in `src/` recursively.

## Running

### QEMU

```bash
make run
```

Or manually:

```bash
qemu-system-i386 -drive format=raw,file=build/os-image.bin -debugcon stdio
```

### Real Hardware

Write the OS image to a USB drive or floppy disk:

```bash
sudo dd if=build/os-image.bin of=/dev/sdX bs=512
```

**⚠️ WARNING**: Replace `/dev/sdX` with your actual device. This will erase all data on the target device!

## Project Structure

```
.
├── inc/
│   ├── boot_info.h      # Boot information structure definitions
│   └── screen.h         # VGA text mode and graphics functions
├── src/
│   ├── boot.asm         # Stage 1 bootloader (MBR)
│   ├── loader.asm       # Stage 2 loader (VBE, E820, protected mode)
│   ├── kernel_entry.asm # Kernel entry point (receives boot_info)
│   └── kernel.c         # Main kernel code
├── build/               # Build output directory
├── linker.ld            # Linker script for kernel
├── makefile             # Build automation
├── LICENSE.md           # MIT License
└── README.md            # This file
```

## Development

### Serial Debug Output

The kernel includes `serial_printf()` for formatted debug output. In QEMU, use `-debugcon stdio` to see output on stdout:

```c
serial_printf("Framebuffer at 0x%x\n", fb_addr);
serial_printf("Resolution: %dx%d @ %d bpp\n", width, height, bpp);
```

Supported format specifiers: `%s` (string), `%d` (signed int), `%x` (hex), `%c` (char)

### Adding C Files

The makefile automatically discovers and compiles all `.c` files in `src/` and subdirectories. Just add your `.c` files anywhere under `src/` and run `make`.

### Extending the Bootloader

1. **Memory Management**: Use the E820 map to implement a physical memory allocator
2. **Font Rendering**: Add bitmap fonts for text rendering in graphics mode
3. **Interrupts**: Set up GDT/IDT for interrupt handling
4. **Drivers**: Implement keyboard, mouse, and disk drivers
5. **File System**: Add support for reading files from disk (FAT, ext2, custom FS)

## Technical Details

### Boot Process

1. BIOS loads stage 1 bootloader (MBR) at `0x7C00`
2. Stage 1 loads stage 2 loader from disk at `0x7E00`
3. Stage 2 performs:
   - E820 memory detection
   - VBE graphics mode setup (if enabled)
   - Loads kernel from disk at `0x1000`
   - Switches to 32-bit protected mode
   - Jumps to kernel with boot info pointer in `EAX`
4. Kernel entry point (`kernel_entry.asm`) receives boot info and calls `kernel_main()`

### VBE Graphics

The bootloader uses VBE (VESA BIOS Extensions) to set up high-resolution graphics modes:

- **INT 0x15, AX=0x4F00**: Get VBE controller info
- **INT 0x15, AX=0x4F01**: Get mode info for specific mode number
- **INT 0x15, AX=0x4F02**: Set VBE mode

The framebuffer is accessed as a linear memory-mapped buffer (LFB).

### E820 Memory Map

The E820 memory map provides detailed information about system memory regions:

- **Type 1**: Usable RAM
- **Type 2**: Reserved (do not use)
- **Type 3**: ACPI reclaimable
- **Type 4**: ACPI NVS
- **Type 5**: Bad memory

Each entry contains a 64-bit base address and length, supporting systems with >4GB RAM.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Author

Kevin Meerts (2025)

## Acknowledgments

- VBE graphics implementation inspired by VESA specifications
- E820 memory detection based on OSDev wiki documentation
- Boot info structure modeled after Multiboot specification
