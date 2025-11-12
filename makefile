# Bootloader Project Makefile

# Tools
ASM = nasm
CC = gcc
LD = ld

# Directories
SRC_DIR = src
INC_DIR = inc
BUILD_DIR = build

# Flags
ASM_FLAGS = -f bin
CC_FLAGS = -m32 -ffreestanding -nostdlib -I$(INC_DIR) -O2 -Wall -Wextra
LD_FLAGS = -m elf_i386 -T linker.ld

# Output files
BOOT_BIN = $(BUILD_DIR)/boot.bin
LOADER_BIN = $(BUILD_DIR)/loader.bin
KERNEL_BIN = $(BUILD_DIR)/kernel.bin
OS_IMG = $(BUILD_DIR)/os.img
OS_ISO = $(BUILD_DIR)/os.iso

# Source files
BOOT_SRC = $(SRC_DIR)/boot.asm
LOADER_SRC = $(SRC_DIR)/loader.asm
KERNEL_ENTRY_SRC = $(SRC_DIR)/kernel_entry.asm

# Find all C source files recursively in src/
C_SOURCES = $(shell find $(SRC_DIR) -name '*.c')
# Generate corresponding object file paths in build/ (preserve directory structure)
C_OBJECTS = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(C_SOURCES))

# Default target
all: dirs $(OS_IMG)

# Create build directory and subdirectories as needed
dirs:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(sort $(dir $(C_OBJECTS)))

# Build boot sector (Stage 1)
$(BOOT_BIN): $(BOOT_SRC)
	$(ASM) $(ASM_FLAGS) $< -o $@

# Build loader (Stage 2)
$(LOADER_BIN): $(LOADER_SRC)
	$(ASM) $(ASM_FLAGS) $< -o $@

# Build kernel entry
$(BUILD_DIR)/kernel_entry.o: $(KERNEL_ENTRY_SRC)
	$(ASM) -f elf32 $< -o $@

# Pattern rule to compile C files (preserves directory structure)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CC_FLAGS) -c $< -o $@

# Link kernel
$(BUILD_DIR)/kernel.elf: $(BUILD_DIR)/kernel_entry.o $(C_OBJECTS)
	$(LD) $(LD_FLAGS) $(BUILD_DIR)/kernel_entry.o $(C_OBJECTS) -o $@

$(KERNEL_BIN): $(BUILD_DIR)/kernel.elf
	objcopy -O binary $< $@

# Create disk image (boot + loader + kernel + config + initrd)
$(OS_IMG): $(BOOT_BIN) $(LOADER_BIN) $(KERNEL_BIN) boot.cfg INITRD.IMG
	# Create empty image
	dd if=/dev/zero of=$@ bs=512 count=2880
	# Write boot sector (sector 1)
	dd if=$(BOOT_BIN) of=$@ bs=512 count=1 conv=notrunc
	# Write loader (sectors 2-17, 16 sectors = 8KB)
	dd if=$(LOADER_BIN) of=$@ bs=512 seek=1 count=16 conv=notrunc
	# Write kernel (starting at sector 18, takes ~32 sectors)
	dd if=$(KERNEL_BIN) of=$@ bs=512 seek=17 conv=notrunc
	# Write config file (sector 50)
	dd if=boot.cfg of=$@ bs=512 seek=49 count=1 conv=notrunc
	# Write INITRD module (sector 100)
	dd if=INITRD.IMG of=$@ bs=512 seek=99 conv=notrunc

# Create bootable ISO
$(OS_ISO): $(OS_IMG) boot.cfg INITRD.IMG
	@mkdir -p $(BUILD_DIR)/iso
	@cp $(OS_IMG) $(BUILD_DIR)/iso/
	@cp boot.cfg $(BUILD_DIR)/iso/BOOT.CFG
	@cp INITRD.IMG $(BUILD_DIR)/iso/INITRD.IMG
	@if command -v genisoimage > /dev/null; then \
		genisoimage -quiet -V 'CUSTOMOS' -input-charset iso8859-1 -o $@ -b os.img -hide os.img $(BUILD_DIR)/iso; \
	elif command -v mkisofs > /dev/null; then \
		mkisofs -quiet -V 'CUSTOMOS' -input-charset iso8859-1 -o $@ -b os.img -hide os.img $(BUILD_DIR)/iso; \
	else \
		echo "Error: Neither genisoimage nor mkisofs found. Install with:"; \
		echo "  Ubuntu/Debian: sudo apt install genisoimage"; \
		echo "  Arch: sudo pacman -S cdrtools"; \
		exit 1; \
	fi
	@echo "ISO created: $@"

# Create ISO
iso: $(OS_ISO)

# Run in QEMU
run: $(OS_IMG)
	qemu-system-i386 -drive file=$(OS_IMG),format=raw -debugcon stdio -m 4G -enable-kvm -cpu host -machine accel=kvm,acpi=on

# Run ISO in QEMU
run-iso: $(OS_ISO)
	qemu-system-i386 -cdrom $(OS_ISO) -debugcon stdio -m 4G -enable-kvm -cpu host

# Debug in QEMU
debug: $(OS_IMG)
	qemu-system-i386 -drive file=$(OS_IMG),format=raw -s -S

# Clean build files
clean:
	rm -rf $(BUILD_DIR)

.PHONY: all dirs run run-iso iso debug clean