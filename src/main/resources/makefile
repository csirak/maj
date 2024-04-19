# Specify the target file
TARGET = hello

# Specify the source file
SRC = hello.s

# Specify the linker script
LDSCRIPT = sifive_u/hello.ld

# Specify the include directory
INCLUDE = -Isifive_u

# Specify the RISC-V toolchain prefix
TOOLCHAIN_PREFIX = riscv64-unknown-elf-

# Compiler and linker flags
CFLAGS = -march=rv64g -mabi=lp64 -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles

# Default target
all: $(TARGET)

# Rule to compile the source file
$(TARGET): $(SRC)
	$(TOOLCHAIN_PREFIX)gcc $(CFLAGS) -T$(LDSCRIPT) $(INCLUDE) $< -o $@

# Run target
run: $(TARGET)
	qemu-system-riscv64 -no-reboot -nographic -machine sifive_u -bios none -kernel $(TARGET)

# Clean target
clean:
	rm -f $(TARGET)