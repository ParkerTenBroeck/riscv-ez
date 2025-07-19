riscv64-none-elf-gcc test.c -fpie -march=rv32imaf_zicsr -mabi=ilp32f -nolibc -nostdlib -O1 -fno-inline -Wl,-i,-q
riscv64-none-elf-readelf a.out -a 
riscv64-none-elf-objdump a.out -dStrR -D

