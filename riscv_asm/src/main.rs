fn main() {
    use std::collections::HashMap;
    use std::path::PathBuf;
    let mut sources = HashMap::new();
    sources.insert(
        PathBuf::from("test.asm"),
        r#"
    
#include "one.asm"

#macro TEST0 {TEST}
#macro TEST {asldkj}

TEST
TEST0
.text

_start:
    // Load the first number into register a0
    addi a0, x0, 5  // a0 = 5
    li a4, 12.3

    // Load the second number into register a1
    addi a1, x0, 10 // a1 = 10

    // Add the numbers and store the result in a2
    add a2, a0, a1  // a2 = a0 + a1


    // The result is now in a2.  You can add code here to store this to memory or exit
    // For a simple exit, we'll use a system call.
    addi a7, x0, 10 // Exit syscall number
    ecall



.section ".data"

text:
    .string "Hello, World!"
values:
    .values 12u32, 33u32
    
    "#
        .to_owned(),
    );
    assembler::with_bump(|bump| {
        let res = assembler::assemble_and_link(
            &sources,
            vec!["test.asm".as_ref()],
            bump,
            riscv_asm::RiscvAssembler::default(),
        );

        println!("{res}");
    });
}
