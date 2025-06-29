use crate::lex::Number;

pub enum Instruction<'a> {
    Rtype {
        opcode: RTypeOpCode,
        immediate: Immediate<'a>,
        rd: Register,
        rs1: Register,
        rs2: Register,
    },
    IType {
        opcode: ITypeOpCode,
        immediate: Immediate<'a>,
        rs1: Register,
        rd: Register,
    },
    SType {
        opcode: STypeOpCode,
        immediate: Immediate<'a>,
        rs1: Register,
        rs2: Register,
    },
    BType {
        opcode: BTypeOpCode,
        immediate: Immediate<'a>,
        rs1: Register,
        rs2: Register,
    },
    UType {
        opcode: UTypeOpCode,
        immediate: Immediate<'a>,
        rd: Register,
    },
    JType {
        opcode: JTypeOpCode,
        immediate: Immediate<'a>,
        rd: Register,
    },
}

const fn rtype(opcode: u32, func3: u32, func7: u32) -> u32 {
    opcode | (func7 << 25) | (func3 << 12)
}

#[repr(u32)]
pub enum RTypeOpCode {
    Slli = rtype(0b0010011, 0b001, 0b0000000),
    Srli = rtype(0b0010011, 0b101, 0b0000000),
    Srai = rtype(0b0010011, 0b101, 0b0100000),

    Add = rtype(0b0110011, 0b000, 0b0000000),
    Sub = rtype(0b0110011, 0b000, 0b0100000),
    Sll = rtype(0b0110011, 0b001, 0b0000000),
    Slt = rtype(0b0110011, 0b010, 0b0000000),
    Sltu = rtype(0b0110011, 0b011, 0b0000000),
    Xor = rtype(0b0110011, 0b100, 0b0000000),
    Srl = rtype(0b0110011, 0b101, 0b0000000),
    Sra = rtype(0b0110011, 0b101, 0b0100000),
    Or = rtype(0b0110011, 0b110, 0b0000000),
    And = rtype(0b0110011, 0b111, 0b0000000),

    LrW = rtype(0b0101111, 0b011, 0b0000010),
    ScW = rtype(0b0101111, 0b011, 0b0000011),

    AmoswapW = rtype(0b0101111, 0b011, 0b00001 << 2),
    SmoaddW = rtype(0b0101111, 0b011, 0b00000 << 2),
    AmoxorW = rtype(0b0101111, 0b011, 0b00100 << 2),
    AmoandW = rtype(0b0101111, 0b011, 0b01100 << 2),
    AmoorW = rtype(0b0101111, 0b011, 0b01000 << 2),
    AmoMinW = rtype(0b0101111, 0b011, 0b10100 << 2),
    AmoManW = rtype(0b0101111, 0b011, 0b11000 << 2),
    AmoMaxuW = rtype(0b0101111, 0b011, 0b11100 << 2),
}

const fn isbtype(opcode: u32, func3: u32, func7: u32) -> u32 {
    opcode | (func7 << 25) | (func3 << 12)
}

#[repr(u32)]
pub enum ITypeOpCode {
    Jalr,

    Lb,
    Lh,
    Lw,
    Lbu,
    Lhu,

    Addi,
    Slti,
    Sltiu,
    Xori,
    Ori,
    Andi,

    Fence,
    FenceTso,
    Pause,
    ECall,
    EBreak,

    FenceI,
    Csrrw,
    Csrrs,
    Csrrc,
    Csrrwi,
    Csrrsi,
    Csrrci,
}

#[repr(u32)]
pub enum STypeOpCode {
    Sb,
    Sh,
    Sw,
}

#[repr(u32)]
pub enum BTypeOpCode {
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,
}

#[repr(u32)]
pub enum UTypeOpCode {
    Lui,
    Auipc,
}

#[repr(u32)]
pub enum JTypeOpCode {
    Jal,
}

pub struct Register(pub u8);

pub enum Immediate<'a> {
    Label(&'a str),
    Number(Number<'a>),
    Expression(),
}
