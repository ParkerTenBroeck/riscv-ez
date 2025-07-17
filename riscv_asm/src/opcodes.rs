pub const fn opcode(opcode: u32) -> u32 {
    opcode & 0b1111111
}

pub const fn func3(func: u32) -> u32 {
    (func & 0b111) << 12
}

pub const fn func7(func: u32) -> u32 {
    (func & 0b1111111) << 25
}

pub const fn func5(func: u32) -> u32 {
    (func & 0b11111) << 27
}

pub const fn acquire(acquire: bool) -> u32 {
    (acquire as u32) << 26
}

pub const fn release(release: bool) -> u32 {
    (release as u32) << 25
}

pub const fn rs3(reg: u32) -> u32 {
    (reg & 0b11111) << 27
}

pub const fn rs2(reg: u32) -> u32 {
    (reg & 0b11111) << 20
}

pub const fn rs1(reg: u32) -> u32 {
    (reg & 0b11111) << 15
}

pub const fn rd(reg: u32) -> u32 {
    (reg & 0b11111) << 7
}

pub const fn imm_11_0_s(imm: u32) -> u32 {
    func7(imm >> 5) | rs2(imm)
}

pub const fn imm_31_12_u(imm: u32) -> u32 {
    (imm & 0xFFFFF) << 12
}

pub const fn into_12_bit_sign(value: i32) -> bool {
    -0x800 <= value && value <= 0x7FF
}

pub const fn into_12_bit_sign_usg(value: u32) -> bool {
    value <= 0x7FF
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum RTypeOpCode {
    Slli = opcode(0b0010011) | func3(0b001),
    Srli = opcode(0b0010011) | func3(0b101),
    Srai = opcode(0b0010011) | func3(0b101) | func7(0b0100000),

    Add = opcode(0b0110011) | func3(0b000),
    Sub = opcode(0b0110011) | func3(0b000) | func7(0b0100000),
    Sll = opcode(0b0110011) | func3(0b001),
    Slt = opcode(0b0110011) | func3(0b010),
    Sltu = opcode(0b0110011) | func3(0b011),
    Xor = opcode(0b0110011) | func3(0b100),
    Srl = opcode(0b0110011) | func3(0b101),
    Sra = opcode(0b0110011) | func3(0b101) | func7(0b0100000),
    Or = opcode(0b0110011) | func3(0b110),
    And = opcode(0b0110011) | func3(0b111),

    LrW = opcode(0b0101111) | func3(0b011) | func5(0b0000010),
    ScW = opcode(0b0101111) | func3(0b011) | func5(0b0000011),

    AmoswapW = opcode(0b0101111) | func3(0b011) | func5(0b00001),
    SmoaddW = opcode(0b0101111) | func3(0b011) | func5(0b00000),
    AmoxorW = opcode(0b0101111) | func3(0b011) | func5(0b00100),
    AmoandW = opcode(0b0101111) | func3(0b011) | func5(0b01100),
    AmoorW = opcode(0b0101111) | func3(0b011) | func5(0b01000),
    AmoMinW = opcode(0b0101111) | func3(0b011) | func5(0b10100),
    AmoMaxW = opcode(0b0101111) | func3(0b011) | func5(0b11000),
    AmoMaxuW = opcode(0b0101111) | func3(0b011) | func5(0b11100),

    Mul = opcode(0x0110011) | func3(0x0) | func5(0x01),
    Mulh = opcode(0x0110011) | func3(0x1) | func5(0x01),
    Mulsu = opcode(0x0110011) | func3(0x2) | func5(0x01),
    Mulu = opcode(0x0110011) | func3(0x3) | func5(0x01),
    Div = opcode(0x0110011) | func3(0x4) | func5(0x01),
    Divu = opcode(0x0110011) | func3(0x5) | func5(0x01),
    Rem = opcode(0x0110011) | func3(0x6) | func5(0x01),
    Remu = opcode(0x0110011) | func3(0x7) | func5(0x01),
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ITypeOpCode {
    Jalr = opcode(0b1100111) | func3(0x0),

    Lb = opcode(0b0000011) | func3(0x0),
    Lh = opcode(0b0000011) | func3(0x1),
    Lw = opcode(0b0000011) | func3(0x2),
    Lbu = opcode(0b0000011) | func3(0x4),
    Lhu = opcode(0b0000011) | func3(0x5),

    Addi = opcode(0b0010011) | func3(0x0),
    Slli = opcode(0b0010011) | func3(0x1),
    Slti = opcode(0b0010011) | func3(0x2),
    Sltiu = opcode(0b0010011) | func3(0x3),
    Xori = opcode(0b0010011) | func3(0x4),
    Srli = opcode(0b0010011) | func3(0x5),
    Srai = opcode(0b0010011) | func3(0x5) | func7(0x20),
    Ori = opcode(0b0010011) | func3(0x6),
    Andi = opcode(0b0010011) | func3(0x7),

    Fence,
    FenceTso,
    Pause,
    ECall = opcode(0b1110011) | imm_11_0_s(0x0),
    EBreak = opcode(0b1110011) | imm_11_0_s(0x1),

    FenceI,
    Csrrw,
    Csrrs,
    Csrrc,
    Csrrwi,
    Csrrsi,
    Csrrci,
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum STypeOpCode {
    Sb = opcode(0b0100011) | func3(0x0),
    Sh = opcode(0b0100011) | func3(0x1),
    Sw = opcode(0b0100011) | func3(0x2),
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BTypeOpCode {
    Beq = opcode(0b1100011) | func3(0x0),
    Bne = opcode(0b1100011) | func3(0x1),
    Blt = opcode(0b1100011) | func3(0x4),
    Bge = opcode(0b1100011) | func3(0x5),
    Bltu = opcode(0b1100011) | func3(0x6),
    Bgeu = opcode(0b1100011) | func3(0x7),
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UTypeOpCode {
    Lui = opcode(0b0110111),
    Auipc = opcode(0b0010111),
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum JTypeOpCode {
    Jal = opcode(0b0110111),
}
