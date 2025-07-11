use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64};

use mem::SharedMem;

use std::sync::atomic::Ordering as O;

#[derive(Default, Debug)]
pub struct RV32IMAState {
    pub regs: [AtomicU32; 32],
    pub pc: AtomicU32,
    pub mstatus: AtomicU32,
    pub cycle: AtomicU64,
    pub timer: AtomicU64,
    pub timermatch: AtomicU64,
    pub mscratch: AtomicU32,
    pub mtvec: AtomicU32,
    pub mie: AtomicU32,
    pub mip: AtomicU32,
    pub mepc: AtomicU32,
    pub mtval: AtomicU32,
    pub mcause: AtomicU32,

    pub extrafalgs: AtomicU32,
    pub exit: AtomicBool,
}

macro_rules! opcode {
    ($ir:expr) => {
        ($ir) & 0b1111111
    };
}

macro_rules! func7 {
    ($ir:expr) => {
        ($ir) >> 25
    };
}

macro_rules! func3 {
    ($ir:expr) => {
        (($ir) >> 12) & 0b111
    };
}

macro_rules! rd {
    ($ir:expr) => {
        (($ir) >> 7) & 0b11111
    };
}

macro_rules! rs1 {
    ($ir:expr) => {
        (($ir) >> 15) & 0b11111
    };
}

macro_rules! rs2 {
    ($ir:expr) => {
        (($ir) >> 20) & 0b11111
    };
}

macro_rules! imm_u {
    ($ir:expr) => {
        ($ir) & 0xfffff000
    };
}

macro_rules! imm_i {
    ($ir:expr) => {
        ($ir as i32 & (0b11111 << 20)) >> 20
    };
}

macro_rules! imm_i_u {
    ($ir:expr) => {
        $ir >> 20
    };
}

macro_rules! imm_b {
    ($ir:expr) => {
        (((($ir & 0xf00) >> 7)
            | (($ir & 0x7e000000) >> 20)
            | (($ir & 0x80) << 4)
            | (($ir >> 31) << 12)) as i32)
            << 19
            >> 19
    };
}

macro_rules! imm_s {
    ($ir: expr) => {
        (((($ir >> 7) & 0x1f) | (($ir & 0xfe000000) >> 20)) as i32) >> 20 << 20
    };
}

macro_rules! imm_j {
    ($ir:expr) => {
        (((($ir & 0x80000000) >> 11)
            | (($ir & 0x7fe00000) >> 20)
            | (($ir & 0x00100000) >> 9)
            | ($ir & 0x000ff000)) as i32)
            << 11
            >> 11
    };
}

impl RV32IMAState {
    fn invalid_op_code(&self) {}
    pub fn step(&self, mem: &SharedMem) {
        self.exit.store(true, O::Relaxed);
        self.run(mem);
    }

    pub fn run(&self, mem: &SharedMem) {
        let mut pc = self.pc.load(O::Relaxed);
        if pc & 0b11 != 0 {
            todo!()
        }

        loop {
            let ir: u32 = mem.read(pc);

            macro_rules! rd_set {
                ($e:expr) => {{
                    let d = rd!(ir);
                    if(d!=0){
                        self.regs[d as usize].store($e as u32, O::Relaxed);
                    }
                }};
            }

            macro_rules! rs1_get {
                () => {
                    self.regs[rs1!(ir) as usize].load(O::Relaxed) as i32
                };
            }

            macro_rules! rs2_get {
                () => {
                    self.regs[rs2!(ir) as usize].load(O::Relaxed) as i32
                };
            }
            'skip_pc: {
                match opcode!(ir) {
                    // I instructions
                    0b0110111 => rd_set!(imm_u!(ir)), //LUI
                    0b0010111 => rd_set!(pc.wrapping_add(imm_u!(ir))), // AUIPC
                    0b1101111 => {
                        // JAL
                        rd_set!(pc.wrapping_add(4));
                        pc = pc.wrapping_add(imm_j!(ir) as u32);
                    }
                    0b1100111 => {
                        // JALR
                        rd_set!(pc.wrapping_add(4));
                        pc = (pc as i32)
                            .wrapping_add(imm_i!(ir))
                            .wrapping_add(rs1_get!()) as u32;
                    }
                    0b1100011 => {
                        let cond = match func3!(ir) {
                            0b000 => rs1_get!() == rs2_get!(),
                            0b001 => rs1_get!() != rs2_get!(),
                            0b100 => rs1_get!() < rs2_get!(),
                            0b101 => rs1_get!() >= rs2_get!(),
                            0b110 => (rs1_get!() as u32) < rs2_get!() as u32,
                            0b111 => rs1_get!() as u32 >= rs2_get!() as u32,
                            _ => break 'skip_pc self.invalid_op_code(),
                        };
                        if cond {
                            pc = pc.wrapping_add(imm_b!(ir) as u32);
                        }
                    }
                    0b0000011 => match func3!(ir) {
                        0b000 => rd_set!(
                            mem.read::<i8>(rs1_get!().wrapping_add(imm_i!(ir)) as u32)
                                .to_le() as i32
                        ), // LB
                        0b001 => rd_set!(
                            mem.read::<i16>(rs1_get!().wrapping_add(imm_i!(ir)) as u32)
                                .to_le() as i32
                        ), // LH
                        0b010 => rd_set!(
                            mem.read::<i32>(rs1_get!().wrapping_add(imm_i!(ir)) as u32)
                                .to_le()
                        ), // LW
                        0b100 => rd_set!(
                            mem.read::<u8>(rs1_get!().wrapping_add(imm_i!(ir)) as u32)
                                .to_le()
                        ), // LBU
                        0b101 => rd_set!(
                            mem.read::<u16>(rs1_get!().wrapping_add(imm_i!(ir)) as u32)
                                .to_le()
                        ), // LHU
                        _ => break 'skip_pc self.invalid_op_code(),
                    },
                    0b0100011 => match func3!(ir) {
                        0b000 => mem.write(
                            rs1_get!().wrapping_add(imm_s!(ir)) as u32,
                            (rs2_get!() as u8).to_le(),
                        ), //SB
                        0b001 => mem.write(
                            rs1_get!().wrapping_add(imm_s!(ir)) as u32,
                            (rs2_get!() as u16).to_le(),
                        ), //SH
                        0b010 => mem.write(
                            rs1_get!().wrapping_add(imm_s!(ir)) as u32,
                            rs2_get!().to_le(),
                        ), //SW
                        _ => break 'skip_pc self.invalid_op_code(),
                    },
                    0b0010011 => match func3!(ir) {
                        0b000 => rd_set!(rs1_get!() + imm_i!(ir)), // ADDI
                        0b001 => rd_set!(rs1_get!() << rs2!(ir)),  // SLLI
                        0b010 => rd_set!(rs1_get!() < imm_i!(ir)), // STLI
                        0b011 => rd_set!(rs1_get!() as u32 > imm_i!(ir) as u32), // STLIU
                        0b100 => rd_set!(rs1_get!() ^ imm_i!(ir)), // XORI
                        0b101 => match func7!(ir) {
                            0b0000000 => rd_set!(rs1_get!() as u32 >> rs2!(ir)), // SRLI
                            0b0100000 => rd_set!(rs1_get!() >> rs2!(ir)),        // SRAI
                            _ => break 'skip_pc self.invalid_op_code(),
                        },
                        0b110 => rd_set!(rs1_get!() | imm_i!(ir)), // ORI
                        0b111 => rd_set!(rs1_get!() & imm_i!(ir)), // ANDI
                        _ => unreachable!(),
                    },
                    0b0110011 => match func7!(ir) {
                        0b0000000 => match func3!(ir) {
                            0b000 => rd_set!(rs1_get!() + rs2_get!()),  // ADD
                            0b001 => rd_set!(rs1_get!() << rs2_get!()), // SLL
                            0b010 => rd_set!(rs1_get!() < rs2_get!()),  // STL
                            0b011 => rd_set!(rs1_get!() as u32 > rs2_get!() as u32), // STLU
                            0b100 => rd_set!(rs1_get!() ^ rs2_get!()),  // XOR
                            0b101 => rd_set!(rs1_get!() as u32 >> rs2_get!()), // SRL
                            0b110 => rd_set!(rs1_get!() | rs2_get!()),  // OR
                            0b111 => rd_set!(rs1_get!() & rs2_get!()),  // AND
                            _ => unreachable!(),
                        },
                        0b01000000 => match func3!(ir) {
                            0b000 => rd_set!(rs1_get!() - rs2_get!()),  // SUB
                            0b101 => rd_set!(rs1_get!() >> rs2_get!()), // SRA
                            _ => break 'skip_pc self.invalid_op_code(),
                        },
                        0b0000001 => match func3!(ir) {
                            // M extension
                            0b000 => rd_set!(rs1_get!().wrapping_mul(rs2_get!())), //MUL
                            0b001 => {
                                rd_set!((rs1_get!() as i64).wrapping_mul(rs2_get!() as i64) >> 32)
                            } //MULH
                            0b010 => rd_set!(
                                (rs1_get!() as i64).wrapping_mul(rs2_get!() as u32 as i64) >> 32
                            ), //MULHSU
                            0b011 => rd_set!(
                                (rs1_get!() as u32 as u64).wrapping_mul(rs2_get!() as u32 as u64)
                                    >> 32
                            ), //MULHU
                            0b100 => {
                                //DIV
                                let t = rs2_get!();
                                if t == 0 {
                                    rd_set!(u32::MAX)
                                } else {
                                    rd_set!(rs1_get!().wrapping_div(t))
                                }
                            }
                            0b101 => {
                                //DIVU
                                let t = rs2_get!() as u32;
                                if t == 0 {
                                    rd_set!(u32::MAX)
                                } else {
                                    rd_set!((rs1_get!() as u32).wrapping_div(t))
                                }
                            }
                            0b110 => {
                                //REM
                                let t = rs2_get!();
                                if t == 0 {
                                    rd_set!(u32::MAX)
                                } else {
                                    rd_set!(t.wrapping_rem(t))
                                }
                            }
                            0b111 => {
                                //REMU
                                let t = rs2_get!() as u32;
                                if t == 0 {
                                    rd_set!(u32::MAX)
                                } else {
                                    rd_set!((rs1_get!() as u32).wrapping_rem(t))
                                }
                            }
                            _ => unreachable!(),
                        },
                        _ => break 'skip_pc self.invalid_op_code(),
                    },

                    0b0101111 => {
                        // A extension
                        break 'skip_pc self.invalid_op_code();
                    }
                    // F extension
                    0b0000111 => {} //FLW
                    0b0100111 => {} //FLS
                    0b1000011 => {} // FMADD.S
                    0b1000111 => {} // FMSUB.S
                    0b1001011 => {} // FNMSUB.S
                    0b1001111 => {} // FNMADD.S
                    0b1010011 => match func7!(ir) {
                        0b0 => {}
                        _ => break 'skip_pc self.invalid_op_code(),
                    },

                    0b0001111 => {
                        // Zifencei
                        match func3!(ir) {
                            0b000 => {} //fence
                            0b001 => {} //fence.i
                            _ => break 'skip_pc self.invalid_op_code(),
                        }
                    }
                    0b1110011 => match imm_i_u!(ir) {
                        // Zicsr
                        0b000000000000 => self.ecall(mem), // ECALL
                        0b000000000001 => self.ebreak(),   // EBREAK
                        _ => break 'skip_pc self.invalid_op_code(),
                    },

                    _ => break 'skip_pc self.invalid_op_code(),
                }
            }
            if self.exit.load(O::Relaxed) {
                self.exit.store(false, O::Relaxed);
                break;
            }
        }
        self.pc.store(pc.wrapping_add(4), O::Relaxed);
    }

    fn ebreak(&self) {
        self.exit.store(true, O::Relaxed);
    }

    fn ecall(&self, _mem: &SharedMem) {
        println!("{self:#?}");
    }
}
