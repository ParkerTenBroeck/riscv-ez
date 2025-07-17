use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

use assembler::expression::AssemblyRegister;

use crate::{RiscvAssembler, opcodes::*};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Register(pub u8);

impl Register {
    pub fn rd(&self) -> u32 {
        rd(self.0 as u32)
    }
    pub fn rs1(&self) -> u32 {
        rs1(self.0 as u32)
    }
    pub fn rs2(&self) -> u32 {
        rs2(self.0 as u32)
    }
    pub fn rs3(&self) -> u32 {
        rs3(self.0 as u32)
    }
}

pub struct InvalidRegisterName;

impl FromStr for Register {
    type Err = InvalidRegisterName;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "x0" | "zero" => Ok(Self(0)),
            "x1" | "ra" => Ok(Self(1)),
            "x2" | "sp" => Ok(Self(2)),
            "x3" | "gp" => Ok(Self(3)),
            "x4" | "tp" => Ok(Self(4)),
            "x5" | "t0" => Ok(Self(5)),
            "x6" | "t1" => Ok(Self(6)),
            "x7" | "t2" => Ok(Self(7)),
            "x8" | "s0" | "fp" => Ok(Self(8)),
            "x9" | "s1" => Ok(Self(9)),
            "x10" | "a0" => Ok(Self(10)),
            "x11" | "a1" => Ok(Self(11)),
            "x12" | "a2" => Ok(Self(12)),
            "x13" | "a3" => Ok(Self(13)),
            "x14" | "a4" => Ok(Self(14)),
            "x15" | "a5" => Ok(Self(15)),
            "x16" | "a6" => Ok(Self(16)),
            "x17" | "a7" => Ok(Self(17)),
            "x18" | "s2" => Ok(Self(18)),
            "x19" | "s3" => Ok(Self(19)),
            "x20" | "s4" => Ok(Self(20)),
            "x21" | "s5" => Ok(Self(21)),
            "x22" | "s6" => Ok(Self(22)),
            "x23" | "s7" => Ok(Self(23)),
            "x24" | "s8" => Ok(Self(24)),
            "x25" | "s9" => Ok(Self(25)),
            "x26" | "s10" => Ok(Self(26)),
            "x27" | "s11" => Ok(Self(27)),
            "x28" | "t3" => Ok(Self(28)),
            "x29" | "t4" => Ok(Self(29)),
            "x30" | "t5" => Ok(Self(30)),
            "x31" | "t6" => Ok(Self(31)),

            "f0" => Ok(Self(32)),
            "f1" => Ok(Self(32 + 1)),
            "f2" => Ok(Self(32 + 2)),
            "f3" => Ok(Self(32 + 3)),
            "f4" => Ok(Self(32 + 4)),
            "f5" => Ok(Self(32 + 5)),
            "f6" => Ok(Self(32 + 6)),
            "f7" => Ok(Self(32 + 7)),
            "f8" => Ok(Self(32 + 8)),
            "f9" => Ok(Self(32 + 9)),
            "f10" => Ok(Self(32 + 10)),
            "f11" => Ok(Self(32 + 11)),
            "f12" => Ok(Self(32 + 12)),
            "f13" => Ok(Self(32 + 13)),
            "f14" => Ok(Self(32 + 14)),
            "f15" => Ok(Self(32 + 15)),
            "f16" => Ok(Self(32 + 16)),
            "f17" => Ok(Self(32 + 17)),
            "f18" => Ok(Self(32 + 18)),
            "f19" => Ok(Self(32 + 19)),
            "f20" => Ok(Self(32 + 20)),
            "f21" => Ok(Self(32 + 21)),
            "f22" => Ok(Self(32 + 22)),
            "f23" => Ok(Self(32 + 23)),
            "f24" => Ok(Self(32 + 24)),
            "f25" => Ok(Self(32 + 25)),
            "f26" => Ok(Self(32 + 26)),
            "f27" => Ok(Self(32 + 27)),
            "f28" => Ok(Self(32 + 28)),
            "f29" => Ok(Self(32 + 29)),
            "f30" => Ok(Self(32 + 30)),
            "f31" => Ok(Self(32 + 31)),
            _ => Err(InvalidRegisterName),
        }
    }
}

impl Register {
    pub fn is_regular(self) -> bool {
        (0..32).contains(&self.0)
    }

    pub fn is_floating(self) -> bool {
        (32..64).contains(&self.0)
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let regs = [
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3",
            "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            "t3", "t4", "t5", "t6",
        ];
        match self.0 {
            0..32 => write!(f, "{}", regs[self.0 as usize]),
            32..64 => write!(f, "f{}", self.0),
            _ => write!(f, "UNKNOWN<{}>", self.0),
        }
    }
}

impl<'a> AssemblyRegister<'a> for Register {
    type Lang = RiscvAssembler;
}
