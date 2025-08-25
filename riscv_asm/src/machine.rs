use assembler::simple::trans::TranslationUnitMachine;
use assembler::simple::trans::reloc::*;

#[derive(Clone, Copy, Debug, Default)]

pub struct RiscvMachine {}

impl TranslationUnitMachine for RiscvMachine {
    type PtrSizeType = u32;
    type Reloc = RiscvReloc;
}

#[derive(Clone, Copy, Debug)]
pub struct RiscvReloc {}

impl Reloc for RiscvReloc {}
