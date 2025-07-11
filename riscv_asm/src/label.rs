use std::fmt::{Display, Formatter};

use assembler::expression::{AssemblyLabel, AssemblyRegister};

use crate::RiscvAssembler;

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub enum LabelMeta {
    PcRel,
    Absolute,
    Size,
    Align,
    #[default]
    Unset,
}

impl LabelMeta {
    pub fn is_unset(&self) -> bool {
        matches!(self, LabelMeta::Unset)
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct Label<'a> {
    pub ident: &'a str,
    pub offset: i32,
    pub meta: LabelMeta,
}

impl<'a> Label<'a> {
    pub fn new(ident: &'a str) -> Self {
        Label {
            ident,
            offset: 0,
            meta: LabelMeta::Unset,
        }
    }
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.meta {
            LabelMeta::PcRel => write!(f, "pc_rel(")?,
            LabelMeta::Absolute => write!(f, "absolute(")?,
            LabelMeta::Size => write!(f, "size(")?,
            LabelMeta::Align => write!(f, "align(")?,
            LabelMeta::Unset => {}
        }
        write!(f, "{}", self.ident)?;
        if self.offset != 0 {
            write!(f, "+{}", self.offset)?;
        }
        if !self.meta.is_unset() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<'a> AssemblyLabel<'a, RiscvAssembler> for Label<'a> {
    type Offset = i32;

    fn add_constant_offset(mut self, offset: Self::Offset) -> Self {
        self.offset += offset;
        self
    }

    fn sub_constant_offset(mut self, offset: Self::Offset) -> Self {
        self.offset -= offset;
        self
    }
}
