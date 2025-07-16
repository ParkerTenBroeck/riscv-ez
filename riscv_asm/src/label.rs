use std::fmt::{Display, Formatter};

use assembler::expression::{AssemblyLabel, AssemblyRegister};

use crate::RiscvAssembler;

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct LabelMeta{
    pub kind: Option<RelocKind>,
    pub pattern: Option<RelocPattern>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RelocPattern{

}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RelocKind {
    PcRel,
    Absolute,
    Size,
    Align,
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
            meta: Default::default(),
        }
    }

    pub fn offset(mut self, offset: i32) -> Self {
        self.offset = self.offset.wrapping_add(offset);
        self
    }
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.meta.kind {
            Some(RelocKind::PcRel) => write!(f, "pc_rel(")?,
            Some(RelocKind::Absolute) => write!(f, "absolute(")?,
            Some(RelocKind::Size) => write!(f, "size(")?,
            Some(RelocKind::Align) => write!(f, "align(")?,
            _ => {}
        }
        match self.meta.pattern {
            _ => {}
        }
        write!(f, "{}", self.ident)?;
        if self.offset != 0 {
            write!(f, "+{}", self.offset)?;
        }
        if !self.meta.kind.is_none() {
            write!(f, ")")?;
        }
        if !self.meta.pattern.is_none() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<'a> AssemblyLabel<'a, RiscvAssembler> for Label<'a> {
    type Offset = i32;
}
