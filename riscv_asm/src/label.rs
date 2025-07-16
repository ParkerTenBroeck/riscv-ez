use std::fmt::{Display, Formatter};

use assembler::expression::{AssemblyLabel, AssemblyRegister};

use crate::RiscvAssembler;

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct LabelMeta {
    pub kind: Option<RelocKind>,
    pub pattern: Option<RelocPattern>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RelocPattern {
    High,
    Low,
}

impl std::fmt::Display for RelocPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RelocPattern::High => write!(f, "hi"),
            RelocPattern::Low => write!(f, "lo"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RelocKind {
    PcRel,
    Absolute,
    GotPcRel,
    Size,
    Align,
}

impl std::fmt::Display for RelocKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RelocKind::PcRel => write!(f, "pcrel"),
            RelocKind::Absolute => write!(f, "absolute"),
            RelocKind::GotPcRel => write!(f, "got_pcrel"),
            RelocKind::Size => write!(f, "size"),
            RelocKind::Align => write!(f, "align"),
        }
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
        if let Some(kind) = self.meta.kind {
            write!(f, "{kind}")?
        }
        if let Some(pattern) = self.meta.pattern {
            write!(f, "{pattern}")?
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
