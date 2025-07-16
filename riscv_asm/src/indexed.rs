use std::fmt::{Display, Formatter};

use crate::{NodeVal, RiscvAssembler, label::Label, reg::Register};
use assembler::{
    context::NodeId,
    expression::{Constant, ExpressionEvaluatorContext, Indexed, Value},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MemoryIndex<'a> {
    LabelRegisterOffset(Register, Label<'a>),
    RegisterOffset(Register, i32),
}

impl<'a> Indexed<'a, RiscvAssembler> for MemoryIndex<'a> {}

impl<'a> Display for MemoryIndex<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            MemoryIndex::LabelRegisterOffset(reg, l) => {
                if l.meta.kind.is_none() && l.meta.pattern.is_none() && l.offset != 0 {
                    write!(f, "(")?;
                }
                write!(f, "{l}")?;
                if l.meta.kind.is_none() && l.meta.pattern.is_none() && l.offset != 0 {
                    write!(f, ")")?;
                }
                if reg.0 != 0 {
                    write!(f, "[{reg}]")?;
                }
                Ok(())
            }
            MemoryIndex::RegisterOffset(reg, offset) => {
                if reg.0 == 0 {
                    write!(f, "{offset}")?;
                } else {
                    write!(f, "{reg}")?;
                    if offset != 0 {
                        write!(f, "[{offset}]")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl<'a> Default for MemoryIndex<'a> {
    fn default() -> Self {
        Self::RegisterOffset(Register::default(), 0)
    }
}
