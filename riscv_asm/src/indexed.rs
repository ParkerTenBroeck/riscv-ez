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

impl<'a> Indexed<'a, RiscvAssembler> for MemoryIndex<'a> {
    fn from_indexed(
        ctx: &mut impl ExpressionEvaluatorContext<'a, RiscvAssembler>,
        node: NodeId<'a>,
        lhs: NodeVal<'a>,
        rhs: NodeVal<'a>,
    ) -> Value<'a, RiscvAssembler> {
        match (rhs.0, lhs.0) {
            (Value::Register(r), Value::Constant(Constant::I32(i))) => {
                Value::Indexed(MemoryIndex::RegisterOffset(r, i))
            }
            (Value::Constant(Constant::I32(i)), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::RegisterOffset(r, i))
            }
            (
                Value::Indexed(MemoryIndex::RegisterOffset(r, o)),
                Value::Constant(Constant::I32(i)),
            ) => Value::Indexed(MemoryIndex::RegisterOffset(r, o.wrapping_add(i))),
            (
                Value::Constant(Constant::I32(i)),
                Value::Indexed(MemoryIndex::RegisterOffset(r, o)),
            ) => Value::Indexed(MemoryIndex::RegisterOffset(r, o.wrapping_add(i))),

            (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(Label {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(Label {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Label(l), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }
            (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(MemoryIndex::RegisterOffset(r, i))) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(
                    r,
                    Label {
                        ident: l.ident,
                        offset: l.offset.wrapping_add(i),
                        meta: l.meta,
                    },
                ))
            }
            (Value::Indexed(MemoryIndex::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(
                    r,
                    Label {
                        ident: l.ident,
                        offset: l.offset.wrapping_add(i),
                        meta: l.meta,
                    },
                ))
            }
            _ => {
                ctx.context().report_error(
                    node,
                    format!(
                        "Cannot index {} with {}",
                        lhs.0.get_type(),
                        rhs.0.get_type()
                    ),
                );
                Value::Indexed(MemoryIndex::default())
            }
        }
    }
}

impl<'a> Display for MemoryIndex<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            MemoryIndex::LabelRegisterOffset(reg, l) => {
                if l.meta.is_unset() && l.offset != 0 {
                    write!(f, "(")?;
                }
                write!(f, "{l}")?;
                if l.meta.is_unset() && l.offset != 0 {
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
