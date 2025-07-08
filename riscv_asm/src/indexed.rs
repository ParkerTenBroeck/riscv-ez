use std::fmt::{Display, Formatter};

use crate::{NodeVal, RiscvAssembler, reg::Register};
use assembler::{
    context::NodeId,
    expression::{Constant, ExpressionEvaluatorContext, Indexed, LabelUse, Value},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Index<'a> {
    LabelRegisterOffset(Register, LabelUse<'a>),
    RegisterOffset(Register, i32),
}

impl<'a> Indexed<'a, RiscvAssembler> for Index<'a> {
    fn from_indexed(
        ctx: &mut impl ExpressionEvaluatorContext<'a, RiscvAssembler>,
        node: NodeId<'a>,
        lhs: NodeVal<'a>,
        rhs: NodeVal<'a>,
    ) -> Value<'a, RiscvAssembler> {
        match (rhs.0, lhs.0) {
            (Value::Register(r), Value::Constant(Constant::I32(i))) => {
                Value::Indexed(Index::RegisterOffset(r, i))
            }
            (Value::Constant(Constant::I32(i)), Value::Register(r)) => {
                Value::Indexed(Index::RegisterOffset(r, i))
            }
            (Value::Indexed(Index::RegisterOffset(r, o)), Value::Constant(Constant::I32(i))) => {
                Value::Indexed(Index::RegisterOffset(r, o.wrapping_add(i)))
            }
            (Value::Constant(Constant::I32(i)), Value::Indexed(Index::RegisterOffset(r, o))) => {
                Value::Indexed(Index::RegisterOffset(r, o.wrapping_add(i)))
            }

            (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(LabelUse {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(LabelUse {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Label(l), Value::Register(r)) => {
                Value::Indexed(Index::LabelRegisterOffset(r, l))
            }
            (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(Index::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(Index::RegisterOffset(r, i))) => {
                Value::Indexed(Index::LabelRegisterOffset(
                    r,
                    LabelUse {
                        ident: l.ident,
                        offset: l.offset.wrapping_add(i),
                        meta: l.meta,
                    },
                ))
            }
            (Value::Indexed(Index::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(Index::LabelRegisterOffset(
                    r,
                    LabelUse {
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
                Value::Indexed(Index::default())
            }
        }
    }
}

impl<'a> Display for Index<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Index::LabelRegisterOffset(reg, l) => {
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
            Index::RegisterOffset(reg, offset) => {
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

impl<'a> Default for Index<'a> {
    fn default() -> Self {
        Self::RegisterOffset(Register::default(), 0)
    }
}
