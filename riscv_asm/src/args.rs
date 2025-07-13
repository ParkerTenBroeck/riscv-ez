use crate::label::Label;
use crate::{Register, RiscvAssembler};
use assembler::assembler::{Assembler, lang::AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::{CoercedArg, Immediate};
use assembler::expression::{
    AssemblyRegister, Constant, ExpressionEvaluatorContext, ImplicitCastFrom, ImplicitCastTo as _,
    Indexed, Value, ValueType,
};
use std::fmt::{Display, Formatter};

pub enum RegOffset<'a, L: AssemblyLanguage<'a>> {
    Constant(L::Reg, i32),
    Label(L::Reg, Label<'a>),
}
impl<'a, L: AssemblyLanguage<'a>> Default for RegOffset<'a, L> {
    fn default() -> Self {
        Self::Constant(L::Reg::default(), 0)
    }
}
impl<'a> CoercedArg<'a, RiscvAssembler> for RegOffset<'a, RiscvAssembler> {
    const TYPE_REPR: &'static str = "indexed|register|label|<integer>";
    const HINT: ValueType<'a, RiscvAssembler> = ValueType::<'a, RiscvAssembler>::Indexed;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, RiscvAssembler>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => Ok(RegOffset::Constant(
                Default::default(),
                c.cast_with(node, context, context.config().implicit_cast_label_offset)
                    .ok_or(None)?,
            )),

            Value::Label(label) => Ok(RegOffset::Label(Default::default(), label)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct RegReg(pub Register);
impl<'a> CoercedArg<'a, RiscvAssembler> for RegReg {
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, RiscvAssembler> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, RiscvAssembler>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(r) if !r.is_regular() => Err(Some(
                "Expected regular register found floating register".into(),
            )),
            Value::Register(r) => Ok(RegReg(Register(r.0 & 0b11111))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct FloatReg(pub Register);
impl<'a> CoercedArg<'a, RiscvAssembler> for FloatReg {
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, RiscvAssembler> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, RiscvAssembler>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(r) if !r.is_floating() => Err(Some(
                "Expected floating register found regular register".into(),
            )),
            Value::Register(r) => Ok(FloatReg(Register(r.0 & 0b11111))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}
