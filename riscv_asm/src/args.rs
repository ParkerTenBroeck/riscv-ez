use crate::label::{Label, LabelExpr};
use crate::{Register, RiscvAssembler};
use assembler::assembler::{Assembler, lang::AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::CoercedArg;
use assembler::expression::{
    AssemblyRegister, Constant, ImplicitCastFrom, ImplicitCastTo as _, Indexed, Value, ValueType,
};
use std::fmt::{Display, Formatter};

pub enum RegOffset<'a> {
    Constant(Register, i32),
    Label(Register, LabelExpr<'a>),
}
impl<'a> Default for RegOffset<'a> {
    fn default() -> Self {
        Self::Constant(Register::default(), 0)
    }
}
impl<'a> CoercedArg<'a> for RegOffset<'a> {
    type LANG = RiscvAssembler<'a>;
    const TYPE_REPR: &'static str = "indexed|register|label|<integer>";
    const HINT: ValueType<'a, RiscvAssembler<'a>> = ValueType::<'a, RiscvAssembler>::Indexed;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, RiscvAssembler<'a>>,
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
impl<'a> CoercedArg<'a> for RegReg {
    type LANG = RiscvAssembler<'a>;
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, RiscvAssembler<'a>> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, RiscvAssembler<'a>>,
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
impl<'a> CoercedArg<'a> for FloatReg {
    type LANG = RiscvAssembler<'a>;
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, RiscvAssembler<'a>> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, RiscvAssembler<'a>>,
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

pub enum Immediate<'a, L: AssemblyLanguage<'a>> {
    SignedConstant(i32),
    UnsignedConstant(u32),
    Label(L::Label),
}
impl<'a, L: AssemblyLanguage<'a>> Default for Immediate<'a, L> {
    fn default() -> Self {
        Self::UnsignedConstant(0)
    }
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Immediate<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "<integer>|label";
    const HINT: ValueType<'a, L> = ValueType::I32;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                c if c.is_signed_integer() => Ok(Immediate::SignedConstant(
                    c.cast(node, context).ok_or(None)?,
                )),
                c if c.is_unsigned_integer() => Ok(Immediate::UnsignedConstant(
                    c.cast(node, context).ok_or(None)?,
                )),
                _ => {
                    context.report_error(
                        node,
                        format!("expected <integer> constant found {}", value.get_type()),
                    );
                    Err(None)
                }
            },
            Value::Label(label) => Ok(Immediate::Label(label)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}
