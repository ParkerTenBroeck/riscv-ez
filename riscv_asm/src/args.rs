use crate::{Register, RiscvAssembler};
use assembler::assembler::{Assembler, AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::{CoercedArg, Immediate};
use assembler::expression::{
    Constant, ConvertResult, ExpressionEvaluatorContext, Indexed, LabelUse, AssemblyRegister, Value, ValueType,
};
use std::fmt::{Display, Formatter};

pub enum RegOffset<'a, L: AssemblyLanguage<'a>> {
    Constant(L::RegType, i32),
    Label(L::RegType, LabelUse<'a>),
}
impl<'a, L: AssemblyLanguage<'a>> Default for RegOffset<'a, L> {
    fn default() -> Self {
        Self::Constant(L::RegType::default(), 0)
    }
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a, L> for RegOffset<'a, L> {
    const TYPE_REPR: &'static str = "indexed";
    const HINT: ValueType<'a, L> = ValueType::<'a, L>::Indexed;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                Constant::I32(value) => Ok(RegOffset::Constant(L::RegType::default(), value)),
                c => match c.to_i32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to i32 could lead to errors",
                        );
                        Ok(RegOffset::Constant(L::RegType::default(), val))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to i32 is lossy");
                        Ok(RegOffset::Constant(L::RegType::default(), val))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into i32",
                        value.get_type()
                    ))),
                },
            },
            Value::Label(label) => Ok(RegOffset::Label(L::RegType::default(), label)),
            // Value::RegisterOffset(r, o) => Ok(RegOffset::Constant(r, o)),
            // Value::LabelRegisterOffset(r, l) => Ok(RegOffset::Label(r, l)),
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
