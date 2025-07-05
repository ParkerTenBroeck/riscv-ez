use crate::assembler::instructions::Register;
use crate::context::{Context, Node, NodeId};
use crate::expression::{
    ArgumentsTypeHint, Constant, ConvertResult, ExpressionEvaluatorContext, LabelUse, Value,
    ValueType,
};
use crate::util::IntoStrDelimable;

#[derive(Default)]
pub struct U32Opt(pub Option<u32>);
impl<'a> CoercedArg<'a> for U32Opt {
    const TYPE_REPR: &'static str = "u32";
    const HINT: ValueType = ValueType::U32;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                Constant::U32(value) => Ok(U32Opt(Some(value))),
                c => match c.to_u32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to u32 could lead to errors",
                        );
                        Ok(U32Opt(Some(val)))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to u32 is lossy");
                        Ok(U32Opt(Some(val)))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into u32",
                        value.get_type()
                    ))),
                },
            },
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct U32Power2Opt(pub Option<u32>);
impl<'a> CoercedArg<'a> for U32Power2Opt {
    const TYPE_REPR: &'static str = "u32";
    const HINT: ValueType = ValueType::U32;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        fn chk_pow<'a>(context: &Context<'a>, value: u32, node: NodeId<'a>) -> Option<u32> {
            if value.is_power_of_two() {
                Some(value)
            } else {
                context.report_error(node, "Value is not a power of two");
                None
            }
        }
        match value {
            Value::Constant(c) => match c {
                Constant::U32(val) => Ok(U32Power2Opt(chk_pow(context, val, node))),
                c => match c.to_u32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to u32 could lead to errors",
                        );
                        Ok(U32Power2Opt(chk_pow(context, val, node)))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to u32 is lossy");
                        Ok(U32Power2Opt(chk_pow(context, val, node)))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into u32",
                        value.get_type()
                    ))),
                },
            },
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct StrOpt<'a>(pub Option<&'a str>);
impl<'a> CoercedArg<'a> for StrOpt<'a> {
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType = ValueType::String;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::String(str)) => Ok(StrOpt(Some(str))),
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct RegReg(pub Register);
impl<'a> CoercedArg<'a> for RegReg {
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType = ValueType::Register;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(r) if !r.is_regular() => Err(Some(
                "Expected regular register found floating register".into(),
            )),
            Value::Register(r) => Ok(RegReg(Register(r.0 & 0b11111))),
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct FloatReg(Register);
impl<'a> CoercedArg<'a> for FloatReg {
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType = ValueType::Register;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(r) if !r.is_floating() => Err(Some(
                "Expected floating register found regular register".into(),
            )),
            Value::Register(r) => Ok(FloatReg(Register(r.0 & 0b11111))),
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

pub enum Immediate<'a> {
    SignedConstant(i32),
    UnsignedConstant(u32),
    Label(LabelUse<'a>),
}
impl<'a> Default for Immediate<'a> {
    fn default() -> Self {
        Self::UnsignedConstant(0)
    }
}
impl<'a> CoercedArg<'a> for Immediate<'a> {
    const TYPE_REPR: &'static str = "i32|label";
    const HINT: ValueType = ValueType::I32;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                Constant::I32(value) => Ok(Immediate::SignedConstant(value)),
                Constant::U32(value) => Ok(Immediate::UnsignedConstant(value)),
                c @ (Constant::U8(_) | Constant::U16(_) | Constant::U64(_)) => match c.to_u32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to u32 could lead to errors",
                        );
                        Ok(Immediate::UnsignedConstant(val))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to u32 is lossy");
                        Ok(Immediate::UnsignedConstant(val))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into u32",
                        value.get_type()
                    ))),
                }
                c => match c.to_i32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to i32 could lead to errors",
                        );
                        Ok(Immediate::SignedConstant(val))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to i32 is lossy");
                        Ok(Immediate::SignedConstant(val))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into i32",
                        value.get_type()
                    ))),
                },
            },
            Value::Label(label) => Ok(Immediate::Label(label)),
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

pub enum RegOffset<'a> {
    Constant(Register, i32),
    Label(Register, LabelUse<'a>),
}
impl<'a> Default for RegOffset<'a> {
    fn default() -> Self {
        Self::Constant(Register::default(), 0)
    }
}
impl<'a> CoercedArg<'a> for RegOffset<'a> {
    const TYPE_REPR: &'static str = "indexed";
    const HINT: ValueType = ValueType::Indexed;

    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) => match c {
                Constant::I32(value) => Ok(RegOffset::Constant(Register::default(), value)),
                c => match c.to_i32() {
                    ConvertResult::Success(val) => {
                        context.report_warning(
                            node,
                            "implicit conversion to i32 could lead to errors",
                        );
                        Ok(RegOffset::Constant(Register::default(), val))
                    }
                    ConvertResult::Lossy(val) => {
                        context.report_warning(node, "conversion to i32 is lossy");
                        Ok(RegOffset::Constant(Register::default(), val))
                    }
                    ConvertResult::Failure => Err(Some(format!(
                        "Cannot convert {} into i32",
                        value.get_type()
                    ))),
                },
            },
            Value::Label(label) => Ok(RegOffset::Label(Register::default(), label)),
            Value::RegisterOffset(r, o) => Ok(RegOffset::Constant(r, o)),
            Value::LabelRegisterOffset(r, l) => Ok(RegOffset::Label(r, l)),
            _ => Err(None),
        }
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Default::default()
    }
}

impl<'a, T: CoercedArg<'a>> CoercedArg<'a> for Node<'a, T> {
    const TYPE_REPR: &'static str = T::TYPE_REPR;
    const HINT: ValueType = T::HINT;

    fn from(context: &Context<'a>, node: NodeId<'a>, value: Value<'a>) -> Result<Self, Option<String>> {
        T::from(context, node, value).map(|v| Node(v, node))
    }

    fn default(context: &Context<'a>, node: NodeId<'a>) -> Self {
        Node(T::default(context, node), node)
    }
}

pub trait CoercedArg<'a>: Sized {
    const TYPE_REPR: &'static str;
    const HINT: ValueType;
    fn from(
        context: &Context<'a>,
        node: NodeId<'a>,
        value: Value<'a>,
    ) -> Result<Self, Option<String>>;
    
    fn default(
        context: &Context<'a>,
        node: NodeId<'a>,
    ) -> Self;
}

pub trait CoercedArgs<'a> {
    fn from(ctx: &mut impl ExpressionEvaluatorContext<'a>, fb: NodeId<'a>) -> Self ;
}

fn wrong_number_args<'a>(
    context: &Context<'a>,
    node: NodeId<'a>,
    args: Vec<Node<'a, Value<'a>>>,
    expected: &[&str],
) {
    context.report_error(
        node,
        format!(
            "Wrong number of arguments, expected [{}] got [{}]",
            expected.iter().delim(", "),
            args.iter().map(|i| i.0.get_type()).delim(", "),
        ),
    );
}

fn coerce_argument<'a, T: CoercedArg<'a>>(
    context: &Context<'a>,
    Node(arg, node): Node<'a, Value<'a>>,
) -> T {
    T::from(context, node, arg)
        .inspect_err(|err| match err {
            None => context.report_error(
                node,
                format!(
                    "Incorrect argument, expected [{}] got [{}]",
                    T::TYPE_REPR,
                    arg.get_type()
                ),
            ),
            Some(msg) => context.report_error(node, msg),
        })
        .unwrap_or_else(|_| T::default(context, node))
}

impl<'a, A: CoercedArg<'a>> CoercedArgs<'a> for A {
    fn from(ctx: &mut impl ExpressionEvaluatorContext<'a>, fb: NodeId<'a>) -> Self {
        let Node(args, node) = ctx.args(fb, ArgumentsTypeHint::Individual(&[A::HINT]));
        if args.len() != 1 {
            wrong_number_args(ctx.context(), node, args, &[A::TYPE_REPR]);
            A::default(ctx.context(), fb)
        } else {
            coerce_argument(ctx.context(), args[0])
        }
    }
}

impl<'a, A: CoercedArg<'a>> CoercedArgs<'a> for (A,) {
    fn from(ctx: &mut impl ExpressionEvaluatorContext<'a>, fb: NodeId<'a>) -> Self {
        let Node(args, node) = ctx.args(fb, ArgumentsTypeHint::Individual(&[A::HINT]));
        if args.len() != 1 {
            wrong_number_args(ctx.context(), node, args, &[A::TYPE_REPR]);
            (A::default(ctx.context(), fb),)
        } else {
            (coerce_argument(ctx.context(), args[0]),)
        }
    }
}

impl<'a, A: CoercedArg<'a>, B: CoercedArg<'a>> CoercedArgs<'a> for (A, B) {
    fn from(ctx: &mut impl ExpressionEvaluatorContext<'a>, fb: NodeId<'a>) -> Self {
        let Node(args, node) = ctx.args(fb, ArgumentsTypeHint::Individual(&[A::HINT, B::HINT]));
        if args.len() != 2 {
            wrong_number_args(ctx.context(), node, args, &[A::TYPE_REPR, B::TYPE_REPR]);
            (A::default(ctx.context(), fb), B::default(ctx.context(), fb))
        } else {
            (
                coerce_argument(ctx.context(), args[0]),
                coerce_argument(ctx.context(), args[1]),
            )
        }
    }
}

impl<'a, A: CoercedArg<'a>, B: CoercedArg<'a>, C: CoercedArg<'a>> CoercedArgs<'a> for (A, B, C) {
    fn from(ctx: &mut impl ExpressionEvaluatorContext<'a>, fb: NodeId<'a>) -> Self {
        let Node(args, node) = ctx.args(
            fb,
            ArgumentsTypeHint::Individual(&[A::HINT, B::HINT, C::HINT]),
        );
        if args.len() != 2 {
            wrong_number_args(
                ctx.context(),
                node,
                args,
                &[A::TYPE_REPR, B::TYPE_REPR, C::TYPE_REPR],
            );
            (A::default(ctx.context(), fb), B::default(ctx.context(), fb), C::default(ctx.context(), fb))
        } else {
            (
                coerce_argument(ctx.context(), args[0]),
                coerce_argument(ctx.context(), args[1]),
                coerce_argument(ctx.context(), args[2]),
            )
        }
    }
}
