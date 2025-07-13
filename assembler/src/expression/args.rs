use crate::assembler::lang::AssemblyLanguage;
use crate::context::{Context, Node, NodeId};
use crate::expression::{
    ArgumentsTypeHint, Constant, ExpressionEvaluatorContext, ImplicitCastTo, NodeVal, Value,
    ValueType,
};
use crate::lex::Token;
use crate::util::IntoStrDelimable;

#[derive(Default)]
pub struct U32Opt(pub Option<u32>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a, L> for U32Opt {
    const TYPE_REPR: &'static str = "<integer>";
    const HINT: ValueType<'a, L> = ValueType::U32;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(c) if c.is_integer() => Ok(U32Opt(c.cast(node, context))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct U32Power2Opt(pub Option<u32>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a, L> for U32Power2Opt {
    const TYPE_REPR: &'static str = "<integer>";
    const HINT: ValueType<'a, L> = ValueType::U32;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        fn chk_pow<'a>(context: &mut Context<'a>, value: u32, node: NodeId<'a>) -> Option<u32> {
            if value.is_power_of_two() {
                Some(value)
            } else {
                context.report_error(node, "Value is not a power of two");
                None
            }
        }
        match value {
            Value::Constant(c) => {
                let value = c.cast(node, context).ok_or(None)?;
                Ok(U32Power2Opt(chk_pow(context, value, node)))
            }
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct StrOpt<'a>(pub Option<&'a str>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a, L> for StrOpt<'a> {
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::String;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::String(str)) => Ok(StrOpt(Some(str))),
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
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a, L> for Immediate<'a, L> {
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

impl<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, L>> CoercedArg<'a, L> for Node<'a, T> {
    const TYPE_REPR: &'static str = T::TYPE_REPR;
    const HINT: ValueType<'a, L> = T::HINT;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        T::from_arg(context, node, value).map(|v| Node(v, node))
    }

    fn default(context: &mut Context<'a>, node: NodeId<'a>) -> Self {
        Node(T::default(context, node), node)
    }
}

pub trait CoercedArg<'a, L: AssemblyLanguage<'a>>: Sized {
    const TYPE_REPR: &'static str;
    const HINT: ValueType<'a, L>;
    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>>;

    fn default(context: &mut Context<'a>, node: NodeId<'a>) -> Self;
}

pub trait CoercedArgs<'a, L: AssemblyLanguage<'a>> {
    fn from_args(
        ctx: &mut impl ExpressionEvaluatorContext<'a, L>,
        args: Node<'a, Vec<NodeVal<'a, L>>>,
    ) -> Self;

    fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, L>) -> R) -> R;

    fn args(
        ctx: &mut impl ExpressionEvaluatorContext<'a, L>,
        fb: NodeId<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args(fb, args))
    }

    fn args_delim(
        ctx: &mut impl ExpressionEvaluatorContext<'a, L>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args_delim(start, end, args))
    }

    fn coerced_args(ctx: &mut impl ExpressionEvaluatorContext<'a, L>, fb: NodeId<'a>) -> Self
    where
        Self: Sized,
    {
        let args = Self::args(ctx, fb);
        Self::from_args(ctx, args)
    }

    fn coerced_args_delim(
        ctx: &mut impl ExpressionEvaluatorContext<'a, L>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Self
    where
        Self: Sized,
    {
        let args = Self::args_delim(ctx, start, end);
        Self::from_args(ctx, args)
    }
}

fn wrong_number_args<'a>(
    context: &mut Context<'a>,
    node: NodeId<'a>,
    args: Vec<Node<'a, Value<'a, impl AssemblyLanguage<'a>>>>,
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

fn coerce_argument<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, L>>(
    context: &mut Context<'a>,
    Node(arg, node): Node<'a, Value<'a, L>>,
) -> T {
    T::from_arg(context, node, arg)
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

macro_rules! nya {
    (!expand,) => {
    };
    (!expand, $v:ident, $($t:ident),*$(,)?) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    ($($t:ident),*$(,)?) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    (!impl, $($t:ident),*$(,)?) => {
        impl<'a, AL: AssemblyLanguage<'a>, $($t: CoercedArg<'a, AL>,)*> CoercedArgs<'a, AL> for ($($t,)*) {
            fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, AL>) -> R) -> R{
                func(ArgumentsTypeHint::Individual(&[$($t::HINT,)*]))
            }

            fn from_args(
                ctx: &mut impl ExpressionEvaluatorContext<'a, AL>,
                Node(args, node): Node<'a, Vec<NodeVal<'a, AL>>>,
            ) -> Self {
                if args.len() != nya!(count: $($t,)*) {
                    wrong_number_args(ctx.context(), node, args, &[$($t::TYPE_REPR,)*]);
                    ($($t::default(ctx.context(), node),)*)
                }else{
                    nya!(ctx, args, 0, $($t,)*);
                    ($($t,)*)
                }
            }
        }
    };
    ($ctx:expr, $args:expr, $count:expr, ) => {
    };
    ($ctx:expr, $args:expr, $count:expr, $v:ident, $($t:ident,)*) => {
        #[allow(non_snake_case)]
        let $v = coerce_argument($ctx.context(), $args[$count]);
        nya!($ctx, $args, $count+1, $($t,)*);
    };
    (args: $count:expr, $v:ident, $($t:ident),*) => {

    };
    (count:) => {
        0
    };
    (count: $v:ident,) => {
        1
    };
    (count: $v:ident, $($t:ident),*$(,)?) => {
        1+nya!(count: $($t,)*)
    };
}
nya!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
