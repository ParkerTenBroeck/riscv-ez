use std::convert::Infallible;
use std::marker::PhantomData;

use crate::assembler::lang::AssemblyLanguage;
use crate::context::{Context, Node, NodeId};
use crate::expression::{
    ArgumentsTypeHint, Constant, ExpressionEvaluator, ImplicitCastTo, NodeVal, Value, ValueType,
};
use crate::lex::Token;
use crate::util::IntoStrDelimable;

macro_rules! integer {
    ($($ty:ty, $kind:expr, $vt:ident, $opt:ident, $($pow:ident)?),* $(,)?) => {$(
        #[non_exhaustive]
        pub enum $opt<L>{
            Val(Option<$ty>),
            __(Infallible, PhantomData<L>)
        }
        impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $opt<L> {
            type LANG = L;
            const TYPE_REPR: &'static str = $kind;
            const HINT: ValueType<'a, L> = ValueType::$vt;

            fn from_arg(
                context: &mut Context<'a>,
                node: NodeId<'a>,
                value: Value<'a, L>,
            ) -> Result<Self, Option<String>> {
                match value {
                    Value::Constant(c) if c.is_integer() => Ok($opt::Val(c.cast(node, context))),
                    _ => Err(None),
                }
            }

            fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
                Self::Val(None)
            }
        }

        #[non_exhaustive]
        pub enum $vt<L>{
            Val($ty),
            __(Infallible, PhantomData<L>)
        }
        impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $vt<L> {
            type LANG = L;
            const TYPE_REPR: &'static str = $kind;
            const HINT: ValueType<'a, L> = ValueType::$vt;

            fn from_arg(
                context: &mut Context<'a>,
                node: NodeId<'a>,
                value: Value<'a, L>,
            ) -> Result<Self, Option<String>> {
                match value {
                    Value::Constant(c) if c.is_integer() => Ok(Self::Val(c.cast(node, context).unwrap_or_default())),
                    _ => Err(None),
                }
            }

            fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
                Self::Val(<$ty>::default())
            }
        }

        $(
            #[non_exhaustive]
            pub enum $pow<L>{
                Val(Option<$ty>),
                __(Infallible, PhantomData<L>)
            }
            impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $pow<L> {
                type LANG = L;
                const TYPE_REPR: &'static str = $kind;
                const HINT: ValueType<'a, L> = ValueType::$vt;

                fn from_arg(
                    context: &mut Context<'a>,
                    node: NodeId<'a>,
                    value: Value<'a, L>,
                ) -> Result<Self, Option<String>> {
                    match value {
                        Value::Constant(c) => {
                            let value: $ty = c.cast(node, context).ok_or(None)?;
                            Ok(Self::Val(if value.is_power_of_two() {
                                    Some(value)
                                } else {
                                    context.report_error(node, "value is not a power of two");
                                    None
                                }
                            ))
                        }
                        _ => Err(None),
                    }
                }

                fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
                    Self::Val(None)
                }
            }
        )?

    )*};
}

macro_rules! value {
    ($($ty:ty, $kind:expr, $vt:ident, $opt:ident),* $(,)?) => {$(
        #[non_exhaustive]
        pub enum $opt<L>{
            Val(Option<$ty>),
            __(Infallible, PhantomData<L>)
        }
        impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $opt<L> {
            type LANG = L;
            const TYPE_REPR: &'static str = $kind;
            const HINT: ValueType<'a, L> = ValueType::String;

            fn from_arg(
                _: &mut Context<'a>,
                _: NodeId<'a>,
                value: Value<'a, L>,
            ) -> Result<Self, Option<String>> {
                match value {
                    Value::Constant(Constant::$vt(v)) => Ok(Self::Val(Some(v))),
                    _ => Err(None),
                }
            }

            fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
               Self::Val(None)
            }
        }

        #[non_exhaustive]
        pub enum $vt<L>{
            Val($ty),
            __(Infallible, PhantomData<L>)
        }
        impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for $vt<L> {
            type LANG = L;
            const TYPE_REPR: &'static str = $kind;
            const HINT: ValueType<'a, L> = ValueType::String;

            fn from_arg(
                _: &mut Context<'a>,
                _: NodeId<'a>,
                value: Value<'a, L>,
            ) -> Result<Self, Option<String>> {
                match value {
                    Value::Constant(Constant::$vt(v)) => Ok(Self::Val(v)),
                    _ => Err(None),
                }
            }

            fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
                Self::Val(<$ty>::default())
            }
        }
    )*};
}

integer!(
    i8, "<integer>", I8, I8Opt, ,
    i16, "<integer>", I16, I16Opt, ,
    i32, "<integer>", I32, I32Opt, ,
    i64, "<integer>", I64, I64Opt, ,

    u8, "<integer>", U8, U8Opt, U8Pow2Opt,
    u16, "<integer>", U16, U16Opt, U16Pow2Opt,
    u32, "<integer>", U32, U32Opt, U32Pow2Opt,
    u64, "<integer>", U64, U64Opt, U64Pow2Opt,


    f32, "<float>", F32, F32Opt, ,
    f64, "<float>", F64, F64Opt, ,
);

value!(char, "char", Char, CharOpt, bool, "bool", Bool, BoolOpt,);

#[non_exhaustive]
pub enum StrOpt<'a, L> {
    Val(Option<&'a str>),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for StrOpt<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::String;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::String(str)) => Ok(Self::Val(Some(str))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self::Val(None)
    }
}

pub struct Reg<'a, L: AssemblyLanguage<'a>>(pub L::Reg);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Reg<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, L> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(reg) => Ok(Self(reg)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(L::Reg::default())
    }
}

pub struct RegOpt<'a, L: AssemblyLanguage<'a>>(pub Option<L::Reg>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for RegOpt<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "register";
    const HINT: ValueType<'a, L> = ValueType::Register;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Register(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(None)
    }
}

pub struct Label<'a, L: AssemblyLanguage<'a>>(pub L::Label);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Label<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "label";
    const HINT: ValueType<'a, L> = ValueType::Label;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Label(reg) => Ok(Self(reg)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(L::Label::default())
    }
}

pub struct LabelOpt<'a, L: AssemblyLanguage<'a>>(pub Option<L::Label>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for LabelOpt<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "label";
    const HINT: ValueType<'a, L> = ValueType::Label;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Label(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(None)
    }
}

pub struct Indexed<'a, L: AssemblyLanguage<'a>>(pub L::Indexed);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Indexed<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "indexed";
    const HINT: ValueType<'a, L> = ValueType::Indexed;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Indexed(reg) => Ok(Self(reg)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(L::Indexed::default())
    }
}

pub struct IndexedOpt<'a, L: AssemblyLanguage<'a>>(pub Option<L::Indexed>);
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for IndexedOpt<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "indexed";
    const HINT: ValueType<'a, L> = ValueType::Indexed;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Indexed(reg) => Ok(Self(Some(reg))),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self(None)
    }
}

#[non_exhaustive]
pub enum Str<'a, L> {
    Val(&'a str),
    __(Infallible, PhantomData<L>),
}
impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Str<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "str";
    const HINT: ValueType<'a, L> = ValueType::String;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, L>,
    ) -> Result<Self, Option<String>> {
        match value {
            Value::Constant(Constant::String(str)) => Ok(Self::Val(str)),
            _ => Err(None),
        }
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self::Val("")
    }
}

impl<'a, L: AssemblyLanguage<'a>> CoercedArg<'a> for Value<'a, L> {
    type LANG = L;
    const TYPE_REPR: &'static str = "any";
    const HINT: ValueType<'a, L> = ValueType::Any;

    fn from_arg(
        _: &mut Context<'a>,
        _: NodeId<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        Ok(value)
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        Self::Constant(Constant::I32(0))
    }
}

impl<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, LANG = L>> CoercedArg<'a> for Node<'a, T> {
    type LANG = L;
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

pub trait CoercedArg<'a>: Sized {
    type LANG: AssemblyLanguage<'a>;
    const TYPE_REPR: &'static str;
    const HINT: ValueType<'a, Self::LANG>;
    const OPTIONAL: bool = false;
    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>>;

    fn default(context: &mut Context<'a>, node: NodeId<'a>) -> Self;
}

impl<'a, T: CoercedArg<'a>> CoercedArg<'a> for Option<T> {
    type LANG = T::LANG;
    const TYPE_REPR: &'static str = T::TYPE_REPR;
    const HINT: ValueType<'a, Self::LANG> = T::HINT;
    const OPTIONAL: bool = true;

    fn from_arg(
        context: &mut Context<'a>,
        node: NodeId<'a>,
        value: Value<'a, Self::LANG>,
    ) -> Result<Self, Option<String>> {
        T::from_arg(context, node, value).map(Some)
    }

    fn default(_: &mut Context<'a>, _: NodeId<'a>) -> Self {
        None
    }
}

pub trait CoercedArgs<'a, L: AssemblyLanguage<'a>> {
    fn from_args(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        args: Node<'a, Vec<NodeVal<'a, L>>>,
    ) -> Node<'a, Self>
    where
        Self: Sized;

    fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, L>) -> R) -> R;

    fn args(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        fb: NodeId<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args(fb, args))
    }

    fn args_delim(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        init: NodeId<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        Self::with_hint(|args| ctx.args_delim(init, start, end, args))
    }

    fn coerced_args(ctx: &mut ExpressionEvaluator<'a, '_, L>, init: NodeId<'a>) -> Node<'a, Self>
    where
        Self: Sized,
    {
        let args = Self::args(ctx, init);
        Self::from_args(ctx, args)
    }

    fn coerced_args_delim(
        ctx: &mut ExpressionEvaluator<'a, '_, L>,
        init: NodeId<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, Self>
    where
        Self: Sized,
    {
        let args = Self::args_delim(ctx, init, start, end);
        Self::from_args(ctx, args)
    }
}

fn wrong_number_args<'a>(
    context: &mut Context<'a>,
    node: NodeId<'a>,
    args: Vec<Node<'a, Value<'a, impl AssemblyLanguage<'a>>>>,
    expected: &[&str],
    vargs: bool,
) {
    let vargs = if vargs { "*" } else { "" };
    context.report_error(
        node,
        format!(
            "wrong number of arguments, expected [{}{vargs}] got [{}]",
            expected.iter().delim(", "),
            args.iter().map(|i| i.0.get_type()).delim(", "),
        ),
    );
}

fn coerce_argument<'a, L: AssemblyLanguage<'a>, T: CoercedArg<'a, LANG = L>>(
    context: &mut Context<'a>,
    a: Option<Node<'a, Value<'a, L>>>,
    backup: NodeId<'a>,
) -> T {
    if let Some(Node(arg, node)) = a {
        T::from_arg(context, node, arg)
            .inspect_err(|err| match err {
                None => context.report_error(
                    node,
                    format!(
                        "incorrect argument, expected [{}{}] got [{}]",
                        T::TYPE_REPR,
                        if T::OPTIONAL { "?" } else { "" },
                        arg.get_type()
                    ),
                ),
                Some(msg) => context.report_error(node, msg),
            })
            .unwrap_or_else(|_| T::default(context, node))
    } else {
        T::default(context, backup)
    }
}

macro_rules! nya {
    (!expand,) => {
    };
    (!expand, $v:ident, $($t:ident,)*) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    ($($t:ident),*$(,)?) => {
        nya!(!expand, $($t,)*);
        nya!(!impl, $($t,)*);
    };
    (!impl, $($t:ident),*$(,)?) => {

        #[allow(unused_parens)]
        impl<'a, $($t: CoercedArg<'a, LANG=AL>,)* AL: AssemblyLanguage<'a>> CoercedArgs<'a, AL> for ($($t),*) {
            fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, AL>) -> R) -> R{
                func(ArgumentsTypeHint::Individual(&[$($t::HINT,)*]))
            }

            fn from_args(
                ctx: &mut ExpressionEvaluator<'a, '_, AL>,
                Node(args, node): Node<'a, Vec<NodeVal<'a, AL>>>,
            ) -> Node<'a, Self> {
                nya!(oargs: $($t,)*);
                #[allow(unused_comparisons)]
                if args.len() < nya!(count_min: $($t,)*) || args.len() > nya!(count: $($t,)*) {
                    wrong_number_args(ctx.context, node, args, &[$(&format!("{}{}", $t::TYPE_REPR, if $t::OPTIONAL {"?"} else {""}),)*], false);
                    Node(($($t::default(ctx.context, node)),*), node)
                }else{
                    #[allow(unused)]
                    let mut iter = args.into_iter();
                    nya!(dargs: ctx, node, iter, $($t,)*);
                    Node(($($t),*), node)
                }
            }
        }

        #[allow(unused_parens)]
        impl<'a, $($t: CoercedArg<'a, LANG=AL>,)* VV: CoercedArg<'a, LANG=AL>, AL: AssemblyLanguage<'a>> CoercedArgs<'a, AL> for ($($t,)* Vec<VV>) {
            fn with_hint<R>(func: impl FnOnce(ArgumentsTypeHint<'a, '_, AL>) -> R) -> R{
                func(ArgumentsTypeHint::Comb(&[$($t::HINT,)*], VV::HINT))
            }

            fn from_args(
                ctx: &mut ExpressionEvaluator<'a, '_, AL>,
                Node(args, node): Node<'a, Vec<NodeVal<'a, AL>>>,
            ) -> Node<'a, Self> {
                nya!(oargs: $($t,)*);
                #[allow(unused_comparisons)]
                if args.len() < nya!(count_min: $($t,)*) {
                    wrong_number_args(ctx.context, node, args, &[$(&format!("{}{}", $t::TYPE_REPR, if $t::OPTIONAL {"?"} else {""}),)* VV::TYPE_REPR], true);
                    Node(($($t::default(ctx.context, node),)* Vec::new()), node)
                }else{
                    #[allow(unused_mut)]
                    let mut iter = args.into_iter();
                    nya!(dargs: ctx, node, iter, $($t,)*);
                    let vv = iter.map(|a|{coerce_argument(ctx.context, Some(a), node)}).collect();
                    Node(($($t,)* vv), node)
                }
            }
        }
    };
    (oargs: $o:ident, $t:ident, $($r:ident,)*) => {
        const{
            if $o::OPTIONAL{
                assert!($t::OPTIONAL, "Non optional parameter cannot follow an optional parameter")
            }
        }
        nya!(oargs: $t, $($r,)*)
    };
    (oargs: $o:ident, ) => {
    };
    (oargs: ) => {
    };
    (dargs: $ctx:expr, $node:expr, $iter:expr, ) => {
    };
    (dargs: $ctx:expr, $node:expr, $iter:expr, $v:ident, $($t:ident,)*) => {
        #[allow(non_snake_case)]
        let $v = coerce_argument($ctx.context, $iter.next(), $node);
        nya!(dargs: $ctx, $node, $iter, $($t,)*);
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

    (count_min:) => {
        0
    };
    (count_min: $v:ident,) => {
        ($v::OPTIONAL as usize)
    };
    (count_min: $v:ident, $($t:ident),*$(,)?) => {
        ($v::OPTIONAL as usize)+nya!(count_min: $($t,)*)
    };
}
nya!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
