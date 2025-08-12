use num_traits::*;

use crate::{
    assembler::LangCtx,
    context::{Node, NodeId},
    expression::{
        AssemblyLabel, AssemblyRegister, CustomValue, ExprCtx, FuncParamParser, Indexed, NodeVal,
        Value, ValueType, binop::BinOp, unop::UnOp,
    },
    lex::Number,
};

pub trait FromAsPrimitive<F> {
    fn from_as(v: F) -> Self;
}

impl<F: 'static + Copy, T: 'static + Copy> FromAsPrimitive<F> for T
where
    F: AsPrimitive<T>,
{
    fn from_as(v: F) -> T {
        v.as_()
    }
}

pub trait AsmNum:
    PrimInt
    + FromPrimitive
    + Default
    + NumCast
    + ToPrimitive
    + ToBytes
    + WrappingAdd
    + WrappingSub
    + WrappingNeg
    + WrappingMul
    + WrappingShl
    + WrappingShr
    + std::fmt::Display
    + std::fmt::Debug
    + Num<FromStrRadixErr = std::num::ParseIntError>
    + AsPrimitive<u8>
    + AsPrimitive<u16>
    + AsPrimitive<u32>
    + AsPrimitive<u64>
    + AsPrimitive<u128>
    + AsPrimitive<i8>
    + AsPrimitive<i16>
    + AsPrimitive<i32>
    + AsPrimitive<i64>
    + AsPrimitive<i128>
    + AsPrimitive<f32>
    + AsPrimitive<f64>
    + FromAsPrimitive<u8>
    + FromAsPrimitive<u16>
    + FromAsPrimitive<u32>
    + FromAsPrimitive<u64>
    + FromAsPrimitive<u128>
    + FromAsPrimitive<i8>
    + FromAsPrimitive<i16>
    + FromAsPrimitive<i32>
    + FromAsPrimitive<i64>
    + FromAsPrimitive<i128>
    + FromAsPrimitive<f32>
    + FromAsPrimitive<f64>
    + FromAsPrimitive<bool>
    + FromAsPrimitive<char>
{
    const POSTFIX: &str;
}

macro_rules! prims {
    ($($ident:ident),* $(,)?) => {$(
        impl AsmNum for $ident{
            const POSTFIX: &str = stringify!($ident);
        }
    )*};
}

prims!(i8, i16, i32, i64, i128, u8, u16, u32, u64, u128);

pub trait AssemblyLanguage<'a>: Sized {
    type Reg: AssemblyRegister<'a, Lang = Self>;
    type Indexed: Indexed<'a, Lang = Self>;
    type CustomValue: CustomValue<'a, Lang = Self>;
    type Label: AssemblyLabel<'a, Lang = Self>;
    type AssembledResult;

    type Usize: AsmNum;
    type Isize: AsmNum;
    type Uptr: AsmNum;
    type Iptr: AsmNum;
    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self>;

    fn parse_numeric_literal(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self)
            .parse_numeric_literal_base(num, negated, hint)
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).func_base(func, hint)
    }

    fn eval_binop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        lhs: NodeVal<'a, Self>,
        op: Node<'a, BinOp>,
        rhs: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).binop_base(node, lhs, op, rhs, hint)
    }

    fn eval_unnop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        op: Node<'a, UnOp>,
        expr: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).unop_base(node, op, expr, hint)
    }

    #[allow(clippy::too_many_arguments)]
    fn eval_index(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        lhs: Option<NodeVal<'a, Self>>,
        opening: NodeId<'a>,
        rhs: Option<NodeVal<'a, Self>>,
        closing: NodeId<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self)
            .index_base(node, lhs, opening, rhs, closing, hint)
    }

    fn eval_cast(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        expr: NodeVal<'a, Self>,
        as_node: NodeId<'a>,
        ty: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval(self).cast_base(node, expr, as_node, ty, hint)
    }

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeId<'a>,
    );

    fn encounter_label(&mut self, ctx: &mut LangCtx<'a, '_, Self>, label: &'a str, n: NodeId<'a>);
    fn encounter_comment(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        comment: &'a str,
        n: NodeId<'a>,
    );

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult;
}
