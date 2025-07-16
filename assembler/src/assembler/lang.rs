use crate::{
    assembler::Assembler,
    context::{Node, NodeId},
    expression::{
        AssemblyLabel, AssemblyRegister, CustomValue, ExpressionEvaluatorContext, FuncParamParser,
        Indexed, NodeVal, Value, ValueType, binop::BinOp, unop::UnOp,
    },
    lex::Number,
};

pub trait AssemblyLanguage<'a>: Sized + Clone + 'a {
    type Reg: AssemblyRegister<'a, Self>;
    type Indexed: Indexed<'a, Self>;
    type CustomValue: CustomValue<'a, Self>;
    type Label: AssemblyLabel<'a, Self>;
    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        ident: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self>;

    fn parse_numeric_literal(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval().parse_numeric_literal_base(num, negated, hint)
    }

    fn eval_func(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval().func_base(func, hint)
    }

    fn eval_binop(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        lhs: NodeVal<'a, Self>,
        op: Node<'a, BinOp>,
        rhs: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval().binop_base(node, lhs, op, rhs, hint)
    }

    fn eval_unnop(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        op: Node<'a, UnOp>,
        expr: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval().unop_base(node, op, expr, hint)
    }

    fn eval_index(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        lhs: NodeVal<'a, Self>,
        opening: NodeId<'a>,
        rhs: NodeVal<'a, Self>,
        closing: NodeId<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval()
            .index_base(node, lhs, opening, rhs, closing, hint)
    }

    fn eval_cast(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        expr: NodeVal<'a, Self>,
        as_node: NodeId<'a>,
        ty: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        ctx.eval().cast_base(node, expr, as_node, ty, hint)
    }

    fn add_label_as_data(asm: &mut Assembler<'a, '_, Self>, ident: Self::Label, node: NodeId<'a>);

    fn assemble_mnemonic(asm: &mut Assembler<'a, '_, Self>, mnemonic: &'a str, n: NodeId<'a>);
}
