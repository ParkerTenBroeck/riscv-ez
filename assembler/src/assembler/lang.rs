use crate::{
    assembler::{Assembler, LangCtx},
    context::{Node, NodeId},
    expression::{
        AssemblyLabel, AssemblyRegister, Constant, CustomValue, ExprCtx, FuncParamParser, Indexed,
        NodeVal, Value, ValueType, binop::BinOp, unop::UnOp,
    },
    lex::Number,
};

pub trait AssemblyLanguage<'a>: Sized + 'a {
    type Reg: AssemblyRegister<'a, Lang = Self>;
    type Indexed: Indexed<'a, Lang = Self>;
    type CustomValue: CustomValue<'a, Lang = Self>;
    type Label: AssemblyLabel<'a, Lang = Self>;
    type AssembledResult;
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
        lhs: NodeVal<'a, Self>,
        opening: NodeId<'a>,
        rhs: NodeVal<'a, Self>,
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

    fn add_label(asm: &mut Assembler<'a, '_, Self>, ident: &'a str, node: NodeId<'a>);
    fn set_section(asm: &mut Assembler<'a, '_, Self>, section: &'a str, node: NodeId<'a>);

    fn add_empty_space_data(
        asm: &mut Assembler<'a, '_, Self>,
        size: usize,
        align: usize,
        node: NodeId<'a>,
    );
    fn add_bytes_as_data(
        asm: &mut Assembler<'a, '_, Self>,
        data: &[u8],
        align: usize,
        node: NodeId<'a>,
    );
    fn add_value_as_data(asm: &mut Assembler<'a, '_, Self>, value: NodeVal<'a, Self>);
    fn add_constant_as_data(asm: &mut Assembler<'a, '_, Self>, value: Node<'a, Constant<'a>>);

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeId<'a>,
    );

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult;
}
