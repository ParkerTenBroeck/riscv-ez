use std::os::unix::ffi::OsStrExt;

use crate::{
    assembler::{lang, LangCtx},
    context::{Node, NodeId},
    expression::{
        args::{AsmStrArg, StrArg, U32Arg, U32Pow2Arg}, binop::BinOp, unop::UnOp, ArgumentsTypeHint, AsmStr, AssemblyLabel, AssemblyRegister, Constant, CustomValue, ExprCtx, FuncParamParser, Indexed, NodeVal, Value, ValueType
    },
    lex::Number,
    logs::LogEntry,
    util::IntoStrDelimable,
};

pub mod trans;

pub trait SimpleAssemblyLanguage<'a>: Sized + 'a {
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
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self>;

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

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeId<'a>,
    );
    #[allow(unused)]
    fn encounter_label(&mut self, ctx: &mut LangCtx<'a, '_, Self>, label: &'a str, n: NodeId<'a>) {}
    fn add_value_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        value: Value<'a, Self>,
        n: NodeId<'a>,
    );
    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult;
    fn state_mut(&mut self) -> &mut SALState<'a>;
    fn state(&self) -> &SALState<'a>;
}

pub trait SimpleAssemblyLanguageBase<'a>: SimpleAssemblyLanguage<'a> {
    fn add_constant_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        constant: Constant<'a>,
        node: NodeId<'a>,
    ) {
        let align = constant.get_align();
        macro_rules! dat {
            ($expr:expr) => {
                self.add_data(ctx, $expr, align as usize, node)
            };
        }
        match self.state_mut().endianess {
            Endianess::Little => match constant {
                Constant::I8(v) => dat!(&v.to_le_bytes()),
                Constant::I16(v) => dat!(&v.to_le_bytes()),
                Constant::I32(v) => dat!(&v.to_le_bytes()),
                Constant::I64(v) => dat!(&v.to_le_bytes()),
                Constant::U8(v) => dat!(&v.to_le_bytes()),
                Constant::U16(v) => dat!(&v.to_le_bytes()),
                Constant::U32(v) => dat!(&v.to_le_bytes()),
                Constant::U64(v) => dat!(&v.to_le_bytes()),
                Constant::F32(v) => dat!(&v.to_le_bytes()),
                Constant::F64(v) => dat!(&v.to_le_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align as usize, node);
                        self.add_data(ctx, &[0], 1, node)
                    }
                },
                Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
            },
            Endianess::Big => match constant {
                Constant::I8(v) => dat!(&v.to_be_bytes()),
                Constant::I16(v) => dat!(&v.to_be_bytes()),
                Constant::I32(v) => dat!(&v.to_be_bytes()),
                Constant::I64(v) => dat!(&v.to_be_bytes()),
                Constant::U8(v) => dat!(&v.to_be_bytes()),
                Constant::U16(v) => dat!(&v.to_be_bytes()),
                Constant::U32(v) => dat!(&v.to_be_bytes()),
                Constant::U64(v) => dat!(&v.to_be_bytes()),
                Constant::F32(v) => dat!(&v.to_be_bytes()),
                Constant::F64(v) => dat!(&v.to_be_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align as usize, node);
                        self.add_data(ctx, &[0], 1, node)
                    }
                },
                Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
            },
        }
    }

    fn add_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        data: &[u8],
        align: usize,
        node: NodeId<'a>,
    ) {
        if self.state().current_section.is_none() {
            ctx.context.report(LogEntry::new()
            .error(node, "section not specified, defaulting to .text")
            .hint_locless("specify section with .text .data .rodata .bss or .section \"<name>\" directives")
        );
        }
    }

    fn add_space_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        space: usize,
        align: usize,
        node: NodeId<'a>,
    ) {
        if self.state().current_section.is_none() {
            ctx.context.report(LogEntry::new()
                .error(node, "section not specified, defaulting to .text")
                .hint_locless("specify section with .text .data .rodata .bss or .section \"<name>\" directives")
            );
            self.state_mut().current_section = Some(".text");
        }
    }

    fn set_section(&mut self, ctx: &mut LangCtx<'a, '_, Self>, section: &'a str, node: NodeId<'a>) {
        self.state_mut().current_section = Some(section);
    }
}

impl<'a, T: SimpleAssemblyLanguage<'a>> SimpleAssemblyLanguageBase<'a> for T {}

impl<'a, T: SimpleAssemblyLanguage<'a>> lang::AssemblyLanguage<'a> for T {
    type Reg = T::Reg;
    type Indexed = T::Indexed;
    type CustomValue = T::CustomValue;
    type Label = T::Label;
    type AssembledResult = T::AssembledResult;

    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self> {
        match ident.0 {
            "__line__" => Value::Constant(Constant::U32(ident.1.top().span.line.wrapping_add(1))),
            "__col__" => Value::Constant(Constant::U32(ident.1.top().span.col)),
            "__len__" => Value::Constant(Constant::U32(ident.1.top().span.len)),
            "__offset__" => Value::Constant(Constant::U32(ident.1.top().span.offset)),
            "__file__" => Value::Constant(Constant::Str(
                if let Some(str) = ident.1.top().source.path.as_os_str().to_str() {
                    AsmStr::Str(str)
                } else {
                    AsmStr::ByteStr(ident.1.top().source.path.as_os_str().as_bytes())
                },
            )),
            _ => self.parse_ident(ctx, ident, hint),
        }
    }

    fn parse_numeric_literal(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        num: Node<'a, Number<'a>>,
        negated: bool,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.parse_numeric_literal(ctx, num, negated, hint)
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_func(ctx, func, hint)
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
        self.eval_binop(ctx, node, lhs, op, rhs, hint)
    }

    fn eval_unnop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        op: Node<'a, UnOp>,
        expr: NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_unnop(ctx, node, op, expr, hint)
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
        self.eval_index(ctx, node, lhs, opening, rhs, closing, hint)
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
        self.eval_cast(ctx, node, expr, as_node, ty, hint)
    }

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeId<'a>,
    ) {
        macro_rules! constant {
            ($argument:ident, $kind:ident) => {
                for Node(crate::expression::args::$argument::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::$kind(arg.unwrap_or_default()), n);
                }
            };
        }
        match mnemonic {
            ".dbg" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context.report_info(args_node, format!("{args:#?}"))
            }
            ".info" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
                ctx.context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }

            ".section" => {
                if let StrArg::Val(Some(sec)) = ctx.eval(self).coerced(n).0 {
                    self.set_section(ctx, sec, n);
                }
            }
            ".align" => if let U32Pow2Arg::Val(Some(align)) = ctx.eval(self).coerced(n).0 {},

            ".label" => {
                if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {
                    self.encounter_label(ctx, label, n);
                }
            }
            ".global" => if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {},
            ".weak" => if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {},
            ".local" => if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {},
            ".type" => if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {},
            ".size" => if let Node(StrArg::Val(Some(label)), n) = ctx.eval(self).coerced(n) {},
            ".text" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".test", node);
            }
            ".bss" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".bss", node);
            }
            ".data" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".data", node);
            }
            ".rodata" => {
                let Node((), node) = ctx.eval(self).coerced(n);
                self.set_section(ctx, ".rodata", node);
            }

            ".space" => {
                if let U32Arg::Val(Some(size)) = ctx.eval(self).coerced(n).0 {
                    self.add_space_data(ctx, size as usize, 1, n);
                }
            }
            ".values" => {
                for arg in ctx.eval(self).args(n, ArgumentsTypeHint::None).0 {
                    self.add_value_data(ctx, arg.0, arg.1);
                }
            }
            ".stringz" => {
                for Node(crate::expression::args::AsmStrArg::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::Str(arg.unwrap_or_default()), n);
                    if !matches!(arg, Some(AsmStr::CStr(_))) {
                        self.add_constant_data(ctx, Constant::U8(0), n);
                    }
                }
            }
            ".string" => {
                for Node(crate::expression::args::AsmStrArg::Val(arg), n) in
                    ctx.eval(self).coerced::<Vec<_>>(n).0
                {
                    self.add_constant_data(ctx, Constant::Str(arg.unwrap_or_default()), n);
                }
            }
            ".u8" => constant!(U8Arg, U8),
            ".u16" => constant!(U16Arg, U16),
            ".u32" => constant!(U32Arg, U32),
            ".u64" => constant!(U64Arg, U64),
            ".i8" => constant!(I8Arg, I8),
            ".i16" => constant!(I16Arg, I16),
            ".i32" => constant!(I32Arg, I32),
            ".i64" => constant!(I64Arg, I64),
            ".f32" => constant!(F32Arg, F32),
            ".f64" => constant!(F64Arg, F64),
            ".bool" => constant!(BoolArg, Bool),
            ".char" => constant!(CharArg, Char),

            _ => self.assemble_mnemonic(ctx, mnemonic, n),
        }
    }

    fn encounter_label(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        mut label: &'a str,
        n: NodeId<'a>,
    ) {
        if label.starts_with('.') {
            if let Some(prev) = self.state_mut().last_non_local_label {
                label = ctx.context.alloc_str(format!("{prev}{label}"))
            } else {
                ctx.context.report(LogEntry::new()
                    .error(n, "encountered local label before non local label")
                    .hint_locless("local labels start with '.' consider adding a non local label before the definition of this one")
            );
            }
        } else {
            self.state_mut().last_non_local_label = Some(label);
        }
        self.encounter_label(ctx, label, n);
    }

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        self.finish(ctx)
    }

    fn encounter_comment(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        comment: &'a str,
        n: NodeId<'a>,
    ) {
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub enum Endianess {
    #[default]
    Little,
    Big,
}

#[derive(Clone, Debug, Default)]
pub struct SALState<'a> {
    pub current_section: Option<&'a str>,
    pub last_non_local_label: Option<&'a str>,
    pub endianess: Endianess,
}
