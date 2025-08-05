use crate::{
    assembler::{LangCtx, lang},
    context::{Node, NodeId},
    expression::{
        ArgumentsTypeHint, AssemblyLabel, AssemblyRegister, Constant, CustomValue, ExprCtx,
        FuncParamParser, Indexed, NodeVal, Value, ValueType,
        args::{StrOpt, U32Opt},
        binop::BinOp,
        unop::UnOp,
    },
    lex::Number,
    util::IntoStrDelimable,
};

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
    fn encounter_label(&mut self, ctx: &mut LangCtx<'a, '_, Self>, label: &'a str, n: NodeId<'a>);
    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult;
    fn state(&mut self) -> &mut SALState<'a>;
}

pub trait SimpleAssemblyLanguageBase<'a>: SimpleAssemblyLanguage<'a> {
    fn add_constant_default(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        endianess: Endianess,
        constant: Node<'a, Constant<'a>>,
    ) {
        // let align = constant.0.get_align();
        // macro_rules! dat {
        //     ($expr:expr) => {
        //         self.lang.add_bytes_as_data(
        //             &mut SALCtx {
        //                 context: ctx.context,
        //                 preprocessor: ctx.preprocessor,
        //                 state: self.state,
        //             },
        //             $expr,
        //             align as usize,
        //             constant.1,
        //         )
        //     };
        // }
        // match endianess {
        //     Endianess::Little => match constant.0 {
        //         Constant::I8(v) => dat!(&v.to_le_bytes()),
        //         Constant::I16(v) => dat!(&v.to_le_bytes()),
        //         Constant::I32(v) => dat!(&v.to_le_bytes()),
        //         Constant::I64(v) => dat!(&v.to_le_bytes()),
        //         Constant::U8(v) => dat!(&v.to_le_bytes()),
        //         Constant::U16(v) => dat!(&v.to_le_bytes()),
        //         Constant::U32(v) => dat!(&v.to_le_bytes()),
        //         Constant::U64(v) => dat!(&v.to_le_bytes()),
        //         Constant::F32(v) => dat!(&v.to_le_bytes()),
        //         Constant::F64(v) => dat!(&v.to_le_bytes()),
        //         Constant::String(v) => dat!(v.as_bytes()),
        //         Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
        //         Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
        //     },
        //     Endianess::Big => match constant.0 {
        //         Constant::I8(v) => dat!(&v.to_be_bytes()),
        //         Constant::I16(v) => dat!(&v.to_be_bytes()),
        //         Constant::I32(v) => dat!(&v.to_be_bytes()),
        //         Constant::I64(v) => dat!(&v.to_be_bytes()),
        //         Constant::U8(v) => dat!(&v.to_be_bytes()),
        //         Constant::U16(v) => dat!(&v.to_be_bytes()),
        //         Constant::U32(v) => dat!(&v.to_be_bytes()),
        //         Constant::U64(v) => dat!(&v.to_be_bytes()),
        //         Constant::F32(v) => dat!(&v.to_be_bytes()),
        //         Constant::F64(v) => dat!(&v.to_be_bytes()),
        //         Constant::String(v) => dat!(v.as_bytes()),
        //         Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
        //         Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
        //     },
        // }
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
        self.parse_ident(ctx, ident, hint)
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
        // macro_rules! constant {
        //         ($kind:ident) => {
        //             for Node(crate::expression::args::$kind::Val(arg), n) in
        //                 ctx.eval(self).coerced::<Vec<_>>(n).0
        //             {
        //                 T::add_constant_as_data(self, Node(Constant::$kind(arg), n));
        //             }
        //         };
        //     }
        //     match mnemonic {
        //         ".dbg" => {
        //             let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
        //             ctx.context.report_info(args_node, format!("{args:#?}"))
        //         }
        //         ".info" => {
        //             let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
        //             ctx.context
        //                 .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
        //         }
        //         ".warning" => {
        //             let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
        //             ctx.context
        //                 .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
        //         }
        //         ".error" => {
        //             let Node(args, args_node) = ctx.eval(self).args(n, ArgumentsTypeHint::None);
        //             ctx.context
        //                 .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
        //         }

        //         ".section" => {
        //             if let StrOpt::Val(Some(sec)) = ctx.eval(self).coerced(n).0 {
        //                 self.set_section(ctx, sec, n);
        //             }
        //         }

        //         ".space" => {
        //             if let U32Opt::Val(Some(size)) = ctx.eval(self).coerced(n).0 {
        //                 self.add_empty_space_data(ctx, size as usize, 1, n);
        //             }
        //         }
        //         ".data" => {
        //             for arg in ctx.eval(self).args(n, ArgumentsTypeHint::None).0 {
        //                 self.add_value_as_data(ctx, arg);
        //             }
        //         }
        //         ".stringz" => {
        //             for Node(crate::expression::args::Str::Val(arg), n) in
        //                 ctx.eval(self).coerced::<Vec<_>>(n).0
        //             {
        //                 T::add_constant_as_data(self, Node(Constant::String(arg), n));
        //                 T::add_constant_as_data(self, Node(Constant::U8(0), n));
        //             }
        //         }
        //         ".string" => {
        //             for Node(crate::expression::args::Str::Val(arg), n) in
        //                 self.eval().coerced::<Vec<_>>(n).0
        //             {
        //                 T::add_constant_as_data(self, Node(Constant::String(arg), n));
        //             }
        //         }
        //         ".u8" => constant!(U8),
        //         ".u16" => constant!(U16),
        //         ".u32" => constant!(U32),
        //         ".u64" => constant!(U64),
        //         ".i8" => constant!(I8),
        //         ".i16" => constant!(I16),
        //         ".i32" => constant!(I32),
        //         ".i64" => constant!(I64),
        //         ".f32" => constant!(F32),
        //         ".f64" => constant!(F64),
        //         ".bool" => constant!(Bool),
        //         ".char" => constant!(Char),

        //         _ => self.assemble_mnemonic(ctx, mnemonic, n),
        //     }
    }

    fn encounter_label(&mut self, ctx: &mut LangCtx<'a, '_, Self>, label: &'a str, n: NodeId<'a>) {
        self.encounter_label(ctx, label, n);
    }

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        self.finish(ctx)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Endianess {
    Little,
    Big,
}

pub struct SALState<'a> {
    current_section: Option<&'a str>,
    last_full_label: Option<&'a str>,
    pub endianess: Endianess,
}

// impl<'a, 'b, T: SimpleAssemblyLanguage<'a>> SALImpl<'a, 'b, T> {
//     pub fn add_constant_default(
//         &mut self,
//         ctx: &mut ExprCtx<'a, '_, Self>,
//         endianess: Endianess,
//         constant: Node<'a, Constant<'a>>,
//     ) {
//         let align = constant.0.get_align();
//         macro_rules! dat {
//             ($expr:expr) => {
//                 self.lang.add_bytes_as_data(
//                     &mut SALCtx {
//                         context: ctx.context,
//                         preprocessor: ctx.preprocessor,
//                         state: self.state,
//                     },
//                     $expr,
//                     align as usize,
//                     constant.1,
//                 )
//             };
//         }
//         match endianess {
//             Endianess::Little => match constant.0 {
//                 Constant::I8(v) => dat!(&v.to_le_bytes()),
//                 Constant::I16(v) => dat!(&v.to_le_bytes()),
//                 Constant::I32(v) => dat!(&v.to_le_bytes()),
//                 Constant::I64(v) => dat!(&v.to_le_bytes()),
//                 Constant::U8(v) => dat!(&v.to_le_bytes()),
//                 Constant::U16(v) => dat!(&v.to_le_bytes()),
//                 Constant::U32(v) => dat!(&v.to_le_bytes()),
//                 Constant::U64(v) => dat!(&v.to_le_bytes()),
//                 Constant::F32(v) => dat!(&v.to_le_bytes()),
//                 Constant::F64(v) => dat!(&v.to_le_bytes()),
//                 Constant::String(v) => dat!(v.as_bytes()),
//                 Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
//                 Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
//             },
//             Endianess::Big => match constant.0 {
//                 Constant::I8(v) => dat!(&v.to_be_bytes()),
//                 Constant::I16(v) => dat!(&v.to_be_bytes()),
//                 Constant::I32(v) => dat!(&v.to_be_bytes()),
//                 Constant::I64(v) => dat!(&v.to_be_bytes()),
//                 Constant::U8(v) => dat!(&v.to_be_bytes()),
//                 Constant::U16(v) => dat!(&v.to_be_bytes()),
//                 Constant::U32(v) => dat!(&v.to_be_bytes()),
//                 Constant::U64(v) => dat!(&v.to_be_bytes()),
//                 Constant::F32(v) => dat!(&v.to_be_bytes()),
//                 Constant::F64(v) => dat!(&v.to_be_bytes()),
//                 Constant::String(v) => dat!(v.as_bytes()),
//                 Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
//                 Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
//             },
//         }
//     }

//     pub fn assemble_mnemonic_default(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
//         macro_rules! constant {
//             ($kind:ident) => {
//                 for Node(crate::expression::args::$kind::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::$kind(arg), n));
//                 }
//             };
//         }
//         match mnemonic {
//             ".dbg" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context.report_info(args_node, format!("{args:#?}"))
//             }
//             ".info" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context
//                     .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
//             }
//             ".warning" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context
//                     .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
//             }
//             ".error" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 ctx.context
//                     .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
//             }

//             ".section" => {
//                 if let StrOpt::Val(Some(sec)) = self.eval().coerced(n).0 {
//                     self.lang.set_section(self, sec, n);
//                 }
//             }

//             ".space" => {
//                 if let U32Opt::Val(Some(size)) = self.eval().coerced(n).0 {
//                     self.lang.add_empty_space_data(self, size as usize, 1, n);
//                 }
//             }
//             ".data" => {
//                 for arg in self.eval().args(n, ArgumentsTypeHint::None).0 {
//                     self.lang.add_value_as_data(self, arg);
//                 }
//             }
//             ".stringz" => {
//                 for Node(crate::expression::args::Str::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::String(arg), n));
//                     T::add_constant_as_data(self, Node(Constant::U8(0), n));
//                 }
//             }
//             ".string" => {
//                 for Node(crate::expression::args::Str::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::String(arg), n));
//                 }
//             }
//             ".u8" => constant!(U8),
//             ".u16" => constant!(U16),
//             ".u32" => constant!(U32),
//             ".u64" => constant!(U64),
//             ".i8" => constant!(I8),
//             ".i16" => constant!(I16),
//             ".i32" => constant!(I32),
//             ".i64" => constant!(I64),
//             ".f32" => constant!(F32),
//             ".f64" => constant!(F64),
//             ".bool" => constant!(Bool),
//             ".char" => constant!(Char),

//             _ => self.unknown_mnemonic(Node(mnemonic, n)),
//         }
//     }
// }

// impl<'a, T: SimpleAssemblyLanguage<'a>> T{
//         pub fn add_constant_default(&mut self, endianess: Endianess, constant: Node<'a, Constant<'a>>) {
//         let align = constant.0.get_align();
//         macro_rules! dat {
//             ($expr:expr) => {
//                 T::add_bytes_as_data(self, $expr, align as usize, constant.1)
//             };
//         }
//         match endianess {
//             Endianess::Little => match constant.0 {
//                 Constant::I8(v) => dat!(&v.to_le_bytes()),
//                 Constant::I16(v) => dat!(&v.to_le_bytes()),
//                 Constant::I32(v) => dat!(&v.to_le_bytes()),
//                 Constant::I64(v) => dat!(&v.to_le_bytes()),
//                 Constant::U8(v) => dat!(&v.to_le_bytes()),
//                 Constant::U16(v) => dat!(&v.to_le_bytes()),
//                 Constant::U32(v) => dat!(&v.to_le_bytes()),
//                 Constant::U64(v) => dat!(&v.to_le_bytes()),
//                 Constant::F32(v) => dat!(&v.to_le_bytes()),
//                 Constant::F64(v) => dat!(&v.to_le_bytes()),
//                 Constant::String(v) => dat!(v.as_bytes()),
//                 Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
//                 Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
//             },
//             Endianess::Big => match constant.0 {
//                 Constant::I8(v) => dat!(&v.to_be_bytes()),
//                 Constant::I16(v) => dat!(&v.to_be_bytes()),
//                 Constant::I32(v) => dat!(&v.to_be_bytes()),
//                 Constant::I64(v) => dat!(&v.to_be_bytes()),
//                 Constant::U8(v) => dat!(&v.to_be_bytes()),
//                 Constant::U16(v) => dat!(&v.to_be_bytes()),
//                 Constant::U32(v) => dat!(&v.to_be_bytes()),
//                 Constant::U64(v) => dat!(&v.to_be_bytes()),
//                 Constant::F32(v) => dat!(&v.to_be_bytes()),
//                 Constant::F64(v) => dat!(&v.to_be_bytes()),
//                 Constant::String(v) => dat!(v.as_bytes()),
//                 Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
//                 Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
//             },
//         }
//     }

//     pub fn assemble_mnemonic_default(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
//         macro_rules! constant {
//             ($kind:ident) => {
//                 for Node(crate::expression::args::$kind::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::$kind(arg), n));
//                 }
//             };
//         }
//         match mnemonic {
//             ".dbg" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context.report_info(args_node, format!("{args:#?}"))
//             }
//             ".info" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context
//                     .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
//             }
//             ".warning" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context
//                     .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
//             }
//             ".error" => {
//                 let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
//                 self.context
//                     .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
//             }

//             ".section" => {
//                 if let StrOpt::Val(Some(sec)) = self.eval().coerced(n).0 {
//                     T::set_section(self, sec, n);
//                 }
//             }

//             ".space" => {
//                 if let U32Opt::Val(Some(size)) = self.eval().coerced(n).0 {
//                     T::add_empty_space_data(self, size as usize, 1, n);
//                 }
//             }
//             ".data" => {
//                 for arg in self.eval().args(n, ArgumentsTypeHint::None).0 {
//                     T::add_value_as_data(self, arg);
//                 }
//             }
//             ".stringz" => {
//                 for Node(crate::expression::args::Str::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::String(arg), n));
//                     T::add_constant_as_data(self, Node(Constant::U8(0), n));
//                 }
//             }
//             ".string" => {
//                 for Node(crate::expression::args::Str::Val(arg), n) in
//                     self.eval().coerced::<Vec<_>>(n).0
//                 {
//                     T::add_constant_as_data(self, Node(Constant::String(arg), n));
//                 }
//             }
//             ".u8" => constant!(U8),
//             ".u16" => constant!(U16),
//             ".u32" => constant!(U32),
//             ".u64" => constant!(U64),
//             ".i8" => constant!(I8),
//             ".i16" => constant!(I16),
//             ".i32" => constant!(I32),
//             ".i64" => constant!(I64),
//             ".f32" => constant!(F32),
//             ".f64" => constant!(F64),
//             ".bool" => constant!(Bool),
//             ".char" => constant!(Char),

//             _ => self.unknown_mnemonic(Node(mnemonic, n)),
//         }
//     }
// }
