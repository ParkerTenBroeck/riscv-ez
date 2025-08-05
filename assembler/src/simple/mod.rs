use std::ops::{Deref, DerefMut};

use crate::{
    assembler::{LangCtx, lang::AssemblyLanguage},
    context::{Context, Node, NodeId},
    expression::{
        AssemblyLabel, AssemblyRegister, Constant, CustomValue, ExprCtx, Indexed, NodeVal,
    },
    preprocess::PreProcessor,
};

pub trait SimpleAssemblyLanguage<'a>: Sized + 'a {
    type Reg<'b>: AssemblyRegister<'a, Lang = SALImpl<'a, 'b, Self>>
    where
        'a: 'b;
    type Indexed<'b>: Indexed<'a, Lang = SALImpl<'a, 'b, Self>>
    where
        'a: 'b;
    type CustomValue<'b>: CustomValue<'a, Lang = SALImpl<'a, 'b, Self>>
    where
        'a: 'b;
    type Label<'b>: AssemblyLabel<'a, Lang = SALImpl<'a, 'b, Self>>
    where
        'a: 'b;
    type AssembledResult;
    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn add_label(&mut self, ctx: &mut SALCtx<'a, '_, '_, Self>, ident: &'a str, node: NodeId<'a>);
    fn set_section(
        &mut self,
        ctx: &mut SALCtx<'a, '_, '_, Self>,
        section: &'a str,
        node: NodeId<'a>,
    );

    fn add_empty_space_data(
        &mut self,
        ctx: &mut SALCtx<'a, '_, '_, Self>,
        size: usize,
        align: usize,
        node: NodeId<'a>,
    );
    fn add_bytes_as_data(
        &mut self,
        ctx: &mut SALCtx<'a, '_, '_, Self>,
        data: &[u8],
        align: usize,
        node: NodeId<'a>,
    );
    fn add_value_as_data(
        &mut self,
        ctx: &mut SALCtx<'a, '_, '_, Self>,
        value: NodeVal<'a, SALImpl<'a, '_, Self>>,
    );
    fn add_constant_as_data(
        &mut self,
        ctx: &mut SALCtx<'a, '_, '_, Self>,
        value: Node<'a, Constant<'a>>,
    );
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

impl<'a> SALState<'a> {
    // fn ctx<'b, 'c, T: SimpleAssemblyLanguage<'a>>(&'b mut self, ctx: &'b mut ExprCtx<'a, 'b, SALImpl<'a, 'c, T>>) -> SALCtx<'a, 'b, 'c, T>{
    //     SALCtx { context: ctx.context, preprocessor: ctx.preprocessor, state: self }
    // }
}

pub struct SALImpl<'a, 'b, T: SimpleAssemblyLanguage<'a>> {
    lang: &'b mut T,
    state: &'b mut SALState<'a>,
}

pub struct SALCtx<'a, 'b, 'c, T: SimpleAssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub preprocessor: &'b mut PreProcessor<'a, SALImpl<'a, 'c, T>>,
    pub state: &'b mut SALState<'a>,
}

impl<'a, 'b, T: SimpleAssemblyLanguage<'a>> Deref for SALImpl<'a, 'b, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.lang
    }
}

impl<'a, 'b, T: SimpleAssemblyLanguage<'a>> DerefMut for SALImpl<'a, 'b, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.lang
    }
}

impl<'a, 'b, T: SimpleAssemblyLanguage<'a>> SALImpl<'a, 'b, T> {
    pub fn add_constant_default(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        endianess: Endianess,
        constant: Node<'a, Constant<'a>>,
    ) {
        let align = constant.0.get_align();
        macro_rules! dat {
            ($expr:expr) => {
                self.lang.add_bytes_as_data(
                    &mut SALCtx {
                        context: ctx.context,
                        preprocessor: ctx.preprocessor,
                        state: self.state,
                    },
                    $expr,
                    align as usize,
                    constant.1,
                )
            };
        }
        match endianess {
            Endianess::Little => match constant.0 {
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
                Constant::String(v) => dat!(v.as_bytes()),
                Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
            },
            Endianess::Big => match constant.0 {
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
                Constant::String(v) => dat!(v.as_bytes()),
                Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
            },
        }
    }

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
}

impl<'a, 'b, T: SimpleAssemblyLanguage<'a>> AssemblyLanguage<'a> for SALImpl<'a, 'b, T> {
    type Reg = T::Reg<'b>;
    type Indexed = T::Indexed<'b>;
    type CustomValue = T::CustomValue<'b>;
    type Label = T::Label<'b>;
    type AssembledResult = T::AssembledResult;
    const DEFAULT_FLOAT_POSTFIX: &'a str = T::DEFAULT_FLOAT_POSTFIX;
    const DEFAULT_INTEGER_POSTFIX: &'a str = T::DEFAULT_INTEGER_POSTFIX;

    fn parse_ident(
        &mut self,
        ctx: &mut crate::expression::ExprCtx<'a, '_, Self>,
        ident: crate::context::Node<'a, &'a str>,
        hint: crate::expression::ValueType<'a, Self>,
    ) -> crate::expression::Value<'a, Self> {
        todo!()
    }

    fn encounter_label(
        &mut self,
        ctx: &mut crate::assembler::LangCtx<'a, '_, Self>,
        label: &'a str,
        n: NodeId<'a>,
    ) {
        todo!()
    }

    fn assemble_mnemonic(
        &mut self,
        ctx: &mut crate::assembler::LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: crate::context::NodeId<'a>,
    ) {
        todo!()
    }

    fn finish(&mut self, ctx: crate::assembler::LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        todo!()
    }
}
