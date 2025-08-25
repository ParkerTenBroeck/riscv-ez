use crate::context::Context;
use crate::expression::args::{AsmStrArg, UptrArg, UptrPow2Arg};
use crate::expression::conversion::AsmNum;
use crate::simple::trans::TranslationUnit;
use crate::{
    assembler::LangCtx,
    context::{Node, NodeRef},
    expression::{
        ArgumentsTypeHint, AsmStr, AssemblyLabel, AssemblyRegister, Constant, CustomValue, ExprCtx,
        FuncParamParser, Indexed, NodeVal, Value, ValueType,
        args::{StrArg, U32Arg, U32Pow2Arg},
        binop::BinOp,
        unop::UnOp,
    },
    lex::Number,
    logs::LogEntry,
    util::IntoStrDelimable,
};


use super::*;

pub trait SimpleAssemblyLanguageBase<'a>: SimpleAssemblyLanguage<'a> {
    fn add_constant_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        constant: Constant<'a, Self>,
        node: NodeRef<'a>,
    ) {
        use num_traits::ToPrimitive;
        let align = constant
            .get_align()
            .unwrap_or_default()
            .to_usize()
            .unwrap_or_default();
        macro_rules! dat {
            ($expr:expr) => {
                self.add_data(ctx, $expr, align, node)
            };
        }
        use num_traits::ToBytes;
        match self.state_mut().endianess {
            Endianess::Little => match constant {
                Constant::I8(v) => dat!(&v.to_le_bytes()),
                Constant::I16(v) => dat!(&v.to_le_bytes()),
                Constant::I32(v) => dat!(&v.to_le_bytes()),
                Constant::I64(v) => dat!(&v.to_le_bytes()),
                Constant::I128(v) => dat!(&v.to_le_bytes()),
                Constant::Isize(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Iptr(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::U8(v) => dat!(&v.to_le_bytes()),
                Constant::U16(v) => dat!(&v.to_le_bytes()),
                Constant::U32(v) => dat!(&v.to_le_bytes()),
                Constant::U64(v) => dat!(&v.to_le_bytes()),
                Constant::U128(v) => dat!(&v.to_le_bytes()),
                Constant::Usize(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Uptr(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::F32(v) => dat!(&v.to_le_bytes()),
                Constant::F64(v) => dat!(&v.to_le_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align, node);
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
                Constant::I128(v) => dat!(&v.to_be_bytes()),
                Constant::Isize(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Iptr(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::U8(v) => dat!(&v.to_be_bytes()),
                Constant::U16(v) => dat!(&v.to_be_bytes()),
                Constant::U32(v) => dat!(&v.to_be_bytes()),
                Constant::U64(v) => dat!(&v.to_be_bytes()),
                Constant::U128(v) => dat!(&v.to_be_bytes()),
                Constant::Usize(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Uptr(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::F32(v) => dat!(&v.to_be_bytes()),
                Constant::F64(v) => dat!(&v.to_be_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align, node);
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
        node: NodeRef<'a>,
    ) {
        let section = self.state_mut().expect_section(ctx.context, node);
        let node = ctx.context.node_to_owned(node);
        self.state_mut()
            .trans
            .get_mut(section)
            .data(data, align, Some(node));
    }

    fn add_space_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        space: usize,
        align: usize,
        node: NodeRef<'a>,
    ) {
        let section = self.state_mut().expect_section(ctx.context, node);
        let node = ctx.context.node_to_owned(node);
        self.state_mut()
            .trans
            .get_mut(section)
            .space(space, align, Some(node));
    }

    fn set_section(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        section: &'a str,
        node: NodeRef<'a>,
    ) {
        self.state_mut().current_section = Some(section);
    }
}

impl<'a, T: SimpleAssemblyLanguage<'a>> SimpleAssemblyLanguageBase<'a> for T {}
