pub mod context;
pub mod lang;
pub mod translation;

use crate::assembler::lang::AssemblyLanguage;
use crate::context::Context;
use crate::expression::args::{StrOpt, U32Opt};
use crate::expression::{ArgumentsTypeHint, Constant, ExpressionEvaluator};
use crate::util::IntoStrDelimable;
use crate::{context::Node, lex::Token, preprocess::PreProcessor};

pub struct LangCtx<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    preprocessor: &'b mut PreProcessor<'a, T>,
}
impl<'a, 'b, T: AssemblyLanguage<'a>> LangCtx<'a, 'b, T> {
    pub fn new(context: &'b mut Context<'a>, preprocessor: &'b mut PreProcessor<'a, T>) -> Self {
        Self {
            context,
            preprocessor,
        }
    }

    pub fn asm<'c>(&'c mut self, lang: &'c mut T) -> Assembler<'a, 'c, T> {
        Assembler {
            context: self.context,
            lang,
            preprocessor: self.preprocessor,
        }
    }

    pub fn eval<'c>(&'c mut self, lang: &'c mut T) -> ExpressionEvaluator<'a, 'c, T> {
        ExpressionEvaluator::new(
            self.context,
            lang,
            self.preprocessor,
            crate::expression::ExprKind::Lang,
        )
    }
}
pub struct PreProcessorCtx<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    lang: &'b mut T,
}
impl<'a, 'b, T: AssemblyLanguage<'a>> PreProcessorCtx<'a, 'b, T> {
    pub fn new(context: &'b mut Context<'a>, lang: &'b mut T) -> Self {
        Self { context, lang }
    }

    pub fn asm(&mut self, preprocessor: &'b mut PreProcessor<'a, T>) -> Assembler<'a, '_, T> {
        Assembler {
            context: self.context,
            lang: self.lang,
            preprocessor,
        }
    }

    pub fn eval<'c>(
        &'c mut self,
        preprocessor: &'c mut PreProcessor<'a, T>,
    ) -> ExpressionEvaluator<'a, 'c, T> {
        ExpressionEvaluator::new(
            self.context,
            self.lang,
            preprocessor,
            crate::expression::ExprKind::PreProcessor,
        )
    }
}

pub struct Assembler<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub lang: &'b mut T,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Endianess {
    Little,
    Big,
}

impl<'a, 'b, T: AssemblyLanguage<'a>> Assembler<'a, 'b, T> {
    pub fn new(
        context: &'b mut Context<'a>,
        lang: &'b mut T,
        preprocessor: &'b mut PreProcessor<'a, T>,
    ) -> Self {
        Self {
            context,
            lang,
            preprocessor,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> T::AssembledResult {
        if let Some(src) = self
            .preprocessor
            .begin(PreProcessorCtx::new(self.context, self.lang), path)
        {
            self.context.set_top_level_src(src);
        }

        while let Some(Node(Token::NewLine, _)) = self.peek() {
            self.next();
        }
        while self.peek().is_some() {
            self.assemble_line();
            while let Some(Node(Token::NewLine, _)) = self.peek() {
                self.next();
            }
        }

        self.lang
            .finish(LangCtx::new(self.context, self.preprocessor))
    }

    fn assemble_line(&mut self) {
        match self.next() {
            Some(Node(Token::Ident(ident), n)) => {
                self.lang.assemble_mnemonic(&mut LangCtx::new(self.context, self.preprocessor), ident, n);
                loop {
                    match self.peek() {
                        None | Some(Node(Token::NewLine, _)) => break,
                        Some(Node(t, n)) => self
                            .context
                            .report_error(n, format!("Unexpected token '{t:#}' at end of line")),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                T::add_label(self, label, source);
            }
            Some(Node(t, n)) => self.context.report_error(
                n,
                format!("Unexpected token {t:#} expected mnemonic or label"),
            ),
            None => {}
        }
    }

    pub fn unknown_mnemonic(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
        self.context
            .report_error(n, format!("Unrecognized mnemonic '{mnemonic}'"));

        while !matches!(self.peek(), None | Some(Node(Token::NewLine, _))) {
            self.next();
        }
    }

    pub fn add_constant_default(&mut self, endianess: Endianess, constant: Node<'a, Constant<'a>>) {
        let align = constant.0.get_align();
        macro_rules! dat {
            ($expr:expr) => {
                T::add_bytes_as_data(self, $expr, align as usize, constant.1)
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

    pub fn assemble_mnemonic_default(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
        macro_rules! constant {
            ($kind:ident) => {
                for Node(crate::expression::args::$kind::Val(arg), n) in
                    self.eval().coerced::<Vec<_>>(n).0
                {
                    T::add_constant_as_data(self, Node(Constant::$kind(arg), n));
                }
            };
        }
        match mnemonic {
            ".dbg" => {
                let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
                self.context.report_info(args_node, format!("{args:#?}"))
            }
            ".info" => {
                let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
                self.context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
                self.context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = self.eval().args(n, ArgumentsTypeHint::None);
                self.context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".space" => {
                if let U32Opt::Val(Some(size)) = self.eval().coerced(n).0 {
                    T::add_empty_space_data(self, size as usize, 1, n);
                }
            }
            ".section" => {
                if let StrOpt::Val(Some(sec)) = self.eval().coerced(n).0 {
                    T::set_section(self, sec, n);
                }
            }

            ".data" => {
                for arg in self.eval().args(n, ArgumentsTypeHint::None).0 {
                    T::add_value_as_data(self, arg);
                }
            }
            ".stringz" => {
                for Node(crate::expression::args::Str::Val(arg), n) in
                    self.eval().coerced::<Vec<_>>(n).0
                {
                    T::add_constant_as_data(self, Node(Constant::String(arg), n));
                    T::add_constant_as_data(self, Node(Constant::U8(0), n));
                }
            }
            ".string" => {
                for Node(crate::expression::args::Str::Val(arg), n) in
                    self.eval().coerced::<Vec<_>>(n).0
                {
                    T::add_constant_as_data(self, Node(Constant::String(arg), n));
                }
            }
            ".u8" => constant!(U8),
            ".u16" => constant!(U16),
            ".u32" => constant!(U32),
            ".u64" => constant!(U64),
            ".i8" => constant!(I8),
            ".i16" => constant!(I16),
            ".i32" => constant!(I32),
            ".i64" => constant!(I64),
            ".f32" => constant!(F32),
            ".f64" => constant!(F64),
            ".bool" => constant!(Bool),
            ".char" => constant!(Char),

            _ => self.unknown_mnemonic(Node(mnemonic, n)),
        }
    }

    fn eval(&mut self) -> ExpressionEvaluator<'a, '_, T> {
        ExpressionEvaluator::new(
            self.context,
            self.lang,
            self.preprocessor,
            crate::expression::ExprKind::Assembler,
        )
    }

    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .next(PreProcessorCtx::new(self.context, self.lang))
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .peek(PreProcessorCtx::new(self.context, self.lang))
    }
}
