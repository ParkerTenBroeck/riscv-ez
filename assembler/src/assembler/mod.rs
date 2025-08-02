pub mod context;
pub mod lang;
pub mod translation;

use crate::assembler::lang::AssemblyLanguage;
use crate::expression::args::{StrOpt, U32Opt, U32Pow2Opt};
use crate::expression::{ArgumentsTypeHint, Constant, ExpressionEvaluatorContext};
use crate::util::IntoStrDelimable;
use crate::{
    assembler::{context::AssemblerState, translation::Section},
    context::Node,
    lex::Token,
    preprocess::PreProcessor,
};

pub struct Assembler<'a, 'b, T: AssemblyLanguage<'a>> {
    pub state: &'b mut AssemblerState<'a, T>,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Endianess {
    Little,
    Big,
}

impl<'a, 'b, T: AssemblyLanguage<'a>> Assembler<'a, 'b, T> {
    pub fn new(
        state: &'b mut AssemblerState<'a, T>,
        preprocessor: &'b mut PreProcessor<'a, T>,
    ) -> Self {
        Self {
            state,
            preprocessor,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> Vec<u8> {
        if let Some(src) = self.preprocessor.begin(self.state, path) {
            self.state.context.set_top_level_src(src);
        }

        self.state.tu.sections.insert(
            "text",
            Section {
                name: "text",
                start: None,
                align: 4,
                data: Vec::new(),
                relocs: Vec::new(),
            },
        );

        while let Some(Node(Token::NewLine, _)) = self.peek() {
            self.next();
        }
        while self.peek().is_some() {
            self.assemble_line();
            while let Some(Node(Token::NewLine, _)) = self.peek() {
                self.next();
            }
        }

        Vec::new()
    }

    fn assemble_line(&mut self) {
        match self.next() {
            Some(Node(Token::Ident(ident), n)) => {
                T::assemble_mnemonic(self, ident, n);
                loop {
                    match self.peek() {
                        None | Some(Node(Token::NewLine, _)) => break,
                        Some(Node(t, n)) => self
                            .state
                            .context
                            .report_error(n, format!("Unexpected token '{t:#}' at end of line")),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.state.add_label(label, source);
            }
            Some(Node(t, n)) => self.state.context.report_error(
                n,
                format!("Unexpected token {t:#} expected mnemonic or label"),
            ),
            None => {}
        }
    }

    pub fn unknown_mnemonic(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
        self.state
            .context
            .report_error(n, format!("Unrecognized mnemonic '{mnemonic}'"));

        while !matches!(self.peek(), None | Some(Node(Token::NewLine, _))) {
            self.next();
        }
    }

    pub fn add_constant_default(&mut self, endianess: Endianess, constant: Node<'a, Constant<'a>>) {
        let align = constant.0.get_align().unwrap_or(1);
        match endianess {
            Endianess::Little => match constant.0 {
                Constant::I8(v) => *self.state.add_data_const::<1>(align) = v.to_le_bytes(),
                Constant::I16(v) => *self.state.add_data_const::<2>(align) = v.to_le_bytes(),
                Constant::I32(v) => *self.state.add_data_const::<4>(align) = v.to_le_bytes(),
                Constant::I64(v) => *self.state.add_data_const::<8>(align) = v.to_le_bytes(),
                Constant::U8(v) => *self.state.add_data_const::<1>(align) = v.to_le_bytes(),
                Constant::U16(v) => *self.state.add_data_const::<2>(align) = v.to_le_bytes(),
                Constant::U32(v) => *self.state.add_data_const::<4>(align) = v.to_le_bytes(),
                Constant::U64(v) => *self.state.add_data_const::<8>(align) = v.to_le_bytes(),
                Constant::F32(v) => *self.state.add_data_const::<4>(align) = v.to_le_bytes(),
                Constant::F64(v) => *self.state.add_data_const::<8>(align) = v.to_le_bytes(),
                Constant::String(v) => self
                    .state
                    .add_data(v.len() as u32, align)
                    .copy_from_slice(v.as_bytes()),
                Constant::Char(v) => {
                    *self.state.add_data_const::<4>(align) = (v as u32).to_le_bytes()
                }
                Constant::Bool(v) => {
                    *self.state.add_data_const::<1>(align) = (v as u8).to_le_bytes()
                }
            },
            Endianess::Big => match constant.0 {
                Constant::I8(v) => *self.state.add_data_const::<1>(align) = v.to_be_bytes(),
                Constant::I16(v) => *self.state.add_data_const::<2>(align) = v.to_be_bytes(),
                Constant::I32(v) => *self.state.add_data_const::<4>(align) = v.to_be_bytes(),
                Constant::I64(v) => *self.state.add_data_const::<8>(align) = v.to_be_bytes(),
                Constant::U8(v) => *self.state.add_data_const::<1>(align) = v.to_be_bytes(),
                Constant::U16(v) => *self.state.add_data_const::<2>(align) = v.to_be_bytes(),
                Constant::U32(v) => *self.state.add_data_const::<4>(align) = v.to_be_bytes(),
                Constant::U64(v) => *self.state.add_data_const::<8>(align) = v.to_be_bytes(),
                Constant::F32(v) => *self.state.add_data_const::<4>(align) = v.to_be_bytes(),
                Constant::F64(v) => *self.state.add_data_const::<8>(align) = v.to_be_bytes(),
                Constant::String(v) => self
                    .state
                    .add_data(v.len() as u32, align)
                    .copy_from_slice(v.as_bytes()),
                Constant::Char(v) => {
                    *self.state.add_data_const::<4>(align) = (v as u32).to_be_bytes()
                }
                Constant::Bool(v) => {
                    *self.state.add_data_const::<1>(align) = (v as u8).to_be_bytes()
                }
            },
        }
    }

    pub fn assemble_mnemonic_default(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
        macro_rules! constant {
            ($kind:ident) => {
                for Node(crate::expression::args::$kind::Val(arg), n) in self.coerced::<Vec<_>>(n).0
                {
                    T::add_constant_as_data(self, Node(Constant::$kind(arg), n));
                }
            };
        }
        match mnemonic {
            ".dbg" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.state
                    .context
                    .report_info(args_node, format!("{args:#?}"))
            }
            ".info" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.state
                    .context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.state
                    .context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.state
                    .context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }

            ".global" => {
                if let Node(StrOpt::Val(Some(_label)), node) = self.coerced(n).0 {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".local" => {
                if let Node(StrOpt::Val(Some(_label)), node) = self.coerced(n).0 {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".weak" => {
                if let Node(StrOpt::Val(Some(_label)), node) = self.coerced(n).0 {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }

            ".section" => {
                if let StrOpt::Val(Some(sec)) = self.coerced(n).0 {
                    self.state.set_current_section(sec);
                }
            }
            ".org" => {
                if let Node(U32Opt::Val(Some(org)), node) = self.coerced(n).0 {
                    if self.state.get_current_section().start.is_some() {
                        self.state
                            .context
                            .report_warning(node, "origin previously set");
                    }
                    self.state.get_current_section().start = Some(org);
                }
            }
            ".space" => {
                if let U32Opt::Val(Some(size)) = self.coerced(n).0 {
                    self.state.add_data(size, 1);
                }
            }
            ".align" => {
                if let U32Pow2Opt::Val(Some(align)) = self.coerced(n).0 {
                    self.state.add_data(0, align);
                }
            }
            ".data" => {
                for arg in self.args(n, ArgumentsTypeHint::None).0 {
                    T::add_value_as_data(self, arg);
                }
            }
            ".stringz" => {
                for Node(crate::expression::args::Str::Val(arg), n) in self.coerced::<Vec<_>>(n).0 {
                    T::add_constant_as_data(self, Node(Constant::String(arg), n));
                    T::add_constant_as_data(self, Node(Constant::U8(0), n));
                }
            }
            ".string" => {
                for Node(crate::expression::args::Str::Val(arg), n) in self.coerced::<Vec<_>>(n).0 {
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
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluatorContext<'a, L> for Assembler<'a, 'b, L> {
    const KIND: crate::expression::ExprKind = crate::expression::ExprKind::Assembler;

    fn asm(&mut self) -> Assembler<'a, '_, L> {
        Assembler {
            state: self.state,
            preprocessor: self.preprocessor,
        }
    }
    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor.next(self.state)
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor.peek(self.state)
    }
}
