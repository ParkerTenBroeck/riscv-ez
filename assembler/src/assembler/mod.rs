pub mod context;
pub mod translation;

use crate::assembler::translation::{CalculationKind, FormKind};
use crate::context::NodeId;
use crate::expression::args::{StrOpt, U32Opt, U32Power2Opt};
use crate::expression::{
    ArgumentsTypeHint, AssemblyRegister, Constant, CustomValue, CustomValueType, ExpressionEvaluatorContext, Indexed, Value, ValueType
};
use crate::util::IntoStrDelimable;
use crate::{
    assembler::{context::AssemblerState, translation::Section},
    context::{Context, Node},
    lex::Token,
    preprocess::PreProcessor,
};

pub trait AssemblyLanguage<'a>: Sized + Clone + 'a{
    type RegType: AssemblyRegister;
    type Indexed: Indexed<'a, Self>;
    type CustomValue: CustomValue<'a, Self>;
    type CustomValueType: CustomValueType<'a, Self>;

    fn parse_ident_assembly(
        asm: &mut Assembler<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Node<'a, Value<'a, Self>>;

    fn parse_ident_preprocessor(
        asm: &mut Assembler<'a, '_, Self>,
        ident: Node<'a, &'a str>,
        hint: ValueType<'a, Self>,
    ) -> Node<'a, Value<'a, Self>> {
        Self::parse_ident_assembly(asm, ident, hint)
    }

    fn assemble_mnemonic(asm: &mut Assembler<'a, '_, Self>, mnemonic: Node<'a, &'a str>);
}

pub struct Assembler<'a, 'b, T: AssemblyLanguage<'a>> {
    pub state: &'b mut AssemblerState<'a, T>,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
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
        if let Some(src) = self.preprocessor.begin(&mut self.state, path) {
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
                T::assemble_mnemonic(self, Node(ident, n));
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

    pub fn assemble_mnemonic_default(&mut self, Node(mnemonic, n): Node<'a, &'a str>) {
        match mnemonic {
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
                if let (Node(StrOpt(Some(_label)), node),) = self.coerced(n) {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".local" => {
                if let (Node(StrOpt(Some(_label)), node),) = self.coerced(n) {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".weak" => {
                if let (Node(StrOpt(Some(_label)), node),) = self.coerced(n) {
                    self.state
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }

            ".section" => {
                if let (StrOpt(Some(sec)),) = self.coerced(n) {
                    self.state.set_current_section(sec);
                }
            }
            ".org" => {
                if let (Node(U32Opt(Some(org)), node),) = self.coerced(n) {
                    if self.state.get_current_section().start.is_some() {
                        self.state
                            .context
                            .report_warning(node, "origin previously set");
                    }
                    self.state.get_current_section().start = Some(org);
                }
            }
            ".space" => {
                if let (U32Opt(Some(size)),) = self.coerced(n) {
                    self.state.add_data(size, 1);
                }
            }
            ".align" => {
                if let (U32Power2Opt(Some(align)),) = self.coerced(n) {
                    self.state.add_data(0, align);
                }
            }

            ".data" => {
                for arg in self.args(n, ArgumentsTypeHint::None).0 {
                    let align = arg.0.get_align().unwrap_or(1);
                    match arg.0 {
                        Value::Constant(v) => match v {
                            Constant::I8(v) => {
                                *self.state.add_data_const::<1>(align) = v.to_le_bytes()
                            }
                            Constant::I16(v) => {
                                *self.state.add_data_const::<2>(align) = v.to_le_bytes()
                            }
                            Constant::I32(v) => {
                                *self.state.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::I64(v) => {
                                *self.state.add_data_const::<8>(align) = v.to_le_bytes()
                            }
                            Constant::U8(v) => {
                                *self.state.add_data_const::<1>(align) = v.to_le_bytes()
                            }
                            Constant::U16(v) => {
                                *self.state.add_data_const::<2>(align) = v.to_le_bytes()
                            }
                            Constant::U32(v) => {
                                *self.state.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::U64(v) => {
                                *self.state.add_data_const::<8>(align) = v.to_le_bytes()
                            }
                            Constant::F32(v) => {
                                *self.state.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::F64(v) => {
                                *self.state.add_data_const::<8>(align) = v.to_le_bytes()
                            }
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
                        Value::Label(l) => {
                            self.state.ins_or_address_reloc(
                                0,
                                l.ident,
                                l.offset,
                                CalculationKind::Absolute,
                                FormKind::Full,
                            );
                        }
                        _ => self
                            .state
                            .context
                            .report_error(arg.1, format!("invalid type {}", arg.0.get_type())),
                    }
                }
            }

            ".string" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.state
                            .add_data(v.len() as u32, 1)
                            .copy_from_slice(v.as_bytes())
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }
            ".stringz" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.state
                            .add_data(v.len() as u32, 1)
                            .copy_from_slice(v.as_bytes());
                        self.state.add_data(1, 1);
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }

            ".u8" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::U8(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<1>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u16" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U16)).0 {
                    if let Value::Constant(Constant::U16(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<2>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U32)).0 {
                    if let Value::Constant(Constant::U32(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U64)).0 {
                    if let Value::Constant(Constant::U64(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".i8" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I8)).0 {
                    if let Value::Constant(Constant::I8(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<1>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i16" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I16)).0 {
                    if let Value::Constant(Constant::I16(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<2>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I32)).0 {
                    if let Value::Constant(Constant::I32(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I64)).0 {
                    if let Value::Constant(Constant::I64(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".f32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::F32)).0 {
                    if let Value::Constant(Constant::F32(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".f64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::F64)).0 {
                    if let Value::Constant(Constant::F64(v)) = arg.0 {
                        *self
                            .state
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.state.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f64", arg.0.get_type()),
                        )
                    }
                }
            }

            _ => self.unknown_mnemonic(Node(mnemonic, n)),
        }
    }
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluatorContext<'a, L> for Assembler<'a, 'b, L> {
    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor.next(&mut self.state)
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor.peek(&mut self.state)
    }

    fn context(&mut self) -> &mut Context<'a> {
        &mut self.state.context
    }

    fn parse_ident(
        &mut self,
        ident: &'a str,
        node: NodeId<'a>,
        hint: ValueType<'a, L>,
    ) -> Node<'a, Value<'a, L>> {
        L::parse_ident_assembly(self, Node(ident, node), hint)
    }
}
