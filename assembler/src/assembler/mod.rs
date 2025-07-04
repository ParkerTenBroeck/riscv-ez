pub mod context;
mod expression;
pub mod instructions;
pub mod translation;

use crate::assembler::expression::{ArgumentsTypeHint, Constant, ConvertResult, Value, ValueType};
use crate::util::IntoStrDelimable;
use crate::{
    assembler::{context::AssemblerContext, translation::Section},
    context::{Context, Node, NodeId},
    lex::Token,
    preprocess::PreProcessor,
};
use std::fmt::Write;
use std::rc::Rc;

pub struct Assembler<'a> {
    context: AssemblerContext<'a>,

    preprocessor: PreProcessor<'a>,
    peek: Option<Node<'a, Token<'a>>>,
    last: Option<Node<'a, Token<'a>>>,
}

impl<'a> Assembler<'a> {
    pub fn new(context: Rc<Context<'a>>, preprocessor: PreProcessor<'a>) -> Self {
        Self {
            context: AssemblerContext::new(context),
            preprocessor,
            peek: None,
            last: None,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> Vec<u8> {
        if let Some(src) = self.preprocessor.begin(path) {
            self.context.context.set_top_level_src(src);
        }

        self.context.tu.sections.insert(
            "text",
            Section {
                name: "text",
                start: None,
                align: 4,
                data: Vec::new(),
                fixer_uppers: Vec::new(),
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

    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        let n = self.peek.take().or_else(|| self.preprocessor.next());
        if n.is_some() {
            self.last = n;
        }
        n
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.preprocessor.next();
        }
        self.peek
    }

    fn args(&mut self, hint: ArgumentsTypeHint) -> Node<'a, Vec<Node<'a, Value<'a>>>> {
        let start = self.peek();
        let args = self.parse_arguments(hint, None);
        let args_node = self
            .context
            .context
            .merge_nodes(start.unwrap().1, self.last.unwrap().1);
        Node(args, args_node)
    }

    fn string_args(&mut self) -> Option<Node<'a, &'a str>> {
        match self
            .args(ArgumentsTypeHint::Mono(ValueType::String))
            .as_ref()
            .map(Vec::as_slice)
        {
            Node([Node(Value::Constant(Constant::String(sec)), node)], _) => Some(Node(sec, node)),
            Node(args, node) => {
                self.context.context.report_error(
                    node,
                    format!(
                        "invalid arguments [{}] expected [{}]",
                        args.iter().map(|i| i.0.get_type()).delim(", "),
                        [ValueType::String].iter().delim(", "),
                    ),
                );
                None
            }
        }
    }

    fn u32_args(&mut self) -> Option<Node<'a, u32>> {
        match self
            .args(ArgumentsTypeHint::Mono(ValueType::U32))
            .as_ref()
            .map(Vec::as_slice)
        {
            Node([Node(v @ Value::Constant(c), node)], _) => match c.to_u32() {
                ConvertResult::Success(value) => Some(Node(value, node)),
                ConvertResult::Lossy(value) => {
                    self.context
                        .context
                        .report_warning(node, "conversion to u32 is lossy");
                    Some(Node(value, node))
                }
                ConvertResult::Failure => {
                    self.context
                        .context
                        .report_error(node, format!("cannot convert {} to u32", v.get_type()));
                    None
                }
            },
            Node(args, node) => {
                self.context.context.report_error(
                    node,
                    format!(
                        "invalid arguments [{}] expected [{}]",
                        args.iter().map(|i| i.0.get_type()).delim(", "),
                        [ValueType::U32].iter().delim(", "),
                    ),
                );
                None
            }
        }
    }

    fn assemble_mnemonic(&mut self, mnemonic: &'a str, n: NodeId<'a>) {
        match mnemonic {
            "lui" => {}
            "auipc" => {}
            "jal" => {}
            "jalr" => {}

            "beq" => {}
            "bne" => {}
            "blt" => {}
            "bge" => {}
            "bltu" => {}
            "bgeu" => {}

            "lb" => {}
            "lh" => {}
            "lw" => {}
            "lbu" => {}
            "lhu" => {}

            "sb" => {}
            "sh" => {}
            "sw" => {}

            "addi" => {}
            "li" => {}
            "ecall" => {}

            ".info" => {
                let Node(args, args_node) = self.args(ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = self.args(ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = self.args(ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }

            ".global" => {
                if let Some(Node(sec, node)) = self.string_args() {
                    self.context
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".local" => {
                if let Some(Node(sec, node)) = self.string_args() {
                    self.context
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }
            ".weak" => {
                if let Some(Node(sec, node)) = self.string_args() {
                    self.context
                        .context
                        .report_warning(node, "not implemented yet");
                }
            }

            ".section" => {
                if let Some(Node(sec, _)) = self.string_args() {
                    self.context.set_current_section(sec);
                }
            }
            ".org" => {
                if let Some(Node(org, node)) = self.u32_args() {
                    if self.context.get_current_section().start.is_some() {
                        self.context
                            .context
                            .report_warning(node, "origin previously set");
                    }
                    self.context.get_current_section().start = Some(org);
                }
            }
            ".space" => {
                if let Some(Node(size, _)) = self.u32_args() {
                    self.context.add_data(size);
                }
            }
            ".align" => {
                if let Some(Node(size, _)) = self.u32_args() {
                    todo!()
                    // self.context.add_data(size);
                }
            }

            ".data" => {
                for arg in self.args(ArgumentsTypeHint::None).0 {
                    match arg.0 {
                        Value::Constant(v) => match v {
                            Constant::I8(v) => {
                                *self.context.add_data_const::<1>() = v.to_le_bytes()
                            }
                            Constant::I16(v) => {
                                *self.context.add_data_const::<2>() = v.to_le_bytes()
                            }
                            Constant::I32(v) => {
                                *self.context.add_data_const::<4>() = v.to_le_bytes()
                            }
                            Constant::I64(v) => {
                                *self.context.add_data_const::<8>() = v.to_le_bytes()
                            }
                            Constant::U8(v) => {
                                *self.context.add_data_const::<1>() = v.to_le_bytes()
                            }
                            Constant::U16(v) => {
                                *self.context.add_data_const::<2>() = v.to_le_bytes()
                            }
                            Constant::U32(v) => {
                                *self.context.add_data_const::<4>() = v.to_le_bytes()
                            }
                            Constant::U64(v) => {
                                *self.context.add_data_const::<8>() = v.to_le_bytes()
                            }
                            Constant::F32(v) => {
                                *self.context.add_data_const::<4>() = v.to_le_bytes()
                            }
                            Constant::F64(v) => {
                                *self.context.add_data_const::<8>() = v.to_le_bytes()
                            }
                            Constant::String(v) => self
                                .context
                                .add_data(v.len() as u32)
                                .copy_from_slice(v.as_bytes()),
                            Constant::Char(v) => {
                                *self.context.add_data_const::<4>() = (v as u32).to_le_bytes()
                            }
                            Constant::Bool(v) => {
                                *self.context.add_data_const::<1>() = (v as u8).to_le_bytes()
                            }
                        },
                        Value::Label(l) => {
                            self.context.add_data(4);
                        }
                        _ => self
                            .context
                            .context
                            .report_error(arg.1, format!("invalid type {}", arg.0.get_type())),
                    }
                }
            }

            ".string" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.context
                            .add_data(v.len() as u32)
                            .copy_from_slice(v.as_bytes())
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }
            ".stringz" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.context
                            .add_data(v.len() as u32)
                            .copy_from_slice(v.as_bytes());
                        self.context.add_data(1);
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }

            ".u8" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::U8(v)) = arg.0 {
                        *self.context.add_data_const::<1>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u16" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U16)).0 {
                    if let Value::Constant(Constant::U16(v)) = arg.0 {
                        *self.context.add_data_const::<2>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u32" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U32)).0 {
                    if let Value::Constant(Constant::U32(v)) = arg.0 {
                        *self.context.add_data_const::<4>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u64" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::U64)).0 {
                    if let Value::Constant(Constant::U64(v)) = arg.0 {
                        *self.context.add_data_const::<8>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".i8" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::I8)).0 {
                    if let Value::Constant(Constant::I8(v)) = arg.0 {
                        *self.context.add_data_const::<1>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i16" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::I16)).0 {
                    if let Value::Constant(Constant::I16(v)) = arg.0 {
                        *self.context.add_data_const::<2>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i32" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::I32)).0 {
                    if let Value::Constant(Constant::I32(v)) = arg.0 {
                        *self.context.add_data_const::<4>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i64" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::I64)).0 {
                    if let Value::Constant(Constant::I64(v)) = arg.0 {
                        *self.context.add_data_const::<8>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".f32" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::F32)).0 {
                    if let Value::Constant(Constant::F32(v)) = arg.0 {
                        *self.context.add_data_const::<4>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".f64" => {
                for arg in self.args(ArgumentsTypeHint::Mono(ValueType::F64)).0 {
                    if let Value::Constant(Constant::F64(v)) = arg.0 {
                        *self.context.add_data_const::<8>() = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f64", arg.0.get_type()),
                        )
                    }
                }
            }

            _ => self
                .context
                .context
                .report_error(n, format!("Unrecognized mnemonic '{mnemonic}'")),
        }
    }

    fn assemble_line(&mut self) {
        match self.next() {
            Some(Node(Token::Ident(ident), n)) => {
                self.assemble_mnemonic(ident, n);
                loop {
                    match self.peek() {
                        None | Some(Node(Token::NewLine, _)) => break,
                        Some(Node(t, n)) => self
                            .context
                            .context
                            .report_error(n, format!("Unexpected token '{t:?}' at end of line")),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.context.add_label(label, source);
            }
            Some(Node(t, n)) => self.context.context.report_error(
                n,
                format!("Unexpected token {t:?} expected identifier or label"),
            ),
            None => {}
        }
    }
}
