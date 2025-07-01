pub mod context;
mod expression;
pub mod instructions;
pub mod translation;

use std::{borrow::Cow, cell::RefCell, rc::Rc};

use crate::{
    assembler::{context::AssemblerContext, translation::Section},
    context::{Context, Node, NodeId},
    lex::{Number, Token},
    preprocess::PreProcessor,
};

pub struct Assembler<'a> {
    context: AssemblerContext<'a>,

    preprocessor: PreProcessor<'a>,
    peek: Option<Node<'a, Token<'a>>>,
    last: Option<Node<'a, Token<'a>>>,
}

#[derive(Clone, Copy, Debug)]
pub enum Constant<'a> {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),

    F32(f32),
    F64(f64),

    StringLiteral(&'a str),
    CharLiteral(char),
    Bool(bool),
}

pub enum ConvertResult<T> {
    Success(T),
    Lossy(T),
    Failure,
}

impl<'a> Constant<'a> {
    pub fn to_u32(&self) -> ConvertResult<u32> {
        match *self {
            Constant::I8(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I8(v) => ConvertResult::Success(v as u32),
            Constant::I16(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I16(v) => ConvertResult::Success(v as u32),
            Constant::I32(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I32(v) => ConvertResult::Success(v as u32),
            Constant::I64(v) if v > u32::MAX as i64 => ConvertResult::Lossy(v as u32),
            Constant::I64(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I64(v) => ConvertResult::Success(v as u32),
            Constant::U8(v) => ConvertResult::Success(v as u32),
            Constant::U16(v) => ConvertResult::Success(v as u32),
            Constant::U32(v) => ConvertResult::Success(v),
            Constant::U64(v) if v > u32::MAX as u64 => ConvertResult::Lossy(v as u32),
            Constant::U64(v) => ConvertResult::Success(v as u32),
            Constant::F32(_) => ConvertResult::Failure,
            Constant::F64(_) => ConvertResult::Failure,
            Constant::StringLiteral(_) => ConvertResult::Failure,
            Constant::CharLiteral(_) => ConvertResult::Failure,
            Constant::Bool(_) => ConvertResult::Failure,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParsedArgument<'a> {
    Label(&'a str, i64),
    Register(u8),
    Constant(Constant<'a>),
}

impl<'a> Assembler<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>, preprocessor: PreProcessor<'a>) -> Self {
        Self {
            context: AssemblerContext::new(context),
            preprocessor,
            peek: None,
            last: None,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> Vec<u8> {
        self.preprocessor.begin(path);
        self.context.tu.sections.insert(
            "text",
            Section {
                name: "text",
                start: None,
                data: Vec::new(),
                size: 0,
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

    fn assemble_mnemonic(&mut self, mnemonic: &'a str, n: NodeId<'a>) {
        let start = self.peek();
        let Ok(args) = self.gather_arguments(mnemonic, n) else {
            return;
        };
        let args_node = self
            .context
            .context
            .borrow_mut()
            .merge_nodes(start.unwrap().1, self.last.unwrap().1);

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

            ".global" => {}
            ".local" => {}
            ".weak" => {}

            ".space" => {}

            ".data" => {}

            ".string" => {}
            ".stringz" => {}

            ".u8" => {}
            ".u16" => {}
            ".u32" => {}
            ".u64" => {}

            ".i8" => {}
            ".i16" => {}
            ".i32" => {}
            ".i64" => {}

            ".f32" => {}
            ".f64" => {
                for arg in args {
                    if let Node(ParsedArgument::Constant(Constant::F32(a)), _) = arg {}
                }
            }

            ".section" => match &args[..] {
                [Node(ParsedArgument::Constant(Constant::StringLiteral(str)), _)] => {
                    self.context.set_current_section(str);
                }
                rem => {
                    self.context
                        .context
                        .borrow_mut()
                        .report_error(args_node, "Invalid arguments {rem:?}");
                }
            },
            ".org" => {
                let mut section = self.context.get_current_section();
                if section.start.is_some() {
                    self.context.context.borrow_mut().report_warning(
                        n,
                        format!(
                            "Section '{}' has already had org set",
                            self.context.current_section
                        ),
                    );
                    section = self.context.get_current_section();
                }
                match &args[..] {
                    [Node(ParsedArgument::Constant(constant), n)] => match constant.to_u32() {
                        ConvertResult::Success(value) => section.start = Some(value),
                        ConvertResult::Lossy(value) => {
                            section.start = Some(value);
                            self.context
                                .context
                                .borrow_mut()
                                .report_warning(n, "Conversion to u32 is lossy");
                        }
                        ConvertResult::Failure => {
                            self.context
                                .context
                                .borrow_mut()
                                .report_error(n, "Argument not convertable to u32");
                        }
                    },
                    rem => {
                        self.context
                            .context
                            .borrow_mut()
                            .report_error(args_node, "Invalid arguments {rem:?}");
                    }
                }
            }

            _ => self
                .context
                .context
                .borrow_mut()
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
                            .borrow_mut()
                            .report_error(n, format!("Unexpected token '{t:?}' at end of line")),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.context.add_label(label, source);
            }
            Some(Node(t, n)) => self.context.context.borrow_mut().report_error(
                n,
                format!("Unexpected token {t:?} expected identifier or label"),
            ),
            None => {}
        }
    }
}
