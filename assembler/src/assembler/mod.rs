pub mod context;
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
        self.peek.take().or_else(|| self.preprocessor.next())
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.preprocessor.next();
        }
        self.peek
    }

    fn parse_numeric_literal(&mut self, num: Number<'a>, n: NodeId<'a>) -> Node<'a, Constant<'a>> {
        let (suffix, radix) = match num.get_hint() {
            crate::lex::TypeHint::Float => (num.get_suffix().unwrap_or("f32"), 10),
            crate::lex::TypeHint::Hex => (num.get_suffix().unwrap_or("i32"), 16),
            crate::lex::TypeHint::Bin => (num.get_suffix().unwrap_or("i32"), 2),
            crate::lex::TypeHint::Int => (num.get_suffix().unwrap_or("i32"), 10),
        };

        macro_rules! integer {
            ($num:ty) => {
                <$num>::from_str_radix(num.get_num(), radix)
                    .inspect_err(|e| {
                        self.context
                            .context
                            .borrow_mut()
                            .report_error(Node(format!("Invalid numeric literal {e}"), n));
                    })
                    .unwrap_or(0)
            };
        }

        macro_rules! float {
            ($num:ty) => {
                num.get_num()
                    .parse()
                    .inspect_err(|e| {
                        self.context
                            .context
                            .borrow_mut()
                            .report_error(Node(format!("Invalid numeric literal {e}"), n));
                    })
                    .unwrap_or(0.0)
            };
        }

        Node(
            match suffix {
                "i8" => Constant::I8(integer!(i8)),
                "i16" => Constant::I16(integer!(i16)),
                "i32" => Constant::I32(integer!(i32)),
                "i64" => Constant::I64(integer!(i64)),
                "u8" => Constant::U8(integer!(u8)),
                "u16" => Constant::U16(integer!(u16)),
                "u32" => Constant::U32(integer!(u32)),
                "u64" => Constant::U64(integer!(u64)),

                "f32" => Constant::F32(float!(f32)),
                "f64" => Constant::F64(float!(f64)),

                suffix => {
                    self.context
                        .context
                        .borrow_mut()
                        .report_error(Node(format!("Unknown numeric suffix '{suffix}'"), n));
                    Constant::I32(0)
                }
            },
            n,
        )
    }

    fn parse_argument(&mut self) -> Option<Node<'a, ParsedArgument<'a>>> {
        match self.peek() {
            Some(Node(Token::NumericLiteral(num), n)) => {
                self.next();
                Some(
                    self.parse_numeric_literal(num, n)
                        .map(ParsedArgument::Constant),
                )
            }
            Some(Node(Token::Ident(str), n)) => {
                self.next();
                Some(Node(ParsedArgument::Label(str, 0), n))
            }
            Some(Node(Token::StringLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::StringLiteral(
                        self.parse_string_literal(str, n),
                    )),
                    n,
                ))
            }
            Some(Node(Token::CharLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::CharLiteral(
                        self.parse_char_literal(str, n),
                    )),
                    n,
                ))
            }
            Some(Node(Token::FalseLiteral, n)) => {
                self.next();
                Some(Node(ParsedArgument::Constant(Constant::Bool(false)), n))
            }
            Some(Node(Token::TrueLiteral, n)) => {
                self.next();
                Some(Node(ParsedArgument::Constant(Constant::Bool(true)), n))
            }
            _ => None,
        }
    }

    fn parse_char_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> char {
        let mut chars = repr.chars();
        if let Some(ok) = chars.next() {
            if chars.next().is_some() {
                self.context
                    .context
                    .borrow_mut()
                    .report_error(Node("Char literal contains more than one char", n));
            }
            ok
        } else {
            self.context
                .context
                .borrow_mut()
                .report_error(Node("Char literal empty", n));
            '\0'
        }
    }

    fn parse_string_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> &'a str {
        repr.into()
    }

    fn gather_arguments(
        &mut self,
        mnemonic: &'a str,
        n: NodeId<'a>,
    ) -> Result<Vec<Node<'a, ParsedArgument<'a>>>, ()> {
        let mut args = Vec::new();
        while !matches!(self.peek(), Some(Node(Token::NewLine, _)) | None) {
            args.push(self.parse_argument().ok_or(())?);
            match self.peek() {
                Some(Node(Token::Comma, _)) => {
                    self.next();
                }
                Some(Node(Token::NewLine, _)) | None => {}
                Some(n) => {
                    self.context
                        .context
                        .borrow_mut()
                        .report_error(n.map(|t| format!("Expected comma found '{t:?}'")));
                }
            }
        }
        Ok(args)
    }

    fn assemble_mnemonic(&mut self, mnemonic: &'a str, n: NodeId<'a>) {
        let Ok(args) = self.gather_arguments(mnemonic, n) else {
            return;
        };
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
                    todo!("{args:?}")
                }
            },
            ".org" => {
                let mut section = self.context.get_current_section();
                if section.start.is_some() {
                    self.context.context.borrow_mut().report_warning(Node(
                        format!(
                            "Section '{}' has already had org set",
                            self.context.current_section
                        ),
                        n,
                    ));
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
                                .report_warning(Node("Conversion to u32 is lossy", n));
                        }
                        ConvertResult::Failure => {
                            self.context
                                .context
                                .borrow_mut()
                                .report_error(Node("Argument not convertable to u32", n));
                        }
                    },
                    rem => {
                        todo!("{args:?}")
                    }
                }
            }

            _ => self
                .context
                .context
                .borrow_mut()
                .report_error(Node(format!("Unrecognized mnemonic '{mnemonic}'"), n)),
        }
    }

    fn assemble_line(&mut self) {
        match self.next() {
            Some(Node(Token::Ident(ident), n)) => {
                self.assemble_mnemonic(ident, n);
                loop {
                    match self.peek() {
                        None | Some(Node(Token::NewLine, _)) => break,
                        Some(unexpected) => self.context.context.borrow_mut().report_error(
                            unexpected.map(|t| format!("Unexpected token '{t:?}' at end of line")),
                        ),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.context.add_label(label, source);
            }
            Some(unexpected) => self.context.context.borrow_mut().report_error(
                unexpected.map(|t| format!("Unexpected token {t:?} expected identifier or label")),
            ),
            None => {}
        }
    }
}
