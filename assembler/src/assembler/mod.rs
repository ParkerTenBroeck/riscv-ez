pub mod instructions;
pub mod translation;

use std::{cell::RefCell, rc::Rc};

use crate::{
    assembler::{instructions::{Instruction, Register}, translation::{Label, Section, TranslationUnit}},
    context::{Context, Node, NodeId},
    error::{ErrorKind, FormattedError},
    lex::{Number, Token},
    preprocess::PreProcessor,
};

pub struct AssemblerContext<'a>{
    context: Rc<RefCell<Context<'a>>>,
    current_section: &'a str,
}

pub struct Assembler<'a> {
    context: AssemblerContext<'a>,

    preprocessor: PreProcessor<'a>,
    peek: Option<Node<Token<'a>>>,
    tu: TranslationUnit<'a, NodeId, Instruction<'a>>,
}

pub enum Argument<'a> {
    Number(Number<'a>),
    Ident(&'a str),
    Register(Register),
    Expression,
}

impl<'a> Assembler<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>, preprocessor: PreProcessor<'a>) -> Self {
        Self {
            context: AssemblerContext { context, current_section: "text" },
            preprocessor,
            tu: Default::default(),
            peek: None,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> Vec<u8> {
        self.preprocessor.begin(path);
        self.tu.sections.insert(
            "text",
            Section {
                name: "text",
                start: 0x4000,
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

    fn next(&mut self) -> Option<Node<Token<'a>>> {
        self.peek.take().or_else(|| self.preprocessor.next())
    }

    fn peek(&mut self) -> Option<Node<Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.preprocessor.next();
        }
        self.peek
    }

    fn assemble_mnemonic(&mut self, mnemonic: &'a str, n: NodeId){
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

            ".section" => {

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
                if let Some(previous) = self.tu.labels.get(label) {
                    self.context.context.borrow_mut().report(|ctx| {
                        FormattedError::default()
                            .add(ctx, source, ErrorKind::Error, "Label bound more than once")
                            .add(ctx, previous.source, ErrorKind::Info, "First bound here")
                    });
                    return;
                }
                self.tu.labels.insert(
                    label,
                    Label {
                        source,
                        section: self.context.current_section,
                        offset: self.tu
                            .sections
                            .get(self.context.current_section)
                            .map(|s| s.size)
                            .unwrap_or(0),
                        size: 0,
                    },
                );
            }
            Some(unexpected) => self.context.context.borrow_mut().report_error(
                unexpected.map(|t| format!("Unexpected token {t:?} expected identifier or label")),
            ),
            None => {}
        }
    }
}
