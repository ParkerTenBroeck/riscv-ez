pub mod context;
pub mod instructions;
mod mnemonics;
pub mod translation;

use crate::expression::ExpressionEvaluatorContext;
use crate::{
    assembler::{context::AssemblerContext, translation::Section},
    context::{Context, Node},
    lex::Token,
    preprocess::PreProcessor,
};
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

    fn expected_args<A, B>(&mut self) -> (A, B) {
        todo!()
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
                            .report_error(n, format!("Unexpected token '{t:#}' at end of line")),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.context.add_label(label, source);
            }
            Some(Node(t, n)) => self.context.context.report_error(
                n,
                format!("Unexpected token {t:#} expected mnemonic or label"),
            ),
            None => {}
        }
    }
}

impl<'a> ExpressionEvaluatorContext<'a> for Assembler<'a> {
    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.next()
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.peek()
    }

    fn context(&self) -> &Context<'a> {
        &self.context.context
    }
}
