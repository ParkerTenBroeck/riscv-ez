pub mod context;
pub mod instructions;
mod mnemonics;
pub mod translation;

use crate::assembler::instructions::Register;
use crate::context::NodeId;
use crate::expression::{ExpressionEvaluatorContext, LabelUse, Value, ValueType};
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

    fn handle_ident(
        &mut self,
        ident: &'a str,
        node: NodeId<'a>,
        _: ValueType,
    ) -> Node<'a, Value<'a>> {
        fn reg<'a>(node: NodeId<'a>, reg: u8) -> Node<'a, Value<'a>> {
            Node(Value::Register(Register(reg)), node)
        }

        match ident {
            "x0" | "zero" => reg(node, 0),
            "x1" | "ra" => reg(node, 1),
            "x2" | "sp" => reg(node, 2),
            "x3" | "gp" => reg(node, 3),
            "x4" | "tp" => reg(node, 4),
            "x5" | "t0" => reg(node, 5),
            "x6" | "t1" => reg(node, 6),
            "x7" | "t2" => reg(node, 7),
            "x8" | "s0" | "fp" => reg(node, 8),
            "x9" | "s1" => reg(node, 9),
            "x10" | "a0" => reg(node, 10),
            "x11" | "a1" => reg(node, 11),
            "x12" | "a2" => reg(node, 12),
            "x13" | "a3" => reg(node, 13),
            "x14" | "a4" => reg(node, 14),
            "x15" | "a5" => reg(node, 15),
            "x16" | "a6" => reg(node, 16),
            "x17" | "a7" => reg(node, 17),
            "x18" | "s2" => reg(node, 18),
            "x19" | "s3" => reg(node, 19),
            "x20" | "s4" => reg(node, 20),
            "x21" | "s5" => reg(node, 21),
            "x22" | "s6" => reg(node, 22),
            "x23" | "s7" => reg(node, 23),
            "x24" | "s8" => reg(node, 24),
            "x25" | "s9" => reg(node, 25),
            "x26" | "s10" => reg(node, 26),
            "x27" | "s11" => reg(node, 27),
            "x28" | "t3" => reg(node, 28),
            "x29" | "t4" => reg(node, 29),
            "x30" | "t5" => reg(node, 30),
            "x31" | "t6" => reg(node, 31),

            "f0" => reg(node, 32),
            "f1" => reg(node, 32 + 1),
            "f2" => reg(node, 32 + 2),
            "f3" => reg(node, 32 + 3),
            "f4" => reg(node, 32 + 4),
            "f5" => reg(node, 32 + 5),
            "f6" => reg(node, 32 + 6),
            "f7" => reg(node, 32 + 7),
            "f8" => reg(node, 32 + 8),
            "f9" => reg(node, 32 + 9),
            "f10" => reg(node, 32 + 10),
            "f11" => reg(node, 32 + 11),
            "f12" => reg(node, 32 + 12),
            "f13" => reg(node, 32 + 13),
            "f14" => reg(node, 32 + 14),
            "f15" => reg(node, 32 + 15),
            "f16" => reg(node, 32 + 16),
            "f17" => reg(node, 32 + 17),
            "f18" => reg(node, 32 + 18),
            "f19" => reg(node, 32 + 19),
            "f20" => reg(node, 32 + 20),
            "f21" => reg(node, 32 + 21),
            "f22" => reg(node, 32 + 22),
            "f23" => reg(node, 32 + 23),
            "f24" => reg(node, 32 + 24),
            "f25" => reg(node, 32 + 25),
            "f26" => reg(node, 32 + 26),
            "f27" => reg(node, 32 + 27),
            "f28" => reg(node, 32 + 28),
            "f29" => reg(node, 32 + 29),
            "f30" => reg(node, 32 + 30),
            "f31" => reg(node, 32 + 31),

            _ => Node(Value::Label(LabelUse::new(ident)), node),
        }
    }
}
