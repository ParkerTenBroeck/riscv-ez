pub mod context;
mod expression;
pub mod instructions;
pub mod translation;

use std::{cell::RefCell, rc::Rc};

use crate::assembler::expression::{
    ArgumentsTypeHint,
};
use crate::{
    assembler::{context::AssemblerContext, translation::Section},
    context::{Context, Node, NodeId},
    lex::Token,
    preprocess::PreProcessor,
};

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
        let args = self.parse_arguments(ArgumentsTypeHint::None, None);
        let args_node = self
            .context
            .context
            .merge_nodes(start.unwrap().1, self.last.unwrap().1);
        self.context
            .context
            .report_error(args_node, format!("{args:#?}"));

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
            ".f64" => {}

            // ".section" => match &args[..] {
            //     [Node(ParsedArgument::Constant(Constant::String(str)), _)] => {
            //         self.context.set_current_section(str);
            //     }
            //     rem => {
            //         self.context
            //             .context
            //             .borrow_mut()
            //             .report_error(args_node, "Invalid arguments {rem:?}");
            //     }
            // },
            // ".org" => {
            //     let mut section = self.context.get_current_section();
            //     if section.start.is_some() {
            //         self.context.context.borrow_mut().report_warning(
            //             n,
            //             format!(
            //                 "Section '{}' has already had org set",
            //                 self.context.current_section
            //             ),
            //         );
            //         section = self.context.get_current_section();
            //     }
            //     match &args[..] {
            //         [Node(ParsedArgument::Constant(constant), n)] => match constant.to_u32() {
            //             ConvertResult::Success(value) => section.start = Some(value),
            //             ConvertResult::Lossy(value) => {
            //                 section.start = Some(value);
            //                 self.context
            //                     .context
            //                     .borrow_mut()
            //                     .report_warning(n, "Conversion to u32 is lossy");
            //             }
            //             ConvertResult::Failure => {
            //                 self.context
            //                     .context
            //                     .borrow_mut()
            //                     .report_error(n, "Argument not convertable to u32");
            //             }
            //         },
            //         rem => {
            //             self.context
            //                 .context
            //                 .borrow_mut()
            //                 .report_error(args_node, "Invalid arguments {rem:?}");
            //         }
            //     }
            // }
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
