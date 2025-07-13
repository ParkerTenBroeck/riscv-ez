pub mod defined_macro;
pub mod file;
pub mod if_else;

use std::collections::HashMap;
use std::collections::VecDeque;

use crate::assembler::Assembler;
use crate::assembler::context::AssemblerState;
use crate::assembler::lang::AssemblyLanguage;
use crate::expression::ExpressionEvaluatorContext;
use crate::expression::ValueType;
use crate::preprocess::defined_macro::TokenIter;
use crate::preprocess::file::FileIter;
use crate::preprocess::if_else::IfDef;
use crate::{
    context::{Node, NodeId, SourceId},
    lex::{Lexer, Token},
};

struct ProducerStage<'a, T: AssemblyLanguage<'a>> {
    iter: Box<dyn PreProcessorIter<'a, T> + 'a>,
    source: Option<NodeId<'a>>,
}

pub trait PreProcessorIter<'a, T: AssemblyLanguage<'a>> {
    fn next(
        &mut self,
        pp: &mut PreProcessor<'a, T>,
        state: &mut AssemblerState<'a, T>,
    ) -> Option<Node<'a, Token<'a>>>;
}

struct FilterStage<'a, T: AssemblyLanguage<'a>> {
    filter: Box<dyn PreProcessorFilter<'a, T> + 'a>,
    source: Option<NodeId<'a>>,
}

pub enum FilterResult<'a> {
    Pass {
        remove: bool,
        token: Option<Node<'a, Token<'a>>>,
    },
    Consume {
        remove: bool,
    },
}

pub trait PreProcessorFilter<'a, T: AssemblyLanguage<'a>> {
    fn filter(
        &mut self,
        pp: &mut PreProcessor<'a, T>,
        state: &mut AssemblerState<'a, T>,
        token: Option<Node<'a, Token<'a>>>,
    ) -> FilterResult<'a>;
}

pub struct PreProcessor<'a, T: AssemblyLanguage<'a>> {
    producers: Vec<ProducerStage<'a, T>>,
    filters: VecDeque<FilterStage<'a, T>>,
    defines: HashMap<&'a str, Vec<Node<'a, Token<'a>>>>,

    peek: Option<Node<'a, Token<'a>>>,
}

impl<'a, T: AssemblyLanguage<'a>> PreProcessor<'a, T> {
    pub fn new() -> Self {
        Self {
            producers: Vec::new(),
            filters: VecDeque::new(),
            defines: HashMap::new(),

            peek: None,
        }
    }

    fn add_producer(&mut self, state: &mut AssemblerState<'a, T>, stage: ProducerStage<'a, T>) {
        if self.producers.len() > state.context.config().producer_stack_limit {
            if let Some(source) = stage.source {
                state.context.report_error(
                    source,
                    format!(
                        "Preprocessor stack recursion limit hit ({}) for producers",
                        state.context.config().producer_stack_limit
                    ),
                );
            } else {
                state.context.report_error_locless(format!(
                    "Preprocessor stack recursion limit hit ({}) for producers",
                    state.context.config().producer_stack_limit
                ));
            }
            self.producers.clear();
        } else {
            self.producers.push(stage);
        }
    }

    fn add_filter(&mut self, state: &mut AssemblerState<'a, T>, stage: FilterStage<'a, T>) {
        if self.filters.len() > state.context.config().filter_stack_limit {
            if let Some(source) = stage.source {
                state.context.report_error(
                    source,
                    format!(
                        "Preprocessor stack recursion limit hit ({}) for filters",
                        state.context.config().filter_stack_limit
                    ),
                );
            } else {
                state.context.report_error_locless(format!(
                    "Preprocessor stack recursion limit hit ({}) for filters",
                    state.context.config().filter_stack_limit
                ));
            }
        } else {
            self.filters.push_back(stage);
        }
    }

    pub fn begin(
        &mut self,
        state: &mut AssemblerState<'a, T>,
        path: impl Into<String>,
    ) -> Option<SourceId<'a>> {
        self.defines.clear();
        self.filters.clear();
        self.producers.clear();
        self.peek = None;

        let result = state.context.get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_producer(
                    state,
                    ProducerStage {
                        iter: Box::new(FileIter {
                            lex: Lexer::new(src.contents),
                            include_location: None,
                            source: src,
                        }),
                        source: None,
                    },
                );
                Some(src)
            }
            Err(error) => {
                state
                    .context
                    .report_error_locless(format!("Failed to load file '{error}'"));

                None
            }
        }
    }

    pub fn include(
        &mut self,
        state: &mut AssemblerState<'a, T>,
        path: impl Into<String>,
        source: NodeId<'a>,
    ) {
        let result = state.context.get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_producer(
                    state,
                    ProducerStage {
                        iter: Box::new(FileIter {
                            lex: Lexer::new(src.contents),
                            include_location: Some(source),
                            source: src,
                        }),
                        source: Some(source),
                    },
                );
            }
            Err(error) => {
                state
                    .context
                    .report_error(source, format!("Failed to include file '{error}'"));
            }
        }
    }

    fn stack_next(&mut self, state: &mut AssemblerState<'a, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        while let Some(mut top) = self.producers.pop() {
            if let Some(next) = top.iter.next(self, state) {
                self.producers.push(top);
                return Some(next);
            }
        }
        None
    }

    fn handle_preprocessor_tag(
        &mut self,
        state: &mut AssemblerState<'a, T>,
        tag: &'a str,
        n: NodeId<'a>,
    ) {
        match tag {
            "include" => match self.stack_next(state) {
                Some(Node(Token::StringLiteral(str), node)) => self.include(state, str, node),
                t => {
                    _ = state
                        .context
                        .unexpected_token(t, Token::StringLiteral(""), false)
                }
            },
            "ifdef" => match self.stack_next(state) {
                Some(Node(Token::Ident(str), node)) => {
                    self.add_filter(
                        state,
                        FilterStage {
                            filter: Box::new(IfDef {
                                source: node,
                                condition: self.defines.contains_key(str),
                                else_loc: None,
                            }),
                            source: Some(node),
                        },
                    );
                }
                t => _ = state.context.unexpected_token(t, Token::Ident(""), false),
            },
            "ifndef" => match self.stack_next(state) {
                Some(Node(Token::Ident(str), node)) => {
                    self.add_filter(
                        state,
                        FilterStage {
                            filter: Box::new(IfDef {
                                source: node,
                                condition: !self.defines.contains_key(str),
                                else_loc: None,
                            }),
                            source: Some(node),
                        },
                    );
                }
                t => _ = state.context.unexpected_token(t, Token::Ident(""), false),
            },
            "if" => {
                let res = Assembler::new(state, self).expr(ValueType::Any);
                self.add_filter(
                    state,
                    FilterStage {
                        filter: Box::new(IfDef {
                            source: res.1,
                            condition: res.0.is_true(),
                            else_loc: None,
                        }),
                        source: Some(res.1),
                    },
                );
            }
            "define" => {
                let ident = match self.stack_next(state) {
                    Some(Node(Token::Ident(str), _)) => str,
                    t => {
                        state.context.unexpected_token(t, Token::Ident(""), false);
                        ""
                    }
                };
                let mut toks = Vec::new();
                loop {
                    match self.stack_next(state) {
                        Some(Node(Token::NewLine, _)) | None => break,
                        Some(tok) => toks.push(tok),
                    }
                }
                if !ident.is_empty() {
                    self.defines.insert(ident, toks);
                }
            }

            unknown => {
                state
                    .context
                    .report_error(n, format!("Unknown preprocessor tag '{unknown}'"));
            }
        }
    }

    fn handle_identifier(
        &mut self,
        state: &mut AssemblerState<'a, T>,
        ident: &'a str,
        n: NodeId<'a>,
    ) -> bool {
        if let Some(value) = self.defines.get(ident) {
            self.add_producer(
                state,
                ProducerStage {
                    iter: Box::new(TokenIter {
                        toks: value.clone().into_iter(),
                        source: n,
                    }),
                    source: Some(n),
                },
            );
            return false;
        }
        true
    }

    fn next_filtered(&mut self, state: &mut AssemblerState<'a, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        loop {
            let mut next = self.stack_next(state);
            let mut filters = VecDeque::new();
            std::mem::swap(&mut filters, &mut self.filters);
            let mut contin = false;
            filters.retain_mut(|f| {
                if contin {
                    return true;
                }
                match f.filter.filter(self, state, next) {
                    FilterResult::Pass { remove, token } => {
                        next = token;
                        !remove
                    }
                    FilterResult::Consume { remove } => {
                        contin = true;
                        !remove
                    }
                }
            });
            filters.append(&mut self.filters);
            self.filters = filters;
            if contin {
                continue;
            }

            match next {
                Some(Node(Token::PreProcessorTag(tag), n)) => {
                    self.handle_preprocessor_tag(state, tag, n)
                }
                t @ Some(Node(Token::Ident(ident), n)) => {
                    if self.handle_identifier(state, ident, n) {
                        return t;
                    }
                }
                other => return other,
            }
        }
    }

    pub fn peek(&mut self, state: &mut AssemblerState<'a, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next(state);
        }
        self.peek
    }

    pub fn next(&mut self, state: &mut AssemblerState<'a, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next_filtered(state);
        }
        self.peek.take()
    }
}
