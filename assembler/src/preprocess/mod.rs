pub mod defined_macro;
pub mod file;
pub mod if_else;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Write;

use crate::assembler::PreProcessorCtx;
use crate::assembler::lang::AssemblyLanguage;
use crate::context::NodeInfo;
use crate::context::Parent;
use crate::expression::Value;
use crate::expression::ValueType;
use crate::lex::Spanned;
use crate::preprocess::defined_macro::TokenIter;
use crate::preprocess::if_else::IfDef;
use crate::{
    context::{Node, NodeId, SourceId},
    lex::{Lexer, Token},
};

struct SrcSlice<'a>{
    contents: &'a [Spanned<Token<'a>>],
    src: SourceId<'a>,
}

impl<'a> SrcSlice<'a>{
    pub fn next(&mut self) -> Option<&'a Spanned<Token>>{
        let(first, rem) = self.contents.split_first()?;
        self.contents = rem;
        Some(first)
    }
}

struct ProducerStage<'a> {
    src: SrcSlice<'a>,
    parent: Parent<'a>,
}

pub trait PreProcessorIter<'a, T: AssemblyLanguage<'a>> {
    fn next(
        &mut self,
        pp: &mut PreProcessor<'a, T>,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
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
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        token: Option<Node<'a, Token<'a>>>,
    ) -> FilterResult<'a>;
}

pub struct PreProcessor<'a, T: AssemblyLanguage<'a>> {
    producers: Vec<ProducerStage<'a>>,
    filters: VecDeque<FilterStage<'a, T>>,
    defines: HashMap<&'a str, &'a [Spanned<Token<'a>>]>,

    peek: Option<Node<'a, Token<'a>>>,
}

impl<'a, T: AssemblyLanguage<'a>> Default for PreProcessor<'a, T> {
    fn default() -> Self {
        Self::new()
    }
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

    fn add_producer(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>, stage: ProducerStage<'a>) {
        let limit = ctx.context.config().producer_stack_limit;
        if self.producers.len() > limit {
            if let Some(source) = stage.parent.parent() {
                ctx.context.report_error(
                    source,
                    format!("preprocessor stack recursion limit hit ({limit}) for producers",),
                );
            } else {
                ctx.context.report_error_locless(format!(
                    "preprocessor stack recursion limit hit ({limit}) for producers"
                ));
            }
            self.producers.clear();
        } else {
            self.producers.push(stage);
        }
    }

    fn add_filter(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>, stage: FilterStage<'a, T>) {
        let limit = ctx.context.config().filter_stack_limit;
        if self.filters.len() > limit {
            if let Some(source) = stage.source {
                ctx.context.report_error(
                    source,
                    format!("preprocessor stack recursion limit hit ({limit}) for filters"),
                );
            } else {
                ctx.context.report_error_locless(format!(
                    "preprocessor stack recursion limit hit ({limit}) for filters"
                ));
            }
        } else {
            self.filters.push_back(stage);
        }
    }

    pub fn begin(
        &mut self,
        mut ctx: PreProcessorCtx<'a, '_, T>,
        path: impl Into<String>,
    ) -> Option<SourceId<'a>> {
        self.defines.clear();
        self.filters.clear();
        self.producers.clear();
        self.peek = None;

        self.include(&mut ctx, path, Parent::None)
    }

    fn parse_file(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        path: impl Into<String>,
        parent: Parent<'a>,
    ) -> SrcSlice<'a> {
        let path = path.into();
        let result = ctx.context.get_source_from_path(&path);
        match result {
            Ok(src) => {
                let mut tokens = Vec::new();
                for token in Lexer::new(src.contents).include_comments() {
                    match token {
                        Ok(token) => tokens.push(token),
                        Err(err) => {
                            ctx.context.report_error(
                                ctx.context.node(NodeInfo {
                                    span: err.span,
                                    source: src,
                                    parent,
                                }),
                                format!("lexer error: {:?}", err.val),
                            );
                        }
                    }
                }
                (ctx.context.alloc_slice(tokens.as_slice()), Some(src))
            }
            Err(error) => {
                if let Some(source) = parent.parent() {
                    ctx.context
                        .report_error(source, format!("failed to load '{path}': {error}"));
                } else {
                    ctx.context
                        .report_error_locless(format!("failed to load '{path}': {error}"));
                }
                (&[], None)
            }
        }
    }

    pub fn include(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        path: impl Into<String>,
        parent: Parent<'a>,
    ) -> Option<SourceId<'a>> {
        let (iter, src) = self.parse_file(ctx, path, parent);
        let src = src?;
        self.add_producer(
            ctx,
            ProducerStage {
                iter: iter.iter(),
                src,
                parent,
            },
        );
        Some(src)
    }

    fn stack_next(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        while let Some(mut top) = self.producers.pop() {
            if let Some(next) = top.iter.next() {
                self.producers.push(top);
                let node = ctx.context.node(NodeInfo { span: next.span, source: top.src, parent: top.parent });
                return Some(Node(next.val, node));
            }
        }
        None
    }

    fn handle_preprocessor_tag(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        tag: &'a str,
        n: NodeId<'a>,
    ) -> Option<Node<'a, Token<'a>>> {
        match tag {
            "include" => match self.stack_next(ctx) {
                Some(Node(Token::StringLiteral(str), node)) => {
                    self.include(ctx, str, Parent::Included { parent: node });
                },
                t => {
                    _ = ctx
                        .context
                        .unexpected_token(t, Token::StringLiteral(""), false)
                }
            },
            "ifdef" => match self.stack_next(ctx) {
                Some(Node(Token::Ident(str), node)) => {
                    self.add_filter(
                        ctx,
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
                t => _ = ctx.context.unexpected_token(t, Token::Ident(""), false),
            },
            "ifndef" => match self.stack_next(ctx) {
                Some(Node(Token::Ident(str), node)) => {
                    self.add_filter(
                        ctx,
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
                t => _ = ctx.context.unexpected_token(t, Token::Ident(""), false),
            },
            "if" => {
                let res = ctx.eval(self).expr(ValueType::Any);
                self.add_filter(
                    ctx,
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
            "concat" => {
                let Node(parts, node): Node<'_, Vec<Value<'a, T>>> =
                    ctx.eval(self).coerced_delim(n, Token::LPar, Token::RPar);
                let mut ident = String::new();
                for part in parts.into_iter() {
                    ident.write_fmt(format_args!("{part}")).unwrap();
                }
                let ident = ctx.context.alloc_str(ident);
                return Some(Node(Token::Ident(ident), node));
            }
            "define" => {
                let ident = match self.stack_next(ctx) {
                    Some(Node(Token::Ident(str), _)) => str,
                    t => {
                        ctx.context.unexpected_token(t, Token::Ident(""), false);
                        ""
                    }
                };
                let mut toks = Vec::new();
                loop {
                    match self.stack_next(ctx) {
                        Some(Node(Token::NewLine, _)) | None => break,
                        Some(tok) => toks.push(tok),
                    }
                }
                if !ident.is_empty() {
                    self.defines.insert(ident, toks);
                }
            }

            unknown => {
                ctx.context
                    .report_error(n, format!("unknown preprocessor tag '{unknown}'"));
            }
        }
        None
    }

    fn handle_identifier(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        ident: &'a str,
        n: NodeId<'a>,
    ) -> bool {
        if let Some(value) = self.defines.get(ident) {
            self.add_producer(
                ctx,
                ProducerStage {
                    iter: Box::new(TokenIter {
                        toks: value.clone().into_iter(),
                        source: n,
                    }),
                    parent: Some(n),
                },
            );
            return false;
        }
        true
    }

    fn next_filtered(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        loop {
            let mut next = self.stack_next(ctx);
            let mut filters = VecDeque::new();
            std::mem::swap(&mut filters, &mut self.filters);
            let mut contin = false;
            filters.retain_mut(|f| {
                if contin {
                    return true;
                }
                match f.filter.filter(self, ctx, next) {
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
                    if let Some(tok) = self.handle_preprocessor_tag(ctx, tag, n) {
                        return Some(tok);
                    }
                }
                t @ Some(Node(Token::Ident(ident), n)) => {
                    if self.handle_identifier(ctx, ident, n) {
                        return t;
                    }
                }
                other => return other,
            }
        }
    }

    pub fn peek(&mut self, ctx: PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next(ctx);
        }
        self.peek
    }

    pub fn next(&mut self, mut ctx: PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next_filtered(&mut ctx);
        }
        self.peek.take()
    }
}
