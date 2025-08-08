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
use crate::logs::LogEntry;
use crate::preprocess::if_else::IfDef;
use crate::{
    context::{Node, NodeId, SourceId},
    lex::{Lexer, Token},
};

#[derive(Clone, Copy)]
pub struct SrcSlice<'a> {
    pub contents: &'a [Spanned<Token<'a>>],
    pub src: SourceId<'a>,
}

impl<'a> SrcSlice<'a> {
    pub fn next(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        let (first, rem) = self.contents.split_first()?;
        self.contents = rem;
        Some(first)
    }

    pub fn peek(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        Some(self.contents.split_first()?.0)
    }

    pub fn make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        parent: Parent<'a>,
    ) -> Option<Node<'a, Token<'a>>> {
        let token = self.next()?;
        Some(Node(
            token.val,
            ctx.context.node(NodeInfo {
                span: token.span,
                source: self.src,
                parent,
            }),
        ))
    }

    pub fn peek_make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        parent: Parent<'a>,
    ) -> Option<Node<'a, Token<'a>>> {
        let token = self.peek()?;
        Some(Node(
            token.val,
            ctx.context.node(NodeInfo {
                span: token.span,
                source: self.src,
                parent,
            }),
        ))
    }

    pub fn new(contents: &'a [Spanned<Token<'a>>], src: SourceId<'a>) -> Self {
        Self { contents, src }
    }
}

pub struct ProducerStage<'a> {
    pub contents: SrcSlice<'a>,
    pub parent: Parent<'a>,
}
impl<'a> ProducerStage<'a> {
    pub fn next(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        self.contents.next()
    }

    pub fn peek(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        self.contents.peek()
    }

    pub fn next_make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        self.contents.make(ctx, self.parent)
    }

    pub fn peek_make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        self.contents.peek_make(ctx, self.parent)
    }
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

pub struct MacroDef<'a> {
    definition: NodeId<'a>,
    contents: SrcSlice<'a>,
}

pub struct PreProcessor<'a, T: AssemblyLanguage<'a>> {
    producers: Vec<ProducerStage<'a>>,
    filters: VecDeque<FilterStage<'a, T>>,
    macros: HashMap<&'a str, MacroDef<'a>>,

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
            macros: HashMap::new(),

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
        self.macros.clear();
        self.filters.clear();
        self.producers.clear();
        self.peek = None;

        self.include(&mut ctx, path, Parent::None)
    }

    pub fn top_parent(&mut self) -> Parent<'a>{
        self.producers.last().map(|p|p.parent).unwrap_or(Parent::None)
    }

    pub fn parse_block(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> SrcSlice<'a> {
        let Some(top) = self.producers.last_mut() else {
            return SrcSlice::new(&[], ctx.context.eof().source);
        };

        let first = top.next_make(ctx);
        match first {
            Some(Node(Token::LBrace, _)) => {}
            got => {
                ctx.context.unexpected_token(got, Token::LBrace, false);
            }
        }

        let slice = top.contents.contents;
        let mut size = 0;
        let mut indent = 0usize;
        loop {
            match top.next() {
                Some(Spanned {
                    val: Token::LBrace, ..
                }) => indent += 1,
                Some(Spanned {
                    val: Token::RBrace, ..
                }) if indent == 0 => break,
                Some(Spanned {
                    val: Token::RBrace, ..
                }) => indent -= 1,
                None => {
                    let node = ctx.context.node(NodeInfo {
                        span: top.contents.src.eof(),
                        source: top.contents.src,
                        parent: top.parent,
                    });
                    ctx.context.report_error(node, "unclosed block");
                    break;
                }
                _ => {}
            }
            size += 1;
        }

        SrcSlice::new(&slice[..size], top.contents.src)
    }

    fn parse_file(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        path: impl Into<String>,
        parent: Parent<'a>,
    ) -> Option<SrcSlice<'a>> {
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
                Some(SrcSlice::new(
                    ctx.context.alloc_slice(tokens.as_slice()),
                    src,
                ))
            }
            Err(error) => {
                if let Some(source) = parent.parent() {
                    ctx.context
                        .report_error(source, format!("failed to load '{path}': {error}"));
                } else {
                    ctx.context
                        .report_error_locless(format!("failed to load '{path}': {error}"));
                }
                None
            }
        }
    }

    pub fn include(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        path: impl Into<String>,
        parent: Parent<'a>,
    ) -> Option<SourceId<'a>> {
        let src = self.parse_file(ctx, path, parent)?;
        self.add_producer(
            ctx,
            ProducerStage {
                contents: src,
                parent,
            },
        );
        Some(src.src)
    }

    fn stack_next(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        while let Some(top) = self.producers.last_mut() {
            if let Some(next) = top.next_make(ctx) {
                return Some(next);
            } else {
                self.producers.pop();
            }
        }
        None
    }

    fn parse_if(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>, n: NodeId<'a>) {
        let res = ctx.eval(self).expr(ValueType::Bool);

        {
let parent = self.top_parent();
        let contents = self.parse_block(ctx);
        if res.0.is_true(){
            self.producers.push(ProducerStage { contents, parent })
        }
        }
        
        let mut produced = res.0.is_true();
        let mut else_encountered = None;
        loop{
            match self.peek{
                Some(Node(Token::PreProcessorTag("else"), n)) => {
                    let parent = self.top_parent();
                    let contents = self.parse_block(ctx);
                    if let Some(other) = else_encountered{
                        ctx.context.report(LogEntry::new().error(n, "else can only appear as the last block in if chain").info(other, "first encounter here"));
                    }else{
                        if !produced{
                            self.producers.push(ProducerStage { contents, parent });
                        }
                        else_encountered = Some(n);
                    }
                }
                Some(Node(Token::PreProcessorTag("elseif"), _)) => {
                    let res = ctx.eval(self).expr(ValueType::Bool);
                    let parent = self.top_parent();
                    let contents = self.parse_block(ctx);
                    if let Some(other) = else_encountered{
                        ctx.context.report(LogEntry::new().error(n, "else can only appear as the last block in if chain").info(other, "first encounter here"));
                    }else if res.0.is_true(){
                        produced = true;
                        self.producers.push(ProducerStage { contents, parent });
                    }
                }
                _ => break,
            }
        }
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
                }
                t => {
                    _ = ctx
                        .context
                        .unexpected_token(t, Token::StringLiteral(""), false)
                }
            },
            "def" => match self.stack_next(ctx) {
                Some(Node(Token::Ident(str), node)) => {
                    self.add_filter(
                        ctx,
                        FilterStage {
                            filter: Box::new(IfDef {
                                source: node,
                                condition: !self.macros.contains_key(str),
                                else_loc: None,
                            }),
                            source: Some(node),
                        },
                    );
                }
                t => _ = ctx.context.unexpected_token(t, Token::Ident(""), false),
            },
            "if" => self.parse_if(ctx, n),
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
            "macro" => {
                let (ident, definition) = match self.stack_next(ctx) {
                    Some(Node(Token::Ident(str), node)) => (str, node),
                    t => {
                        ctx.context.unexpected_token(t, Token::Ident(""), false);
                        ("", ctx.context.eof())
                    }
                };

                let block = self.parse_block(ctx);
                if !ident.is_empty() {
                    self.macros.insert(
                        ident,
                        MacroDef {
                            definition,
                            contents: block,
                        },
                    );
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
        if let Some(value) = self.macros.get(ident) {
            self.add_producer(
                ctx,
                ProducerStage {
                    contents: value.contents,
                    parent: Parent::Pasted {
                        parent: n,
                        definition: Some(value.definition),
                    },
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
