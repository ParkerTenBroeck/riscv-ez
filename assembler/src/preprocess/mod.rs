use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::context::NodeInfo;
use crate::{
    context::{Context, Node, NodeId, SourceId},
    lex::{Lexer, Token},
};

struct TokenIter<'a> {
    toks: std::vec::IntoIter<Node<'a, Token<'a>>>,
    source: NodeId<'a>,
}

impl<'a> PreProcessorIter<'a> for TokenIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>> {
        let tok = self.toks.next()?;

        Some(Node(
            tok.0,
            pp.context.node(NodeInfo {
                span: tok.1.span,
                source: tok.1.source,
                included_by: tok.1.included_by,
                invoked_by: Some(self.source),
            }),
        ))
    }
}

struct FileIter<'a> {
    lex: Lexer<'a>,
    source: SourceId<'a>,
    include_location: Option<NodeId<'a>>,
}

impl<'a> PreProcessorIter<'a> for FileIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>> {
        for token in self.lex.by_ref() {
            match token {
                Ok(ok) => {
                    return Some(Node(
                        ok.val,
                        pp.context.node(NodeInfo {
                            span: ok.span,
                            source: self.source,
                            included_by: self.include_location,
                            invoked_by: None,
                        }),
                    ));
                }
                Err(err) => {
                    let n = pp.context.node(NodeInfo {
                        span: err.span,
                        source: self.source,
                        included_by: self.include_location,
                        invoked_by: None,
                    });
                    pp.context.report_error(n, err.val);
                }
            }
        }
        None
    }
}

struct ProducerStage<'a> {
    iter: Box<dyn PreProcessorIter<'a> + 'a>,
    source: Option<NodeId<'a>>,
}

pub trait PreProcessorIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>>;
}

struct FilterStage<'a> {
    iter: Box<dyn PreProcessorFilter<'a> + 'a>,
    source: Option<NodeId<'a>>,
}

pub trait PreProcessorFilter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>>;
}

pub struct PreProcessor<'a> {
    producers: Vec<ProducerStage<'a>>,
    filters: Vec<FilterStage<'a>>,
    context: Rc<Context<'a>>,
    recursion_limit: usize,
    defines: HashMap<&'a str, Vec<Node<'a, Token<'a>>>>,
    line_beginning: bool,
    previous_newline: bool,
}

impl<'a> PreProcessor<'a> {
    pub fn new(info: Rc<Context<'a>>) -> Self {
        Self {
            producers: Vec::new(),
            filters: Vec::new(),
            context: info,
            recursion_limit: 100,
            defines: HashMap::new(),
            line_beginning: true,
            previous_newline: true,
        }
    }

    fn add_producer(&mut self, stage: ProducerStage<'a>) {
        if self.producers.len() > self.recursion_limit {
            if let Some(source) = stage.source {
                self.context.report_error(
                    source,
                    format!(
                        "Preprocessor stack recursion limit hit ({}) for producers",
                        self.recursion_limit
                    ),
                );
            } else {
                self.context.report_error_nodeless(format!(
                    "Preprocessor stack recursion limit hit ({}) for producers",
                    self.recursion_limit
                ));
            }
        } else {
            self.producers.push(stage);
        }
    }

    fn add_filter(&mut self, stage: FilterStage<'a>) {
        if self.filters.len() > self.recursion_limit {
            if let Some(source) = stage.source {
                self.context.report_error(
                    source,
                    format!(
                        "Preprocessor stack recursion limit hit ({}) for filters",
                        self.recursion_limit
                    ),
                );
            } else {
                self.context.report_error_nodeless(format!(
                    "Preprocessor stack recursion limit hit ({}) for filters",
                    self.recursion_limit
                ));
            }
        } else {
            self.filters.push(stage);
        }
    }

    pub fn begin(&mut self, path: impl Into<String>) -> Option<SourceId<'a>> {
        self.defines.clear();
        self.filters.clear();
        self.producers.clear();
        self.line_beginning = true;
        self.previous_newline = true;

        let result = self.context.get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_producer(ProducerStage {
                    iter: Box::new(FileIter {
                        lex: Lexer::new(src.contents),
                        include_location: None,
                        source: src,
                    }),
                    source: None,
                });
                Some(src)
            }
            Err(error) => {
                self.context
                    .report_error_nodeless(format!("Failed to load file '{error}'"));

                None
            }
        }
    }

    pub fn include(&mut self, path: impl Into<String>, source: NodeId<'a>) {
        let result = self.context.get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_producer(ProducerStage {
                    iter: Box::new(FileIter {
                        lex: Lexer::new(src.contents),
                        include_location: Some(source),
                        source: src,
                    }),
                    source: Some(source),
                });
            }
            Err(error) => {
                self.context
                    .report_error(source, format!("Failed to include file '{error}'"));
            }
        }
    }

    fn stack_next(&mut self) -> Option<Node<'a, Token<'a>>> {
        while let Some(mut top) = self.producers.pop() {
            if let Some(next) = top.iter.next(self) {
                self.producers.push(top);
                self.line_beginning = self.previous_newline;
                self.previous_newline = matches!(next, Node(Token::NewLine, _));
                return Some(next);
            }
        }
        None
    }

    fn handle_preprocessor_tag(&mut self, tag: &'a str, n: NodeId<'a>) {
        match tag {
            "include" => match self.stack_next() {
                Some(Node(Token::StringLiteral(str), node)) => self.include(str, node),
                Some(Node(t, n)) => self
                    .context
                    .report_error(n, format!("Expected string found {t:?}")),
                None => self
                    .context
                    .report_error(n, "Expected string but found EOF"),
            },
            "define" => {
                let ident = match self.stack_next() {
                    Some(Node(Token::Ident(str), _)) => str,
                    Some(Node(t, n)) => {
                        self.context
                            .report_error(n, format!("Expected ident found {t:?}"));
                        return;
                    }
                    None => {
                        self.context.report_error(n, "Expected ident but found EOF");
                        return;
                    }
                };
                let mut toks = Vec::new();
                loop {
                    match self.stack_next() {
                        Some(Node(Token::NewLine, _)) | None => break,
                        Some(tok) => toks.push(tok),
                    }
                }
                self.defines.insert(ident, toks);
            }

            unknown => self
                .context
                .report_error(n, format!("Unknown preprocessor tag '{unknown}'")),
        }
    }

    fn handle_identifier(&mut self, ident: &'a str, n: NodeId<'a>) -> bool {
        if let Some(value) = self.defines.get(ident) {
            self.add_producer(ProducerStage {
                iter: Box::new(TokenIter {
                    toks: value.clone().into_iter(),
                    source: n,
                }),
                source: Some(n),
            });
            return false;
        }
        true
    }

    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        loop {
            match self.stack_next() {
                Some(Node(Token::PreProcessorTag(tag), n)) if self.line_beginning => {
                    self.handle_preprocessor_tag(tag, n)
                }
                t @ Some(Node(Token::Ident(ident), n)) => {
                    if self.handle_identifier(ident, n) {
                        return t;
                    }
                }
                other => return other,
            }
        }
    }
}

impl<'a> Iterator for PreProcessor<'a> {
    type Item = Node<'a, Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
