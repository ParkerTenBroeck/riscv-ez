use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    context::{Context, Node, NodeId, SourceId},
    lex::{Lexer, Token},
};

struct Stage<'a> {
    iter: Box<dyn PreProcessorIter<'a> + 'a>,
    source: Option<NodeId<'a>>,
}

pub trait PreProcessorIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>>;
}

impl<'a> Stage<'a> {}

struct TokenIter<'a> {
    toks: std::vec::IntoIter<Node<'a, Token<'a>>>,
    source: NodeId<'a>,
}

impl<'a> PreProcessorIter<'a> for TokenIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>> {
        let tok = self.toks.next()?;

        Some(pp.context.borrow_mut().parent_child(self.source, tok))
    }
}

struct FileIter<'a> {
    lex: Lexer<'a>,
    source_id: SourceId<'a>,
    include_location: Option<NodeId<'a>>,
}

impl<'a> PreProcessorIter<'a> for FileIter<'a> {
    fn next(&mut self, pp: &mut PreProcessor<'a>) -> Option<Node<'a, Token<'a>>> {
        for token in self.lex.by_ref() {
            match token {
                Ok(ok) => {
                    return Some(pp.context.borrow_mut().create_node(
                        self.source_id,
                        ok,
                        self.include_location,
                    ));
                }
                Err(err) => {
                    let node = pp.context.borrow_mut().create_node(
                        self.source_id,
                        *err,
                        self.include_location,
                    );
                    pp.context.borrow_mut().report_error(node);
                }
            }
        }
        None
    }
}

pub struct PreProcessor<'a> {
    stack: Vec<Stage<'a>>,
    context: Rc<RefCell<Context<'a>>>,
    recursion_limit: usize,
    defines: HashMap<&'a str, Vec<Node<'a, Token<'a>>>>,
    line_begining: bool,
    previous_newline: bool,
}

impl<'a> PreProcessor<'a> {
    pub fn new(info: Rc<RefCell<Context<'a>>>) -> Self {
        Self {
            stack: Vec::new(),
            context: info,
            recursion_limit: 10,
            defines: HashMap::new(),
            line_begining: true,
            previous_newline: true,
        }
    }

    fn add_stack(&mut self, stage: Stage<'a>) {
        if self.stack.len() > self.recursion_limit {
            if let Some(source) = stage.source {
                self.context.borrow_mut().report_error(Node(
                    format!(
                        "Preprocessor stack recursion limit hit ({})",
                        self.recursion_limit
                    ),
                    source,
                ));
            } else {
                self.context.borrow_mut().report_error_hard(format!(
                    "Preprocessor stack recursion limit hit ({})",
                    self.recursion_limit
                ));
            }
        } else {
            self.stack.push(stage);
        }
    }

    pub fn begin(&mut self, path: impl Into<String>) {
        let result = self.context.borrow_mut().get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_stack(Stage {
                    iter: Box::new(FileIter {
                        lex: Lexer::new(src.contents),
                        include_location: None,
                        source_id: src,
                    }),
                    source: None,
                });
            }
            Err(error) => {
                self.context
                    .borrow_mut()
                    .report_error_hard(format!("Failed to load file '{error}'"));
            }
        }
    }

    pub fn include(&mut self, path: impl Into<String>, source: NodeId<'a>) {
        let result = self.context.borrow_mut().get_source_from_path(path);
        match result {
            Ok(src) => {
                self.add_stack(Stage {
                    iter: Box::new(FileIter {
                        lex: Lexer::new(src.contents),
                        include_location: Some(source),
                        source_id: src,
                    }),
                    source: Some(source),
                });
            }
            Err(error) => {
                self.context
                    .borrow_mut()
                    .report_error(Node(format!("Failed to include file '{error}'"), source));
            }
        }
    }

    fn stack_next(&mut self) -> Option<Node<'a, Token<'a>>> {
        while let Some(mut top) = self.stack.pop() {
            if let Some(next) = top.iter.next(self) {
                self.stack.push(top);
                self.line_begining = self.previous_newline;
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
                Some(t) => self
                    .context
                    .borrow_mut()
                    .report_error(t.map(|t| format!("Expected string found {t:?}"))),
                None => self
                    .context
                    .borrow_mut()
                    .report_error(Node("Expected string but found EOF", n)),
            },
            "define" => {
                let ident = match self.stack_next() {
                    Some(Node(Token::Ident(str), _)) => str,
                    Some(t) => {
                        self.context
                            .borrow_mut()
                            .report_error(t.map(|t| format!("Expected ident found {t:?}")));
                        return;
                    }
                    None => {
                        self.context
                            .borrow_mut()
                            .report_error(Node("Expected ident but found EOF", n));
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
                .borrow_mut()
                .report_error(Node(format!("Unknown preprocessor tag '{unknown}'"), n)),
        }
    }

    fn handle_identifier(&mut self, ident: &'a str, n: NodeId<'a>) -> bool {
        if let Some(value) = self.defines.get(ident) {
            self.add_stack(Stage {
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
                Some(Node(Token::PreProcessorTag(tag), n)) if self.line_begining => {
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
