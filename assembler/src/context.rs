use super::logs::LogEntry;
use crate::config::AssemblerConfig;
use crate::lex::Span;
use crate::lex::Token;
use bumpalo::Bump;
use std::fmt::Display;
use std::{collections::HashMap, error::Error};

#[derive(Clone, Copy, Debug)]
pub struct Node<'a, T>(pub T, pub NodeId<'a>);

impl<'a, T> Node<'a, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Node<'a, U> {
        Node(f(self.0), self.1)
    }

    pub fn as_ref(&self) -> Node<'a, &T> {
        Node(&self.0, self.1)
    }
}

pub type NodeId<'a> = &'a NodeInfo<'a>;
pub type SourceId<'a> = &'a Source<'a>;

pub type SourceSupplier<'a> =
    Box<dyn Fn(&str, &Context<'a>) -> Result<&'a str, Box<dyn Error>> + 'a>;

#[derive(Clone, Copy)]
pub struct Source<'a> {
    pub path: &'a str,
    pub contents: &'a str,
}

impl<'a> Source<'a> {
    pub fn eof(&self) -> Span {
        Span {
            line: self.contents.lines().count() as u32,
            col: self.contents.lines().last().unwrap_or("").len() as u32,
            offset: (self.contents.len() as u32),
            len: 1,
        }
    }
}

impl<'a> std::fmt::Debug for Source<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source").field("path", &self.path).finish()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parent<'a> {
    None,
    Included {
        parent: NodeId<'a>,
    },
    Pasted {
        parent: NodeId<'a>,
        definition: Option<NodeId<'a>>,
    },
    Generated {
        parent: NodeId<'a>,
        definition: Option<NodeId<'a>>,
    },
}

impl<'a> Parent<'a> {
    pub fn parent(&self) -> Option<NodeId<'a>> {
        match self {
            Self::None => None,
            Self::Included { parent } => Some(parent),
            Self::Pasted { parent, .. } => Some(parent),
            Self::Generated { parent, .. } => Some(parent),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeInfo<'a> {
    pub span: Span,
    pub source: SourceId<'a>,
    pub parent: Parent<'a>,
}
impl<'a> NodeInfo<'a> {
    pub fn top(mut self: &'a Self) -> NodeId<'a> {
        
        while let Some(next) = self.parent.parent(){
            self = next;
        }
        self
    }
}

impl<'a> PartialEq for Source<'a> {
    fn eq(&self, other: &Self) -> bool {
        if std::ptr::eq(self, other) {
            return true;
        };
        self.path == other.path
    }
}

pub struct Context<'a> {
    bump: &'a Bump,
    supplier: SourceSupplier<'a>,
    source_map: HashMap<&'a str, SourceId<'a>>,
    log: Vec<LogEntry<'a>>,
    top_src: SourceId<'a>,
    top_src_eof: NodeId<'a>,
    config: AssemblerConfig,
}

#[derive(Debug, Default)]
pub struct Functioninfo {
    pub frame_size: usize,
}

impl<'a> Context<'a> {
    pub fn new(
        bump: &'a Bump,
        config: AssemblerConfig,
        source_supplier: impl Fn(&str, &Context<'a>) -> Result<&'a str, Box<dyn Error>> + 'a,
    ) -> Self {
        Self {
            bump,
            source_map: Default::default(),
            supplier: Box::new(source_supplier),
            log: Default::default(),
            top_src: &Source {
                path: "<INVALID>",
                contents: "<INVALID>",
            },
            top_src_eof: &const {
                NodeInfo {
                    span: Span::empty(),
                    source: &Source {
                        path: "<INVALID>",
                        contents: "<INVALID>",
                    },
                    parent: Parent::None,
                }
            },
            config,
        }
    }

    pub fn config(&self) -> &AssemblerConfig {
        &self.config
    }

    pub fn get_source_from_path(
        &mut self,
        path: impl Into<String>,
    ) -> Result<&'a Source<'a>, Box<dyn Error>> {
        let path = path.into();
        if let Some(id) = self.source_map.get(&path.as_str()) {
            return Ok(*id);
        }
        let path = self.bump.alloc_str(&path);
        let contents = (self.supplier)(path, self)?;
        let source = self.bump.alloc(Source { path, contents });
        self.source_map.insert(path, source);
        Ok(source)
    }

    pub fn merge_nodes(&self, left: NodeId<'a>, right: NodeId<'a>) -> NodeId<'a> {
        // TODO optimizing this might be something to do
        fn meow<'a>(thingies: &mut Vec<NodeId<'a>>, start: NodeId<'a>) {
            let mut nya = Some(start);
            while let Some(thing) = nya {
                thingies.push(thing);
                nya = thing.parent.parent();
            }
        }
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();
        meow(&mut lhs, left);
        meow(&mut rhs, right);

        for (lhs, rhs) in lhs.iter().rev().zip(rhs.iter().rev()) {
            if lhs != rhs {
                if lhs.source != rhs.source {
                    panic!("uhhhhhhh, {left:?}, {right:?}")
                } else {
                    return self.node(NodeInfo {
                        span: lhs.span.combine(rhs.span),
                        source: lhs.source,
                        parent: lhs.parent,
                        // included_by: None,
                        // invoked_by: lhs.invoked_by,
                    });
                }
            }
        }
        left
    }

    pub fn alloc_str(&self, data: impl AsRef<str>) -> &'a str {
        self.bump.alloc_str(data.as_ref())
    }

    pub fn alloc<T: Copy>(&self, data: T) -> &'a T {
        self.bump.alloc(data)
    }

    pub fn alloc_slice<T: Copy>(&self, data: &[T]) -> &'a [T] {
        self.bump.alloc_slice_copy(data)
    }

    pub fn eof(&self) -> NodeId<'a> {
        self.top_src_eof
    }

    pub fn node(&self, node: NodeInfo<'a>) -> NodeId<'a> {
        self.bump.alloc(node)
    }

    pub fn report(&mut self, error: LogEntry<'a>) {
        self.log.push(error);
    }

    pub fn report_error_locless(&mut self, msg: impl ToString) {
        self.log.push(LogEntry::default().error_locless(msg));
    }

    pub fn report_warning_locless(&mut self, msg: impl ToString) {
        self.log.push(LogEntry::default().warning_locless(msg));
    }

    pub fn report_info_locless(&mut self, msg: impl ToString) {
        self.log.push(LogEntry::default().info_locless(msg));
    }

    pub fn report_hint_locless(&mut self, msg: impl ToString) {
        self.log.push(LogEntry::default().hint_locless(msg));
    }

    pub fn report_error(&mut self, node: NodeId<'a>, error: impl ToString) {
        let error = LogEntry::new().error(node, error);
        self.log.push(error);
    }

    pub fn report_warning(&mut self, node: NodeId<'a>, error: impl ToString) {
        let error = LogEntry::new().warning(node, error);
        self.log.push(error);
    }

    pub fn report_info(&mut self, node: NodeId<'a>, error: impl ToString) {
        let error = LogEntry::new().info(node, error);
        self.log.push(error);
    }

    pub fn report_error_eof(&mut self, error: impl ToString) {
        let error = LogEntry::new().error(self.top_src_eof, error);
        self.log.push(error);
    }

    pub fn report_warning_eof(&mut self, error: impl ToString) {
        let error = LogEntry::new().warning(self.top_src_eof, error);
        self.log.push(error);
    }

    pub fn report_info_eof(&mut self, error: impl ToString) {
        let error = LogEntry::new().info(self.top_src_eof, error);
        self.log.push(error);
    }

    pub fn unexpected_token(
        &mut self,
        got: Option<Node<'a, Token<'a>>>,
        expected: impl Display,
        alternate: bool,
    ) -> NodeId<'a> {
        if alternate {
            match got {
                Some(Node(got, n)) => self.report_error(
                    n,
                    format!("unexpected token, got {got:#} expected {expected:#}"),
                ),
                None => self
                    .report_error_eof(format!("unexpected token, got eof expected {expected:#}")),
            }
        } else {
            match got {
                Some(Node(got, n)) => self.report_error(
                    n,
                    format!("unexpected token, got {got:#} expected {expected}"),
                ),
                None => {
                    self.report_error_eof(format!("unexpected token, got eof expected {expected}"))
                }
            }
        }

        match got {
            Some(Node(_, n)) => n,
            None => self.eof(),
        }
    }

    pub fn set_top_level_src(&mut self, src: SourceId<'a>) {
        self.top_src = src;
        self.top_src_eof = self.node(NodeInfo {
            span: src.eof(),
            source: src,
            parent: Parent::None,
        });
    }

    pub fn print_errors(&self) {
        for error in &*self.log {
            println!("{error}")
        }
    }

    pub fn take_logs(self) -> Vec<LogEntry<'a>> {
        self.log
    }

    pub fn has_errors(&self) -> bool {
        !self.log.is_empty()
    }
}
