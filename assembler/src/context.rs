use std::{cell::RefCell, collections::HashMap, error::Error, num::NonZeroUsize};

use crate::{
    error::ErrorKind,
    lex::{Span, Spanned},
};

use super::error::FormattedError;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Node<T>(pub T, pub NodeId);

impl<T> Node<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Node<U> {
        Node(f(self.0), self.1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct NodeId(NonZeroUsize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct SourceId(NonZeroUsize);

pub type SourceSupplier = Box<dyn Fn(&str, &mut Context) -> Result<String, Box<dyn Error>>>;

pub struct SourceStorage {
    supplier: SourceSupplier,
    storage: RefCell<Vec<(String, String)>>,
    map: RefCell<HashMap<String, SourceId>>,
}

impl SourceStorage {
    pub fn get_from_path<'a>(
        &'a self,
        path: String,
        context: &mut Context,
    ) -> Result<(Source<'a>, SourceId), Box<dyn Error>> {
        let mut map = self.map.borrow_mut();
        if let Some(id) = map.get(&path) {
            Ok((self.get_from_id(*id), *id))
        } else {
            let mut s = self.storage.borrow_mut();
            let id = NonZeroUsize::new(s.len() + 1).map(SourceId).unwrap();
            s.push((path.clone(), (self.supplier)(&path, context)?));
            map.insert(path, id);
            drop(s);

            Ok((self.get_from_id(id), id))
        }
    }

    pub fn get_from_id<'a>(&'a self, id: SourceId) -> Source<'a> {
        let l = &self.storage.borrow()[id.0.get() - 1];
        unsafe {
            Source {
                path: std::mem::transmute::<&str, &str>(l.0.as_str()),
                contents: std::mem::transmute::<&str, &str>(l.1.as_str()),
            }
        }
    }

    pub fn new(supplier: impl Fn(&str, &mut Context) -> Result<String, Box<dyn Error>> + 'static) -> Self {
        Self {
            supplier: Box::new(supplier),
            storage: RefCell::default(),
            map: RefCell::default(),
        }
    }
}

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
            offset: self.contents.len() as u32 - 1,
            len: 1,
        }
    }
}

impl<'a> std::fmt::Debug for Source<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source").field("path", &self.path).finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NodeInfo {
    pub span: Span,
    pub source: SourceId,
    pub parent: Option<NodeId>,
}

pub struct Context<'a> {
    storage: &'a SourceStorage,
    nodes: Vec<NodeInfo>,
    errors: Vec<FormattedError<'a>>,
}

#[derive(Debug, Default)]
pub struct Functioninfo {
    pub frame_size: usize,
}

impl<'a> Context<'a> {
    pub fn new(storage: &'a mut SourceStorage) -> Self {
        Self {
            storage,
            nodes: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn get_source_from_path(&mut self, path: impl Into<String>) -> Result<(Source<'a>, SourceId), Box<dyn Error>> {
        self.storage.get_from_path(path.into(), self)
    }

    pub fn create_node<T>(
        &mut self,
        source: SourceId,
        spanned: Spanned<T>,
        parent: Option<NodeId>,
    ) -> Node<T> {
        Node(spanned.val, self.node(NodeInfo {
            span: spanned.span,
            source,
            parent,
        }))
    }

    fn node(&mut self, node: NodeInfo) -> NodeId{
        let node_id = NonZeroUsize::new(self.nodes.len() + 1).map(NodeId).unwrap();
        self.nodes.push(node);
        node_id
    }

    pub fn report(&mut self, error: impl FnOnce(&mut Context<'a>) -> FormattedError<'a>) {
        let error = error(self);
        self.errors.push(error);
    }

    pub fn report_error_hard(&mut self, msg: impl Into<String>) {
        self.errors
            .push(FormattedError::default().add_sourceless(ErrorKind::Error, msg.into()));
    }

    pub fn report_error(&mut self, error: Node<impl ToString>) {
        let error = FormattedError::new(self, error.1, ErrorKind::Error, error.0.to_string());
        self.errors.push(error);
    }

    pub fn report_warning(&mut self, error: Node<impl ToString>) {
        let error = FormattedError::new(self, error.1, ErrorKind::Warning, error.0.to_string());
        self.errors.push(error);
    }

    pub fn report_info(&mut self, error: Node<impl ToString>) {
        let error = FormattedError::new(self, error.1, ErrorKind::Info, error.0.to_string());
        self.errors.push(error);
    }

    pub fn get_node(&self, arg: NodeId) -> NodeInfo {
        self.nodes[arg.0.get() - 1]
    }

    pub fn get_source_from_id(&self, source: SourceId) -> Source<'a> {
        self.storage.get_from_id(source)
    }

    pub fn print_errors(&self) {
        for error in &self.errors {
            println!("{error}")
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    pub fn parent_child<T>(&mut self, parent: NodeId, child: Node<T>) -> Node<T> {
        let mut node = self.get_node(child.1);
        node.parent = Some(parent);
        Node(child.0, self.node(node))
    }
}
