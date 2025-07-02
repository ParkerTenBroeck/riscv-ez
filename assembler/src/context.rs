use crate::{
    error::ErrorKind,
    lex::{Span, Spanned},
};
use bumpalo::Bump;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap, error::Error, num::NonZeroUsize};
use std::cell::Cell;
use super::error::FormattedError;

#[derive(Clone, Copy, Debug)]
pub struct Node<'a, T>(pub T, pub NodeId<'a>);

impl<'a, T> Node<'a, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Node<'a, U> {
        Node(f(self.0), self.1)
    }
}

pub type NodeId<'a> = &'a NodeInfo<'a>;
pub type SourceId<'a> = &'a Source<'a>;

pub type SourceSupplier = Rc<dyn Fn(&str, &Context) -> Result<String, Box<dyn Error>>>;

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
pub struct NodeInfo<'a> {
    pub span: Span,
    pub source: SourceId<'a>,
    pub included_by: Option<NodeId<'a>>,
    pub invoked_by: Option<NodeId<'a>>,
}

impl<'a> PartialEq for Source<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self as *const Self as usize == other as *const Self as usize {
            return true;
        };
        self.path == other.path
    }
}

impl<'a> PartialEq for NodeInfo<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self as *const Self as usize == other as *const Self as usize {
            return true;
        };
        self.span == other.span
            && self.source == other.source
            && self.invoked_by == other.invoked_by
            && self.included_by == other.included_by
    }
}

pub struct Context<'a> {
    bump: &'a Bump,
    supplier: SourceSupplier,
    source_map: RefCell<HashMap<&'a str, SourceId<'a>>>,
    errors: RefCell<Vec<FormattedError<'a>>>,
    top_src: Cell<SourceId<'a>>,
    top_src_eof: Cell<NodeId<'a>>,
}

#[derive(Debug, Default)]
pub struct Functioninfo {
    pub frame_size: usize,
}

impl<'a> Context<'a> {
    pub fn new(
        bump: &'a Bump,
        source_supplier: impl Fn(&str, &Context) -> Result<String, Box<dyn Error>> + 'static,
    ) -> Self {
        Self {
            bump,
            source_map: Default::default(),
            supplier: Rc::new(source_supplier),
            errors: Default::default(),
            top_src: Cell::new(&Source {
                path: "<INVALID>",
                contents: "<INVALID>",
            }),
            top_src_eof: Cell::new(&const {
                NodeInfo {
                    span: Span::empty(),
                    source: &Source {
                        path: "<INVALID>",
                        contents: "<INVALID>",
                    },
                    included_by: None,
                    invoked_by: None,
                }
            }),
        }
    }

    pub fn get_source_from_path(
        &self,
        path: impl Into<String>,
    ) -> Result<&'a Source<'a>, Box<dyn Error>> {
        let path = path.into();
        if let Some(id) = self.source_map.borrow().get(&path.as_str()) {
            return Ok(*id);
        }
        let path = self.bump.alloc_str(&path);
        let contents = self.supplier.clone()(&path, self)?;
        let contents = self.bump.alloc_str(contents.as_str());
        let source = self.bump.alloc(Source { path, contents });
        let mut map = self.source_map.borrow_mut();
        map.insert(path, source);
        drop(map);
        Ok(source)
    }

    pub fn merge_nodes(&self, left: NodeId<'a>, right: NodeId<'a>) -> NodeId<'a> {
        // TODO optimizing this might be something to do
        fn meow<'a>(thingies: &mut Vec<NodeId<'a>>, start: NodeId<'a>) {
            let mut nya = Some(start);
            while let Some(thing) = nya {
                thingies.push(thing);
                nya = thing.invoked_by;
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
                        included_by: None,
                        invoked_by: lhs.invoked_by,
                    });
                }
            }
        }
        left
    }

    pub fn alloc<T: Copy>(&self, data: T) -> &'a T {
        self.bump.alloc(data)
    }

    pub fn alloc_slice<T: Copy>(&self, data: &[T]) -> &'a [T] {
        self.bump.alloc_slice_copy(data)
    }

    pub fn top_src_eof(&self) -> NodeId<'a> {
        self.top_src_eof.get()
    }

    pub fn node(&self, node: NodeInfo<'a>) -> NodeId<'a> {
        self.bump.alloc(node)
    }

    pub fn report(&self, error: impl FnOnce(&Context<'a>) -> FormattedError<'a>) {
        let error = error(self);
        self.errors.borrow_mut().push(error);
    }

    pub fn report_error_nodeless(&self, msg: impl Into<String>) {
        self.errors
            .borrow_mut()
            .push(FormattedError::default().add_sourceless(ErrorKind::Error, msg.into()));
    }

    pub fn report_error(&self, node: NodeId<'a>, error: impl ToString) {
        let error = FormattedError::new(self, node, ErrorKind::Error, error.to_string());
        self.errors.borrow_mut().push(error);
    }

    pub fn report_warning(&self, node: NodeId<'a>, error: impl ToString) {
        let error = FormattedError::new(self, node, ErrorKind::Warning, error.to_string());
        self.errors.borrow_mut().push(error);
    }

    pub fn report_info(&self, node: NodeId<'a>, error: impl ToString) {
        let error = FormattedError::new(self, node, ErrorKind::Info, error.to_string());
        self.errors.borrow_mut().push(error);
    }

    pub fn report_error_eof(&self, error: impl ToString) {
        let error =
            FormattedError::new(self, self.top_src_eof.get(), ErrorKind::Error, error.to_string());
        self.errors.borrow_mut().push(error);
    }

    pub fn report_warning_eof(&self, error: impl ToString) {
        let error = FormattedError::new(
            self,
            self.top_src_eof.get(),
            ErrorKind::Warning,
            error.to_string(),
        );
        self.errors.borrow_mut().push(error);
    }

    pub fn report_info_eof(&self, error: impl ToString) {
        let error = FormattedError::new(self, self.top_src_eof.get(), ErrorKind::Info, error.to_string());
        self.errors.borrow_mut().push(error);
    }

    pub fn set_top_level_src(&self, src: SourceId<'a>) {
        self.top_src.set(src);
        self.top_src_eof.set(self.node(NodeInfo {
            span: src.eof(),
            source: src,
            included_by: None,
            invoked_by: None,
        }))
    }

    pub fn print_errors(&self) {
        for error in &*self.errors.borrow() {
            println!("{error}")
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }
}
