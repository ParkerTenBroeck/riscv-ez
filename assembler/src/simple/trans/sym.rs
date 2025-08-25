use std::{collections::HashMap, num::NonZeroUsize};

use crate::{
    logs::LogEntry,
    node::NodeOwned,
    simple::trans::{SectionIdx, str::StrIdx},
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolIdx(usize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum SymbolKind {
    #[default]
    Unresolved,
    Notype,
    Section,
    Object,
    File,
    Common,
    Function,
    Data,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum SymbolVisibility {
    #[default]
    Local,
    Global,
    Weak,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Symbol {
    name: StrIdx,
    pub section: Option<SectionIdx>,
    pub kind: SymbolKind,
    pub visibility: SymbolVisibility,
    pub size: u32,
    pub offset: u32,
}

impl Symbol {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            section: None,
            kind: Default::default(),
            visibility: Default::default(),
            size: Default::default(),
            offset: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Symbols {
    symbols: Vec<Symbol>,
    symbol_map: HashMap<StrIdx, SymbolIdx>,
}

pub enum SymbolError {
    SizePreviouslyDeclared(Option<NodeOwned>),
    KindPreviouslyDeclared(Option<NodeOwned>),
    VisibilityPreviouslyDeclared(Option<NodeOwned>),
    PreviouslyBound(Option<NodeOwned>),
}
impl SymbolError {
    pub fn to_log_entry(&self, label: &str, node: NodeOwned) -> LogEntry<NodeOwned> {
        let (log, node) = match self {
            SymbolError::SizePreviouslyDeclared(last_node) => (
                LogEntry::new().warning(node, "size previously defined"),
                last_node,
            ),
            SymbolError::KindPreviouslyDeclared(last_node) => (
                LogEntry::new().warning(node, "kind previously defined"),
                last_node,
            ),
            SymbolError::VisibilityPreviouslyDeclared(last_node) => (
                LogEntry::new().warning(node, "visibility previously defined"),
                last_node,
            ),
            SymbolError::PreviouslyBound(last_node) => (
                LogEntry::new().error(node, "symbol previously bound"),
                last_node,
            ),
        };
        if let Some(node) = node.clone() {
            log.info(node, "here")
        } else {
            log
        }
    }
}

impl Symbols {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            symbol_map: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, name: StrIdx) -> SymbolIdx {
        if let Some(symbol_idx) = self.symbol_map.get(&name).copied() {
            symbol_idx
        } else {
            let symbol_idx = SymbolIdx(self.symbols.len());
            self.symbols.push(Symbol::new(name));
            self.symbol_map.insert(name, symbol_idx);
            symbol_idx
        }
    }

    pub fn symbol(&mut self, symbol_idx: SymbolIdx) -> &mut Symbol {
        self.symbols.get_mut(symbol_idx.0).unwrap()
    }
}
