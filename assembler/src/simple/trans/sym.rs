use std::{collections::HashMap, num::NonZeroUsize};

use crate::simple::trans::str::StrIdx;

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
    pub kind: SymbolKind,
    pub visibility: SymbolVisibility,
    pub size: u32,
    pub offset: u32,
}

impl Symbol {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            kind: Default::default(),
            visibility: Default::default(),
            size: Default::default(),
            offset: Default::default(),
        }
    }
}

pub struct Symbols {
    symbols: Vec<Symbol>,
}

impl Symbols {
    pub fn insert(&mut self, sym: Symbol) -> SymbolIdx {
        self.symbols.push(sym);
        SymbolIdx(self.symbols.len() - 1)
    }

    pub fn get_mut(&mut self, idx: SymbolIdx) -> Option<&mut Symbol> {
        self.symbols.get_mut(idx.0)
    }

    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }
}
