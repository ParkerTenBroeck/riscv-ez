use crate::NodeOwned;
use std::{collections::HashMap, num::NonZeroUsize};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SectionIdx(NonZeroUsize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct StrIdx(NonZeroUsize);
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolIdx(NonZeroUsize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(NonZeroUsize);

#[derive(Clone, Copy)]
pub struct DataSpan {
    start: usize,
    size: usize,
}

pub struct SourceInfo<T>(T, NodeOwned);

pub struct Reloc {}

pub struct TranslationUnit {
    sections: HashMap<String, Section>,
}

pub struct Section {
    data: Data,
    relocations: Relocations,
    symbols: Symbols,
    debug_info: DebugInfo,
    str_table: StringTable,
}

pub struct Data {
    data: Vec<u8>,
}

pub struct Relocations {
    relocs: Vec<Reloc>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SymbolKind {
    Undefined,
    Section,
    Object,
    File,
    Common,
    Function,
    Data,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SymbolVisibility {
    Local,
    Global,
    Weak,
}

pub struct Symbol {
    name: StrIdx,
    kind: SymbolKind,
    visibility: SymbolVisibility,
    size: u32,
    offset: u32,
}

pub struct Symbols {
    symbols: Vec<Symbol>,
}

pub struct DebugInfo {
    data: Vec<SourceInfo<DataSpan>>,
    relocs: Vec<SourceInfo<RelocIdx>>,
    symbols: Vec<SourceInfo<SymbolIdx>>,
}

pub struct StringTable {
    data: String,
    map: HashMap<String, StrIdx>,
}
