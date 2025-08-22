use crate::{
    node::NodeOwned,
    simple::trans::{reloc::RelocIdx, sym::SymbolIdx},
};

#[derive(Clone, Copy)]
pub struct DataSpan {
    start: usize,
    size: usize,
}

pub struct SourceInfo<T>(T, NodeOwned);

pub struct DataDbg{
    range: std::ops::Range<usize>,
    pub source: NodeOwned
}

pub struct RelocDbg{
    reloc_idx: RelocIdx,
    pub definition: NodeOwned,
}

pub struct SymbolDbg{
    symbol_idx: SymbolIdx,
    pub definition: Option<NodeOwned>,
    pub visibility: Option<NodeOwned>,
    pub kind: Option<NodeOwned>,
}

pub struct DebugInfo {
    data: Vec<SourceInfo<DataSpan>>,
    relocs: Vec<SourceInfo<RelocIdx>>,
    symbols: Vec<SymbolDbg>,
}

impl DebugInfo {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relocs: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn emit_reloc_dbg(&mut self){
        // self.relocs.binary_search(x)
        todo!()
    }
}
