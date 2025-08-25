use std::{
    collections::{HashMap, hash_map::Entry},
    default,
};

use crate::{
    node::NodeOwned,
    simple::trans::{reloc::RelocIdx, sym::SymbolIdx},
};

#[derive(Clone, Debug)]
pub struct DataDbg {
    range: std::ops::Range<usize>,
    node: NodeOwned,
}

#[derive(Default, Clone, Debug)]
pub struct SymbolDbg {
    pub definition: Option<NodeOwned>,
    pub visibility: Option<NodeOwned>,
    pub kind: Option<NodeOwned>,
    pub size: Option<NodeOwned>,
}

#[derive(Clone, Debug, Default)]
pub struct DebugInfo {
    data: Vec<DataDbg>,
    relocs: HashMap<RelocIdx, NodeOwned>,
    symbols: HashMap<SymbolIdx, SymbolDbg>,
}

impl DebugInfo {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relocs: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    pub(super) fn emit_reloc_dbg(&mut self, reloc_idx: RelocIdx, node: NodeOwned) {
        self.relocs.insert(reloc_idx, node);
    }

    pub(super) fn symbol_dbg_entry(
        &mut self,
        symbol_idx: SymbolIdx,
    ) -> Entry<'_, SymbolIdx, SymbolDbg> {
        self.symbols.entry(symbol_idx)
    }

    /// range must start at greater address than all ranges currently present
    pub(super) fn emit_data_dbg(&mut self, range: std::ops::Range<usize>, node: NodeOwned) {
        if let Some(last) = self.data.last_mut()
            && last.range.end == range.start
            && last.node == node
        {
            last.range.end = range.end;
        } else {
            self.data.push(DataDbg { range, node })
        }
    }
}
