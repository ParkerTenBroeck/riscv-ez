use crate::simple::trans::{
    data::Data,
    dbg::DebugInfo,
    reloc::Relocations,
    str::{StrIdx, StringTable},
    sym::Symbols,
};

#[derive(Clone)]
pub struct Section {
    name: StrIdx,
    pub(crate) data: Data,
    pub(crate) relocations: Relocations,
    pub(crate) debug_info: DebugInfo,
}

impl Section {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            data: Data::new(),
            relocations: Relocations::new(),
            debug_info: DebugInfo::new(),
        }
    }
}
