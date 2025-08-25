use std::{
    collections::{HashMap, hash_map::Entry},
    num::NonZeroUsize,
    ops::Index,
    sync::Arc,
};

pub mod data;
pub mod dbg;
pub mod reloc;
pub mod section;
pub mod str;
pub mod sym;

use data::*;
use dbg::*;
use reloc::*;
use section::*;
use str::*;
use sym::*;

use crate::node::NodeOwned;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SectionIdx(NonZeroUsize);

#[derive(Clone)]
pub struct TranslationUnit {
    sections: Vec<Section>,
    section_map: HashMap<StrIdx, SectionIdx>,
    symbols: Symbols,
    str_table: StringTable,
}

impl Default for TranslationUnit {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for TranslationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl TranslationUnit {
    pub fn new() -> Self {
        Self {
            sections: Vec::new(),
            section_map: HashMap::new(),
            symbols: Symbols::new(),
            str_table: StringTable::new(),
        }
    }

    pub fn reset_locals(&mut self) {}

    pub fn resolve_or_make(&mut self, name: &str) -> SectionIdx {
        let name = self.str_table.resolve(name);
        if let Some(idx) = self.section_map.get(&name) {
            return *idx;
        }
        self.sections.push(Section::new(name));
        let idx = SectionIdx(NonZeroUsize::new(self.sections.len()).unwrap());
        self.section_map.insert(name, idx);
        idx
    }

    pub fn get_mut(&mut self, name: &str) -> SectionMut<'_> {
        let section_idx = self.resolve_or_make(name);
        let section = &mut self.sections[section_idx.0.get() - 1];
        SectionMut {
            section,
            section_idx,
            symbols: &mut self.symbols,
            str_table: &mut self.str_table,
        }
    }
}

pub struct SectionMut<'a> {
    section: &'a mut Section,
    section_idx: SectionIdx,
    symbols: &'a mut Symbols,
    str_table: &'a mut StringTable,
}

impl<'a> SectionMut<'a> {
    fn sym_checked(
        &mut self,
        name: &str,
        node: Option<NodeOwned>,
        node_kind: impl Fn(&mut SymbolDbg) -> &mut Option<NodeOwned>,
        err: impl FnOnce(Option<NodeOwned>) -> SymbolError,
    ) -> Result<&mut Symbol, SymbolError> {
        let symbol_idx = self.symbols.resolve(self.str_table.resolve(name));
        let symbol = self.symbols.symbol(symbol_idx);

        let mut dbg = self.section.debug_info.symbol_dbg_entry(symbol_idx);
        if let Entry::Occupied(entry) = &mut dbg
            && let Some(declaration) = node_kind(entry.get_mut())
        {
            return Err(err(Some(declaration.clone())));
        }
        if let Some(node) = node {
            *node_kind(dbg.or_default()) = Some(node);
        }
        Ok(symbol)
    }

    pub fn symbol(&mut self, name: &str) -> SymbolIdx {
        self.symbols.resolve(self.str_table.resolve(name))
    }

    pub fn set_symbol_kind(
        &mut self,
        name: &str,
        kind: SymbolKind,
        node: Option<NodeOwned>,
    ) -> Result<(), SymbolError> {
        let symbol = self.sym_checked(
            name,
            node,
            |sym| &mut sym.kind,
            SymbolError::KindPreviouslyDeclared,
        )?;
        symbol.kind = kind;
        Ok(())
    }

    pub fn set_symbol_size(
        &mut self,
        name: &str,
        size: u32,
        node: Option<NodeOwned>,
    ) -> Result<(), SymbolError> {
        let symbol = self.sym_checked(
            name,
            node,
            |sym| &mut sym.size,
            SymbolError::SizePreviouslyDeclared,
        )?;
        symbol.size = size;
        Ok(())
    }

    pub fn set_symbol_visibility(
        &mut self,
        name: &str,
        visibility: SymbolVisibility,
        node: Option<NodeOwned>,
    ) -> Result<(), SymbolError> {
        let symbol = self.sym_checked(
            name,
            node,
            |sym| &mut sym.visibility,
            SymbolError::VisibilityPreviouslyDeclared,
        )?;
        symbol.visibility = visibility;
        Ok(())
    }

    pub fn bind_symbol(&mut self, name: &str, node: Option<NodeOwned>) -> Result<(), SymbolError> {
        let section_idx = self.section_idx;
        let current_offset = self.section.data.current_offset();
        let symbol = self.sym_checked(
            name,
            node,
            |sym| &mut sym.definition,
            SymbolError::PreviouslyBound,
        )?;
        if symbol.section.is_some() {
            return Err(SymbolError::PreviouslyBound(None));
        }
        symbol.section = Some(section_idx);
        symbol.offset = current_offset;
        Ok(())
    }

    pub fn reloc(&mut self) {}

    pub fn data(&mut self, data: &[u8], align: usize, node: Option<NodeOwned>) {
        let range = self.section.data.push_data(data, align);
        if let Some(node) = node {
            self.section.debug_info.emit_data_dbg(range, node)
        }
    }

    pub fn space(&mut self, space: usize, align: usize, node: Option<NodeOwned>) {
        let range = self.section.data.push_space(space, align);
        if let Some(node) = node {
            self.section.debug_info.emit_data_dbg(range, node)
        }
    }
}
