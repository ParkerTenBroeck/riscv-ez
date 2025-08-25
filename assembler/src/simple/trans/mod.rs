use num_traits::PrimInt;
use std::{
    collections::{HashMap, hash_map::Entry},
    num::NonZeroUsize,
};

pub mod data;
pub mod dbg;
pub mod reloc;
pub mod section;
pub mod str;
pub mod sym;

use dbg::*;
use reloc::*;
use section::*;
use str::*;
use sym::*;

use crate::node::NodeOwned;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SectionIdx(NonZeroUsize);

pub trait TranslationUnitMachine {
    type Reloc: Reloc;
    type PtrSizeType: PrimInt + std::fmt::Debug;
}

pub struct TranslationUnit<T: TranslationUnitMachine> {
    sections: Vec<Section<T>>,
    section_map: HashMap<StrIdx, SectionIdx>,
    symbols: Symbols<T::PtrSizeType>,
    str_table: StringTable,
}

impl<T: TranslationUnitMachine> Clone for TranslationUnit<T> {
    fn clone(&self) -> Self {
        let TranslationUnit {
            sections,
            section_map,
            symbols,
            str_table,
        } = self;
        TranslationUnit {
            sections: sections.clone(),
            section_map: section_map.clone(),
            symbols: symbols.clone(),
            str_table: str_table.clone(),
        }
    }
}

impl<T: TranslationUnitMachine> core::fmt::Debug for TranslationUnit<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let TranslationUnit {
                sections,
                section_map,
                symbols,
                str_table,
            } = self;
        f
        .debug_struct("TranslationUnit")
        .field("sections", &sections)
        .field("section_map", &section_map)
        .field("symbols", &symbols)
        .field("str_table", &str_table)
        .finish()
    }
}

impl<T: TranslationUnitMachine> Default for TranslationUnit<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: TranslationUnitMachine> TranslationUnit<T> {
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

    pub fn get_mut(&mut self, name: &str) -> SectionMut<'_, T> {
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

pub struct SectionMut<'a, T: TranslationUnitMachine> {
    section: &'a mut Section<T>,
    section_idx: SectionIdx,
    symbols: &'a mut Symbols<T::PtrSizeType>,
    str_table: &'a mut StringTable,
}

impl<'a, T: TranslationUnitMachine> SectionMut<'a, T> {
    fn sym_checked(
        &mut self,
        name: &str,
        node: Option<NodeOwned>,
        node_kind: impl Fn(&mut SymbolDbg) -> &mut Option<NodeOwned>,
        err: impl FnOnce(Option<NodeOwned>) -> SymbolError,
    ) -> Result<&mut Symbol<T::PtrSizeType>, SymbolError> {
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
        size: T::PtrSizeType,
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

    pub fn reloc(&mut self, reloc: T::Reloc, node: Option<NodeOwned>) -> RelocIdx {
        let reloc = self.section.relocations.emit(reloc);
        if let Some(node) = node {
            self.section.debug_info.emit_reloc_dbg(reloc, node)
        }
        reloc
    }

    pub fn data(&mut self, data: &[u8], align: T::PtrSizeType, node: Option<NodeOwned>) {
        let range = self.section.data.push_data(data, align);
        if let Some(node) = node {
            self.section.debug_info.emit_data_dbg(range, node)
        }
    }

    pub fn space(&mut self, space: T::PtrSizeType, align: T::PtrSizeType, node: Option<NodeOwned>) {
        let range = self.section.data.push_space(space, align);
        if let Some(node) = node {
            self.section.debug_info.emit_data_dbg(range, node)
        }
    }

    pub fn emit_comment_dbg(&mut self, _comment: &str, _node: NodeOwned) {
        // self.section.debug_info.emit_comment_dbg()
    }
}
