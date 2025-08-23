use std::{collections::HashMap, num::NonZeroUsize, ops::Index};

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

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SectionIdx(NonZeroUsize);

pub struct TranslationUnit {
    sections: Vec<Section>,
    section_map: HashMap<StrIdx, SectionIdx>,
    symbol_map: HashMap<StrIdx, SymbolIdx>,
    str_table: StringTable,
}

impl TranslationUnit {
    pub fn new() -> Self {
        Self {
            sections: Vec::new(),
            section_map: HashMap::new(),
            symbol_map: HashMap::new(),
            str_table: StringTable::new(),
        }
    }

    // pub fn resolve_or_make(&mut self, name: &str) -> SectionIdx {
    //     let name = self.str_table.idx(name);
    //     if let Some(idx) = self.section_map.get(&name) {
    //         return *idx;
    //     }
    //     let idx = SectionIdx(self.sections.len());
    //     self.sections.push(Section::new(name));
    //     self.section_map.insert(name, idx);
    //     idx
    // }

    // pub fn get_mut(&mut self, name: &str) -> SectionMut<'_> {
    //     let section_idx = self.resolve_or_make(name);
    //     let section = &mut self.sections[section_idx.0];
    //     SectionMut {
    //         section,
    //         section_idx,
    //         symbol_map: &mut self.symbol_map,
    //         str_table: &mut self.str_table,
    //     }
    // }
}

pub struct SectionMut<'a> {
    section: &'a mut Section,
    section_idx: SectionIdx,
    symbol_map: &'a mut HashMap<StrIdx, (SectionIdx, SymbolIdx)>,
    str_table: &'a mut StringTable,
}

pub enum SymbolError{
    SymbolDefinedInOtherSection,
}

impl<'a> SectionMut<'a>{
    pub fn symbol(&mut self, name: &str) -> Result<&mut Symbol, SymbolError>{
        let name = self.str_table.idx(name);
        if let Some((section_idx, symbol_idx)) = self.symbol_map.get(&name){
            if *section_idx != self.section_idx{
                return Err(SymbolError::SymbolDefinedInOtherSection)
            }
            Ok(self.section.symbols.get_mut(*symbol_idx).unwrap())
        }else{
            let symbol_idx = self.section.symbols.insert(Symbol::new(name));
            self.symbol_map.insert(name, (self.section_idx, symbol_idx));
            Ok(self.section.symbols.get_mut(symbol_idx).unwrap())
        }
    }

    // pub fn set_symbol_kind(&mut self, name: &str)
}