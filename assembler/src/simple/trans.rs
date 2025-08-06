use std::num::NonZeroUsize;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SectionIdx(NonZeroUsize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct StrIdx(NonZeroUsize);
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolIdx(NonZeroUsize);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(NonZeroUsize);

#[derive(Clone, Copy)]
pub struct LineInfo {
    addr: usize,
    size: usize,

    line: u32,
    col: u32,
    offset: u32,
    file: StrIdx,
}

pub struct Reloc {}

pub struct TranslationUnit {}
