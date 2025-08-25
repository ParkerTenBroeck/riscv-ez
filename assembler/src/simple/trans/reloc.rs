pub trait Reloc: Clone + std::fmt::Debug {}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(usize);

#[derive(Clone, Debug)]
pub struct Relocations<T: Reloc> {
    relocs: Vec<T>,
}

impl<T: Reloc> Default for Relocations<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Reloc> Relocations<T> {
    pub fn new() -> Self {
        Self { relocs: Vec::new() }
    }
}
