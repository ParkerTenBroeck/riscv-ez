pub struct Reloc {}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(usize);

pub struct Relocations {
    relocs: Vec<Reloc>,
}
impl Relocations {
    pub fn new() -> Self {
        Self { relocs: Vec::new() }
    }
}
