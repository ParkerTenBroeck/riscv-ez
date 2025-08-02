pub trait Linker {}

pub struct TranslationUnit<L: Linker> {
    linker: L,
}

pub struct Section {}

pub struct Relocation {}

pub struct Symbol {}
