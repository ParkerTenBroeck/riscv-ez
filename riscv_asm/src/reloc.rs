pub struct Reloc {}

pub enum Kind {
    PcRel,
}

pub enum Pattern {
    S12,
    I12,
    J20,
    W8,
    W16,
    W32,
}
