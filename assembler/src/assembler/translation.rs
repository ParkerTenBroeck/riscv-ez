use std::collections::HashMap;

pub struct TranslationUnit<'a, S> {
    pub sections: HashMap<&'a str, Section<'a>>,
    pub labels: HashMap<&'a str, Label<'a, S>>,
}

impl<'a, S> Default for TranslationUnit<'a, S> {
    fn default() -> Self {
        Self {
            sections: Default::default(),
            labels: Default::default(),
        }
    }
}

pub struct Label<'a, S> {
    pub source: S,
    pub section: &'a str,
    pub offset: u32,
    pub size: u32,
    pub align: u32,
}

pub enum CalculationKind {
    Size,
    Align,
    PcRel,
    Absolute,
}

pub enum FormKind {
    Full,
}

pub struct Relocation<'a> {
    pub label: &'a str,
    pub value_offset: i32,
    pub form: FormKind,
    pub calc: CalculationKind,
    pub section_offset: u32,
}

pub struct Section<'a> {
    pub name: &'a str,
    pub start: Option<u32>,
    pub data: Vec<u8>,
    pub align: u32,
    pub relocs: Vec<Relocation<'a>>,
}
