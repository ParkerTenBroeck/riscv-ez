use std::collections::HashMap;

pub struct TranslationUnit<'a, S, D> {
    pub sections: HashMap<&'a str, Section<'a, D>>,
    pub labels: HashMap<&'a str, Label<'a, S>>,
}

impl<'a, S, D> Default for TranslationUnit<'a, S, D> {
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
}

pub struct Section<'a, D> {
    pub name: &'a str,
    pub start: Option<u32>,
    pub size: u32,
    pub data: Vec<D>,
}
